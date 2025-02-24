import calico.IOWebApp
import calico.html.HtmlAttr
import calico.html.io.*
import calico.html.io.given
import cats.data.OptionT
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlDivElement
import fs2.dom.HtmlElement
import fs2.dom.Window
import monocle.Focus
import monocle.Lens
import smithy.api.Http
import smithy.api.HttpHeader
import smithy.api.HttpLabel
import smithy.api.NonEmptyString
import smithy4s.Blob
import smithy4s.Hints
import smithy4s.ShapeId
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.dynamic.model.Model
import smithy4s.json.Json
import smithy4s.schema.Schema
import util.chaining.*

object App extends IOWebApp {

  def render: Resource[IO, HtmlElement[IO]] = Window[IO]
    .location
    .search
    .get
    .toResource
    .flatMap {
      case query if query.contains("remote") => Dumper.liftToSig(Dumper.remote)
      case _                                 => Dumper.inBrowser
    }
    .flatMap { dumperSig =>
      div(
        Dumper.progressBar(dumperSig),
        renderMain(
          using dumperSig.map(_.toOption)
        ),
      )
    }

  def renderMain(
    using dumperSig: DumperOptionSig
  ): Resource[IO, HtmlElement[IO]] = div(
    h1("Smithy Transcoder"),
    SampleComponent.make(
      "Struct",
      Schema
        .tuple(Schema.string, Schema.int)
        .withId("demo", "Struct")
        .toHttpInputSchema,
      initInput = """{"_1": "foo", "_2": 42}""",
      initModel =
        """$version: "2"
          |
          |namespace demo
          |
          |structure Struct {
          |  @required _1: String
          |  @required _2: Integer
          |}
          |""".stripMargin,
    ),
    SampleComponent.make(
      "HTTP input",
      Schema
        .struct[(String, String, String)](
          Schema.string.required[(String, String, String)]("id", _._1).addHints(HttpLabel()),
          Schema
            .string
            .required[(String, String, String)]("name", _._2)
            .addHints(HttpHeader("x-name")),
          Schema.string.required[(String, String, String)]("details", _._3),
        )(Tuple3.apply)
        .addHints(Http(NonEmptyString("PUT"), NonEmptyString("/data/{id}"))),
      initInput = """{"id": "foo", "name": "bar", "details": "baz"}""",
      initModel =
        """$version: "2"
          |namespace demo
          |
          |@alloy#simpleRestJson
          |service MyService {
          |  operations: [MyOp]
          |}
          |
          |@http(method: "PUT", uri: "/data/{id}")
          |operation MyOp {
          |  input := {
          |    @required @httpLabel id: String
          |    @required @httpHeader("x-name") name: String
          |    @required details: String
          |  }
          |}
      """.stripMargin,
    ),
    SampleComponent.make(
      "String",
      Schema
        .string
        .toHttpInputSchema,
      initInput = """"foo"""",
      initModel =
        """$version: "2"
          |
          |namespace demo
          |
          |string String
          |""".stripMargin,
    ),
    SampleComponent.make(
      "Union",
      Schema
        .either(Schema.string, Schema.int)
        .withId("demo", "Union")
        .toHttpInputSchema,
      initInput = """{"left": "hello"}""",
      initModel =
        """$version: "2"
          |
          |namespace demo
          |
          |union Union {
          |  left: String
          |  right: Integer
          |}
          |""".stripMargin,
    ),
    SampleComponent.make(
      "Document",
      Schema
        .document
        .toHttpInputSchema,
      initInput = """{"foo": "bar"}""",
      initModel =
        """$version: "2"
          |
          |namespace demo
          |
          |document Document
          |""".stripMargin,
    ),
  )

}

private val defaultHttpHint = Http(NonEmptyString("POST"), NonEmptyString("/"))

object SampleComponent {
  import monocle.syntax.all.*

  extension [A](sigref: SignallingRef[IO, A]) {

    inline def lens[B](inline f: Focus.KeywordContext ?=> A => B): SignallingRef[IO, B] =
      SignallingRef.lens(sigref)(
        _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].get,
        _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].replace,
      )

    inline def sig: Signal[IO, A] = sigref
  }

  def make(
    sampleLabel: String,
    initSchema: Schema[?],
    initModel: String,
    initInput: String,
  )(
    using DumperOptionSig
  ): Resource[IO, HtmlElement[IO]] = {

    /** The read format is the one used for decoding the input, the write format is the one used for
      * updating it. The reason why there's two of them: the write format is controlled directly by
      * the user, and there's a background fiber that updates the input and sets the new read
      * format. Basically, "currentInput" must've been written by either the write format, or the
      * user's input.
      *
      * This prevents any strange behavior that could arise from missing a format update event (as
      * signal.discrete can drop them), causing inconsistent state and strange concurrency bugs.
      *
      * Credit to Arman Bilge for suggesting this approach on Discord!
      */
    case class State(
      currentIDL: String,
      currentInput: String,
      readFormatKind: FormatKind,
      writeFormatKind: FormatKind,
      jsonExplicitDefaults: Boolean,
    )

    object State {
      val init: State = {
        val initFmt = FormatKind.JSON

        new State(
          currentIDL = initModel,
          currentInput = initInput,
          readFormatKind = initFmt,
          writeFormatKind = initFmt,
          jsonExplicitDefaults = false,
        )
      }
    }
    case class ValueWithSchema[A](a: A, s: Schema[A])

    for {
      state <- SignallingRef[IO].of(State.init).toResource

      dumperOption = summon[DumperOptionSig]

      currentIDL = state.lens(_.currentIDL)
      currentInput = state.lens(_.currentInput)
      jsonExplicitDefaults = state.lens(_.jsonExplicitDefaults)

      readFormatKind = state.lens(_.readFormatKind)
      readFormat = (readFormatKind.sig, jsonExplicitDefaults.sig).mapN(_.toFormat(_))

      writeFormatKind = state.lens(_.writeFormatKind)
      writeFormat = (writeFormatKind.sig, jsonExplicitDefaults.sig).mapN(_.toFormat(_))

      schema <- (currentIDL.changes, dumperOption)
        .tupled
        .discrete
        .switchMap {
          case (idl, Some(dumper)) =>
            fs2
              .Stream
              .eval(
                buildNewSchema(idl, dumper).attempt
              )

          case _ => fs2.Stream.empty
        }
        // for offline / jvmless mode, general "instantness"
        .holdResource(initSchema.asRight)

      currentValueSignal <-
        (currentInput, readFormat, schema)
          .mapN {
            case (input, format, Right(schema: Schema[?])) =>
              format
                .decode(input)(
                  using schema
                )
                .map(_.tupleRight(schema).map(ValueWithSchema.apply))

            case _ => IO.pure("Invalid schema".asLeft)
          }
          .discrete
          .switchMap(fs2.Stream.eval)
          .hold1Resource

      _ <-
        writeFormat
          .changes
          .discrete
          .evalMap { fmt =>
            // we only want to run this when the format changes, and not when the current value changes...
            // so we read the CV manually with .get rather than zipping the signals.
            currentValueSignal.get.flatMap {
              case Right(vs) =>
                fmt
                  .encode(vs.a)(
                    using vs.s
                  )
                  .flatMap { encoded =>
                    // these could in theory happen separately,
                    // but it might cause glitches in rendering due to how `currentValue` gets computed.
                    state.update(s =>
                      s.copy(
                        currentInput = encoded,
                        readFormatKind = fmt.kind,
                      )
                    )
                  }
              case _ => IO.unit
            }
          }
          .compile
          .drain
          .background

      modelErrors = schema.map(_.swap.toOption.map(_.getMessage))

      modelSourceBlock = div(
        styleAttr := "display: flex; flex-direction:column; flex: 2; overflow: auto",
        div(
          button(
            "Format code",
            disabled <-- (dumperOption.map(_.isEmpty), modelErrors.map(_.isDefined)).mapN(_ || _),
            onClick --> {
              _.switchMap { _ =>
                (currentIDL, dumperOption)
                  .tupled
                  .get
                  .flatMap {
                    case (text, Some(dumper)) =>
                      dumper
                        .format(text)
                        .flatMap(currentIDL.set)
                        // we don't need to update anything or show errors here
                        // because if your formatting fails, you'll see it in the model errors.
                        // still, to give users a way to report if this still fails, we'll log it.
                        .handleErrorWith(IO.consoleForIO.printStackTrace)
                    case _ => IO.unit
                  }
                  .pipe(fs2.Stream.exec)
              }
            },
          )
        ),
        textArea.withSelf { self =>
          (
            styleAttr := "flex: 1; min-height: 150px;",
            disabled <-- dumperOption.map(_.isEmpty),
            onInput(
              self.value.get.flatMap(currentIDL.set)
            ),
            value <-- currentIDL,
          )
        },
        div(
          pre(
            styleAttr := "text-wrap: wrap",
            code(
              styleAttr := "color: #aa0000",
              modelErrors,
            ),
          )
        ),
      )

      inputErrors = currentValueSignal.map(_.swap.toOption)

      inputView = div(
        styleAttr := """display: flex; flex: 3""".stripMargin,
        div(
          styleAttr := "flex: 1",
          form(
            FormatKind
              .values
              .toList
              .map { fmt =>
                label(
                  styleAttr := "display:block",
                  fmt.name,
                  input.withSelf { self =>
                    (
                      `type` := "radio",
                      nameAttr := "format",
                      value := fmt.name,
                      checked <-- writeFormatKind.map(_ === fmt),
                      onInput(
                        self
                          .value
                          .get
                          .map(FormatKind.valueOf)
                          .flatMap(writeFormatKind.set)
                      ),
                      disabled <-- inputErrors.map(_.isDefined),
                    )
                  },
                  // format-specific config - should be generalized when there are more options like this.
                  // these vars should modify their own signals only, same with the format change, but the actual format should still be kept in sync
                  // with the text state, and should be updated by a consumer of a composition of signals (current value + format, desired format, desired format's options).
                  // that's the only reasonable way to avoid repeating onFormatChange in every click handler.
                  Option.when(fmt.usesExplicitDefaults)(
                    label(
                      styleAttr := "display: block; margin-left: 20px",
                      "Explicit defaults",
                      input.withSelf { self =>
                        (
                          `type` := "checkbox",
                          checked <-- jsonExplicitDefaults,
                          disabled <-- writeFormatKind.map(_ =!= fmt),
                          onInput(
                            self
                              .checked
                              .get
                              .flatMap(jsonExplicitDefaults.set)
                          ),
                        )
                      },
                    )
                  ),
                )
              }
          ),
          textArea.withSelf { self =>
            (
              value <-- currentInput,
              onInput(self.value.get.flatMap(currentInput.set)),
              rows := 7,
              styleAttr := "width:300px",
            )
          },
          div(
            pre(
              styleAttr := "text-wrap:wrap",
              code(styleAttr := "color: #aa0000", inputErrors),
            )
          ),
        ),
      )

      e <- div(
        h2(sampleLabel),
        div(
          styleAttr := "display: flex; gap: 20px",
          modelSourceBlock,
          inputView,
        ),
      )
    } yield e
  }

}

private def buildNewSchema(
  idl: String,
  dumper: Dumper,
): IO[Schema[?]] = dumper
  .dump(idl)
  .flatMap { newModelJson =>
    Json
      .read(Blob(newModelJson))(
        using Model.schema
      )
      .liftTo[IO]
  }
  .map { model =>
    DynamicSchemaIndex.load(model)
  }
  .flatMap { dsi =>
    val input =
      dsi
        .allSchemas
        .toList
        .map(_.shapeId)
        .filterNot(_.namespace == "smithy.api")
        .filterNot(_.namespace.startsWith("alloy"))
        .match {
          case one :: Nil => IO.pure(one)
          case other =>
            IO.raiseError(
              new Exception("expected exactly one schema - current app limitation")
            )
        }
        .flatMap { shapeId =>
          dsi
            .getSchema(shapeId)
            .liftTo[IO](new Exception(s"weird - no schema with id $shapeId"))
        }

    val op =
      dsi
        .allServices
        .toList
        .match {
          case Nil        => IO.pure(None)
          case one :: Nil => IO.pure(one.service.some)
          case more =>
            IO.raiseError(new Exception("expected up to one service - current app limitation"))
        }
        .pipe(OptionT(_))
        .map(_.endpoints.toList)
        .semiflatMap {
          case e :: Nil => IO.pure(e)
          case other =>
            IO.raiseError(new Exception("expected exactly one endpoint - current app limitation"))
        }
        .value

    // transplant the Http hint from the operation, if one is present.
    // otherwise, a default value will be used.
    (input, op).mapN {
      case (input, None) => input.toHttpInputSchema
      case (input, Some(op)) =>
        input.addHints(op.hints.get(Http).toList.map(h => h: Hints.Binding)*)
    }
  }

extension [A](s: Schema[A]) {

  private def toHttpInputSchema: Schema[A] = s.addHints(defaultHttpHint)

}
