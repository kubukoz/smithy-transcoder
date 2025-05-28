import calico.IOWebApp
import calico.html.io.*
import calico.html.io.given
import cats.data.OptionT
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.concurrent.Channel
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import fs2.dom.Window
import monocle.Focus
import monocle.Lens
import smithy.api.Http
import smithy.api.NonEmptyString
import smithy.api.Trait
import smithy4s.Blob
import smithy4s.Document
import smithy4s.Hints
import smithy4s.ShapeId
import smithy4s.ShapeTag
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.dynamic.model.Model
import smithy4s.json.Json
import smithy4s.schema.Schema
import util.chaining.*

case class TranscoderSelect()

object TranscoderSelect extends ShapeTag.Companion[TranscoderSelect] {
  val id: ShapeId = ShapeId("st", "select")

  val schema: Schema[TranscoderSelect] = Schema.constant(TranscoderSelect())
}

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
        styleAttr := "padding: 20px;",
        Dumper.progressBar(dumperSig),
        renderMain(
          using dumperSig.map(_.toOption)
        ),
      )
    }

  case class Example(
    name: String,
    model: String,
    input: String,
  ) {

    def toSampleComponentInput: SampleComponent.ExternalUpdate = SampleComponent.ExternalUpdate(
      model = model,
      input = input,
    )

  }

  val examples = List(
    Example(
      name = "Struct",
      model =
        """$version: "2"
          |
          |namespace demo
          |
          |structure Struct {
          |  @required name: String
          |  @required age: Integer
          |}
          |""".stripMargin,
      input = """{"name": "foo", "age": 42}""",
    ),
    Example(
      name = "Multiple shapes",
      model =
        """$version: "2"
          |
          |namespace demo
          |
          |@st#select
          |structure Struct {
          |  @required first: String
          |  @required second: Integer
          |  @required nested: OtherStruct
          |}
          |
          |structure OtherStruct {
          |  @required hello: String
          |}
          |""".stripMargin,
      input = """{"first": "foo", "second": 42, "nested": {"hello": "world"}}""",
    ),
    Example(
      name = "String",
      model =
        """$version: "2"
          |
          |namespace demo
          |
          |string String
          |""".stripMargin,
      input = """"foo"""",
    ),
    Example(
      name = "HTTP input",
      model =
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
      input = """{"id": "foo", "name": "bar", "details": "baz"}""",
    ),
    Example(
      name = "Union",
      model =
        """$version: "2"
          |
          |namespace demo
          |
          |union Union {
          |  left: String
          |  right: Integer
          |}
          |""".stripMargin,
      input = """{"left": "hello"}""",
    ),
    Example(
      name = "Document",
      model =
        """$version: "2"
          |
          |namespace demo
          |
          |document Document
          |""".stripMargin,
      input = """{"foo": "bar"}""",
    ),
    Example(
      name = "Open union",
      model =
        """$version: "2"
          |
          |namespace demo
          |
          |use alloy#jsonUnknown
          |
          |union MyOpenUnion {
          |  s1: Unit
          |  s2: Unit
          |  @jsonUnknown other: Document
          |}
          |""".stripMargin,
      input = """{"whatisthis": "a string, clearly"}""",
    ),
  )

  val defaultExample = examples.groupBy(_.name)("Struct").head

  val defaultSchema =
    Schema
      .struct[(String, Int)](
        Schema.string.required("name", _._1),
        Schema.int.required("age", _._2),
      )((_, _))
      .withId("demo", "Struct")
      .toHttpInputSchema

  def renderMain(
    using dumperSig: DumperOptionSig
  ): Resource[IO, HtmlElement[IO]] =
    for {
      exampleChoice <- Channel.synchronous[IO, Example].toResource

      e <- div(
        h1("Smithy Transcoder"),
        div(
          "Load example: ",
          select.withSelf { self =>
            (
              disabled <-- dumperSig.map(_.isEmpty),
              examples.map { e =>
                option(
                  value := e.name,
                  e.name,
                )
              },
              onInput(
                self.value.get.flatMap { choice =>
                  exampleChoice
                    .send(examples.find(_.name === choice).get)
                    .void
                }
              ),
            )
          },
          SampleComponent.make(
            initSchema = defaultSchema,
            initInput = defaultExample.input,
            initModel = defaultExample.model,
            externalUpdates = exampleChoice.stream.map(_.toSampleComponentInput),
          ),
        ),
      )
    } yield e

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

  case class ExternalUpdate(model: String, input: String)

  def make(
    initSchema: Schema[?],
    initModel: String,
    initInput: String,
    externalUpdates: fs2.Stream[IO, ExternalUpdate],
  )(
    using DumperOptionSig
  ): Resource[IO, HtmlElement[IO]] = {
    val transcoderPreludeText =
      """$version: "2"
        |namespace st
        |
        |/// Apply this to disambiguate which shape should be used for transcoding.
        |@trait structure select {}""".stripMargin

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
                buildNewSchema(prelude = transcoderPreludeText, idl = idl, dumper = dumper).attempt
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

      canonicalValueSignal = currentValueSignal.map {
        _.map { vws =>
          Document.Encoder.fromSchema(vws.s).encode(vws.a)
        }.fold(_ => "-", _.show)
      }

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

      _ <-
        externalUpdates
          .evalMap { update =>
            currentIDL.set(update.model) *>
              currentInput.set(update.input)
          }
          .compile
          .drain
          .background

      modelErrors = schema.map(_.swap.toOption.map(_.getMessage))

      modelSourceBlock = div(
        styleAttr := "display: flex; flex-direction:column; flex: 1; overflow: auto",
        div(
          h3("Schema"),
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
          ),
        ),
        pre(code("prelude.smithy")),
        textArea(
          rows := 5,
          disabled := true,
          transcoderPreludeText,
        ),
        pre(code("input.smithy")),
        textArea.withSelf { self =>
          (
            rows := 30,
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
        div(
          p(schema.map(_.toOption.map(s => s"Matched shape: ${s.shapeId}")))
        ),
      )

      inputErrors = currentValueSignal.map(_.swap.toOption)

      inputView = div(
        styleAttr := """display: flex; flex-direction: column; flex: 1; overflow: auto""".stripMargin,
        h3("Format & input"),
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
                    a(
                      small("(what is this?)"),
                      href := "https://disneystreaming.github.io/smithy4s/docs/protocols/simple-rest-json/overview/#field-filtering-and-explicit-null-encoding",
                    ),
                  )
                ),
              )
            }
        ),
        textArea.withSelf { self =>
          (
            value <-- currentInput,
            onInput(self.value.get.flatMap(currentInput.set)),
            rows := 15,
          )
        },
        div(
          pre(
            styleAttr := "text-wrap:wrap",
            code(styleAttr := "color: #aa0000", inputErrors),
          )
        ),
        h3("Document representation"),
        pre(code(canonicalValueSignal)),
      )

      e <- div(
        styleAttr := "display: flex; gap: 20px",
        modelSourceBlock,
        inputView,
      )
    } yield e
  }

}

private def buildNewSchema(
  prelude: String,
  idl: String,
  dumper: Dumper,
): IO[Schema[?]] = dumper
  .dump("prelude.smithy" -> prelude, "input.smithy" -> idl)
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
    def deconflict[T](
      items: List[T],
      kindPlural: String,
    )(
      hints: T => Hints
    ) =
      items.match {
        case Nil        => IO.raiseError(new Exception(s"no $kindPlural found"))
        case one :: Nil => IO.pure(one)
        case more =>
          more
            .filter(hints(_).has[TranscoderSelect])
            .match {
              case Nil        => IO.raiseError(multipleError(kindPlural))
              case one :: Nil => IO.pure(one)
              case _          => IO.raiseError(multipleWithTraitError(kindPlural))
            }
      }

    def multipleError(kindPlural: String) =
      new Exception(
        s"""Multiple $kindPlural found but none have the ${TranscoderSelect.id} trait.
           |Try adding @${TranscoderSelect.id} to the shape you want to use.""".stripMargin
      )

    def multipleWithTraitError(kindPlural: String) =
      new Exception(
        s"""Multiple $kindPlural with the ${TranscoderSelect.id} trait found.
           |Choose one you want to use, and remove the trait from the others.""".stripMargin
      )

    val input = dsi
      .allSchemas
      .toList
      .filterNot(_.shapeId.namespace == "smithy.api")
      .filterNot(_.shapeId.namespace.startsWith("alloy"))
      .filterNot(_.hints.has[Trait])
      .pipe(deconflict(_, "schemas")(_.hints))

    val op =
      dsi
        .allServices
        .toList
        .match {
          case Nil  => IO.pure(None)
          case more => deconflict(more, "services")(_.service.hints).map(_.service.some)
        }
        .pipe(OptionT(_))
        .map(_.endpoints.toList)
        .semiflatMap(deconflict(_, "endpoints")(_.hints))
        .value

    // transplant the Http hint from the operation, if one is present.
    // otherwise, a default value will be used.
    (input, op).mapN {
      case (input, None) => input.toHttpInputSchema
      case (input, Some(op)) =>
        input.addHints(op.hints.get[Http].toList.map(h => h: Hints.Binding)*)
    }
  }

extension [A](s: Schema[A]) {

  private def toHttpInputSchema: Schema[A] = s.addHints(defaultHttpHint)

}
