import calico.IOWebApp
import calico.html.HtmlAttr
import calico.html.io.*
import calico.html.io.given
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlDivElement
import fs2.dom.HtmlElement
import monocle.syntax.all.*
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

object App extends IOWebApp {

  def render: Resource[IO, HtmlElement[IO]] = Dumper.inBrowser.flatMap { dumper =>
    div(
      Dumper
        .progressBar(dumper),
      renderMain(
        using dumper
      ),
    )
  }

  def renderMain(
    using dumperSig: DumperSig
  ): Resource[IO, HtmlElement[IO]] = div(
    SampleComponent.make(
      "Struct",
      Schema.tuple(Schema.string, Schema.int).withId("demo", "Struct"),
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
        .addHints(Http(method = NonEmptyString("PUT"), uri = NonEmptyString("/data/{id}"))),
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
      Schema.string,
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
      Schema.either(Schema.string, Schema.int).withId("demo", "Union"),
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
      Schema.document,
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

object SampleComponent {

  def make(
    sampleLabel: String,
    initSchema: Schema[?],
    initModel: String,
    initInput: String,
  )(
    using DumperSig
  ): Resource[IO, HtmlElement[IO]] = {

    case class State(
      currentIDL: String,
      currentInput: String,
      selectedFormat: Format,
    )

    object State {
      val init: State = {
        val initFmt = Format.JSON

        new State(
          currentIDL = initModel,
          currentInput = initInput,
          selectedFormat = initFmt,
        )
      }
    }
    case class ValueWithSchema[A](a: A, s: Schema[A])

    for {
      state <- SignallingRef[IO].of(State.init).toResource
      currentIDL = state.map(_.currentIDL)
      currentInput = state.map(_.currentInput)
      format = state.map(_.selectedFormat)

      schema <- currentIDL
        .discrete
        .changes
        .switchMap { idl =>
          fs2.Stream.eval(summon[DumperSig].get).flatMap {
            case Dumper.State.Loaded(dumper) =>
              fs2
                .Stream
                .eval(
                  buildNewSchema(idl)(
                    using dumper
                  ).attempt
                )
            case _ => fs2.Stream.empty
          }
        }
        // for offline / jvmless mode, general "instantness"
        .holdResource(initSchema.asRight)

      currentValueSignal <-
        (currentInput, format, schema)
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

      onFormatChange =
        (fmt: Format) =>
          currentValueSignal.get.flatMap {
            case Right(vs) =>
              fmt
                .encode(vs.a)(
                  using vs.s
                )
                .flatMap { encoded =>
                  state.update(_.copy(currentInput = encoded, selectedFormat = fmt))
                }
            case Left(_) => IO.unit
          }

      modelErrors = schema.map(_.swap.toOption.map(_.getMessage))

      modelSourceBlock = div(
        styleAttr := "display: flex; flex-direction:column; flex: 2; overflow: auto",
        textArea.withSelf { self =>
          (
            styleAttr := "flex: 1;min-height: 150px;",
            disabled <-- summon[DumperSig].map {
              case Dumper.State.Loaded(_) => false
              case _                      => true
            },
            // disabled := true,
            onInput(
              self.value.get.flatMap(idl => state.update(_.copy(currentIDL = idl)))
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
            Format
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
                      checked <-- format.map(_ === fmt),
                      onInput(self.value.get.map(Format.valueOf).flatMap(onFormatChange)),
                      disabled <-- inputErrors.map(_.isDefined),
                    )
                  },
                )
              }
          ),
          textArea.withSelf { self =>
            (
              value <-- currentInput,
              onInput(self.value.get.flatMap(v => state.update(_.copy(currentInput = v)))),
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
  idl: String
)(
  using dumper: Dumper
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
  }
