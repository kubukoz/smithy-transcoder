import calico.IOWebApp
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
import org.scalajs.dom.Fetch
import org.scalajs.dom.HttpMethod
import org.scalajs.dom.RequestInit
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

  val sampleInitSchema =
    """$version: "2"
      |
      |namespace demo
      |
      |structure Struct {}
      |""".stripMargin

  // val render: Resource[IO, HtmlElement[IO]] = div {
  //   Dumper
  //     .inBrowser
  //     .flatMap(_.dump(str))
  //     .timed
  //     .flatMap((time, r) => IO.println(s"loaded thing in ${time.toMillis}ms").as(r))
  //     .toResource
  // }

  def render: Resource[IO, HtmlElement[IO]] = renderMain(
    using Dumper.remote
  )

  def renderMain(
    using dumper: Dumper
  ): Resource[IO, HtmlElement[IO]] = div(
    SampleComponent.make(
      "Struct",
      Schema.tuple(Schema.string, Schema.int).withId("demo", "Struct"),
      initText = """{"_1": "foo", "_2": 42}""",
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
      initText = """{"id": "foo", "name": "bar", "details": "baz"}""",
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
      initText = """"foo"""",
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
      initText = """{"left": "hello"}""",
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
      initText = """{"foo": "bar"}""",
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
    initText: String,
  )(
    using Dumper
  ): Resource[IO, HtmlElement[IO]] = {

    case class State(
      currentIDL: String,
      currentSource: String,
      currentFormat: Format,
    )

    object State {

      def init(s: String): State = State(
        currentIDL = initModel,
        currentSource = s,
        currentFormat = Format.JSON,
      )

    }

    SignallingRef[IO]
      .of(State.init(initText))
      .toResource
      .flatMap { state =>
        val currentFormatSignal = state.map(_.currentFormat)

        val currentTextSignal = state.map(_.currentSource)

        val mkSchemaSignal = state
          .map(_.currentIDL)
          .discrete
          .changes
          .switchMap { idl =>
            fs2.Stream.eval(buildNewSchema(idl).attempt)
          }
          .holdResource(initSchema.asRight)

        mkSchemaSignal
          .flatMap { schemaErrSignal =>
            val mkCurrentValueSignal = (currentTextSignal, currentFormatSignal, schemaErrSignal)
              .mapN { (text, format, schemaErr) =>
                schemaErr.match {
                  case Right(schema) =>
                    format.decode(text)(
                      using schema
                    )
                  case Left(e) => IO.pure("Invalid schema".asLeft)
                }
              }
              .discrete
              .switchMap(fs2.Stream.eval(_).handleErrorWith(_ => fs2.Stream.empty))
              .holdResource(initSchema.asRight)

            mkCurrentValueSignal.map { currentValueSignal =>
              (state, schemaErrSignal, currentValueSignal)
            }
          }
      }
      .flatMap { (state, schemaSignal, currentValueSignal) =>
        val currentErrorsSignal = currentValueSignal.map(_.swap.toOption)
        val hasValueErrors = currentErrorsSignal.map(_.isDefined)

        val schemaErrorsSignal = schemaSignal.map(_.swap.toOption.map(_.getMessage))

        val modelSourceBlock = div(
          styleAttr := "display: flex; flex-direction:column; flex: 2; overflow: auto",
          textArea.withSelf { self =>
            (
              styleAttr := "flex: 1;min-height: 150px;",
              // disabled := true,
              onInput(
                self.value.get.flatMap(idl => state.update(_.copy(currentIDL = idl)))
              ),
              value <-- state.map(_.currentIDL),
            )
          },
          div(
            pre(
              styleAttr := "text-wrap: wrap",
              code(
                styleAttr := "color: #aa0000",
                schemaErrorsSignal,
              ),
            )
          ),
        )

        val demosBlock = div(
          styleAttr := """display: flex; flex: 3""".stripMargin,
          div(
            styleAttr := "flex: 1",
            form(Format.values.toList.map { fmt =>
              label(
                styleAttr := "display:block",
                fmt.name,
                input.withSelf { self =>
                  (
                    `type` := "radio",
                    nameAttr := "format",
                    value := fmt.name,
                    checked <-- state.map(_.currentFormat === fmt),
                    onInput(self.value.get.map(Format.valueOf).flatMap { fmt =>
                      state.update(_.copy(currentFormat = fmt))
                    }),
                    disabled <-- hasValueErrors,
                  )
                },
              )
            }),
            textArea.withSelf { self =>
              (
                value <-- state.map(_.currentSource),
                onInput(self.value.get.flatMap(v => state.update(_.copy(currentSource = v)))),
                rows := 7,
                styleAttr := "width:300px",
              )
            },
            div(
              pre(
                styleAttr := "text-wrap:wrap",
                code(styleAttr := "color: #aa0000", currentErrorsSignal),
              )
            ),
          ),
        )

        div(
          h2(sampleLabel),
          div(
            styleAttr := "display: flex; gap: 20px",
            modelSourceBlock,
            demosBlock,
          ),
        )
      }
  }

}

trait Dumper {
  def dump(s: String): IO[String]
}

object Dumper {

  def inBrowser: IO[Dumper] =
    IO.fromPromise(IO(facades.Cheerpj.cheerpjInit())) *>
      IO.fromPromise(IO(facades.Cheerpj.cheerpjRunLibrary("/app/SmithyDump.jar")))
        .flatMap { c =>
          IO.fromPromise(IO(c.dumper))
        }
        .map { underlying =>
          new {
            def dump(s: String): IO[String] = IO.fromPromise(IO(underlying.dump(s)))
          }
        }

  def remote: Dumper =
    new {
      // http POST /api/dump s=$s
      def dump(s: String): IO[String] = IO
        .fromPromise {
          IO {
            Fetch.fetch(
              "/api/dump",
              new RequestInit {
                this.method = HttpMethod.POST
                this.body = s
              },
            )
          }
        }
        .flatMap(r =>
          IO.fromPromise(IO(r.text())).flatMap {
            case body if r.status == 200 => IO.pure(body)
            case body                    => IO.raiseError(new Exception(body))
          }
        )
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
