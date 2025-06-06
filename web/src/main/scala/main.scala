import calico.IOWebApp
import calico.html.io.*
import calico.html.io.given
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.concurrent.Channel
import fs2.concurrent.Signal
import fs2.dom.HtmlElement
import fs2.dom.Window
<<<<<<< HEAD
import monocle.Focus
import monocle.Lens
import org.scalajs.dom.HTMLTextAreaElement
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
=======
  >>>>>>> main
import smithy4s.schema.Schema

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
      stateRef =
        Window[IO]
          .location
          .hash
          .imap(StateHashCodec.decode(_)) {
            case HashState.Valid(data)                    => StateHashCodec.encode(data)
            case HashState.Missing | HashState.Invalid(_) => ""
          }

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
            stateRef = stateRef,
          ),
        ),
      )
    } yield e

}
