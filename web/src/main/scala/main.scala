//> using dep com.armanbilge::calico::0.2.3
//> using dep org.typelevel::kittens::3.4.0
//> using dep org.typelevel::cats-core::2.13.0
//> using dep com.disneystreaming.smithy4s::smithy4s-xml::0.18.29
//> using dep com.disneystreaming.smithy4s::smithy4s-protobuf::0.18.29
//> using dep com.disneystreaming.smithy4s::smithy4s-http4s::0.18.29
//> using dep com.disneystreaming.smithy4s::smithy4s-dynamic::0.18.29
//> using dep org.http4s::http4s-ember-client::0.23.27
//> using dep org.http4s::http4s-ember-server::0.23.27
//> using dep com.thesamet.scalapb::protobuf-runtime-scala::0.8.14
//> using platform js
//> using jsModuleKind es
//> using option -no-indent
//> using option -deprecation
//> using option -Wunused:all
//> using option -Xkind-projector
//> using scala 3.6.3
import alloy.SimpleRestJson
import calico.IOWebApp
import calico.html.io.*
import calico.html.io.given
import cats.derived.*
import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.kernel.Resource
import cats.effect.std.Mutex
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlDivElement
import fs2.dom.HtmlElement
import monocle.syntax.all.*
import org.http4s.client.Client
import org.http4s.ember.hack.EncoderHack
import org.scalajs.dom.Fetch
import org.scalajs.dom.HttpMethod
import org.scalajs.dom.RequestInit
import scalajs.js
import smithy.api.Http
import smithy.api.HttpHeader
import smithy.api.HttpLabel
import smithy.api.NonEmptyString
import smithy4s.Blob
import smithy4s.Hints
import smithy4s.Service
import smithy4s.ShapeId
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.dynamic.model.Model
import smithy4s.http4s.SimpleRestJsonBuilder
import smithy4s.json.Json
import smithy4s.kinds.PolyFunction5
import smithy4s.schema.OperationSchema
import smithy4s.schema.Schema
import smithy4s.xml.Xml

import java.util.Base64
import scala.scalajs.js.annotation.JSGlobal

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
  )

}

object SampleComponent {

  def make(
    sampleLabel: String,
    initSchema: Schema[?],
    initModel: String,
    initText: String,
  )(
    using dumper: Dumper
  ): Resource[IO, HtmlElement[IO]] = {

    case class State[A](
      currentSchema: Schema[A],
      currentSource: String,
      currentFormat: Format,
      result: Either[String, A],
    ) {
      def updateSource(newSource: String): IO[State[A]] = currentFormat
        .decode(newSource)(
          using currentSchema
        )
        .map { r =>
          copy(
            currentSource = newSource,
            result = r,
          )
        }

      def updateFormat(newFormat: Format): IO[State[A]] =
        result match {
          case Left(_) => this.pure[IO]
          case Right(v) =>
            newFormat
              .encode(v)(
                using currentSchema
              )
              .map { encoded =>
                copy(
                  currentFormat = newFormat,
                  currentSource = encoded,
                )
              }
        }
    }

    object State {

      private def zero = State(
        currentSchema = initSchema,
        currentSource = "",
        currentFormat = Format.JSON,
        result = Left("default value! you shouldn't see this."),
      )

      def init(s: String): IO[State[?]] = zero.updateSource(s)

    }

    enum Format derives Eq {
      case JSON
      case Protobuf
      case XML
      case HTTP

      def name = productPrefix

      // todo: look into whether this caches decoders properly
      def decode[A](
        input: String
      )(
        using Schema[A]
      ): IO[Either[String, A]] =
        this match {
          case JSON =>
            Json
              .read(Blob(input))
              .leftMap(_.toString)
              .pure[IO]

          case Protobuf =>
            smithy4s
              .protobuf
              .Protobuf
              .codecs
              .fromSchema(summon[Schema[A]])
              .readBlob(
                Blob(
                  Base64.getDecoder.decode(input)
                )
              )
              .leftMap(_.toString)
              .pure[IO]
          case XML =>
            Xml
              .decoders
              .fromSchema(summon[Schema[A]])
              .decode(Blob(input))
              .leftMap(_.toString)
              .pure[IO]

          case HTTP =>
            // todo: uncopy paste
            case class Op[I, E, O, SI, SO](i: I)

            val svc =
              new Service.Reflective[Op] {
                def hints: Hints = Hints(SimpleRestJson())
                def id: ShapeId = ShapeId("demo", "MyService")
                def input[I, E, O, SI, SO](op: Op[I, E, O, SI, SO]): I = op.i
                def ordinal[I, E, O, SI, SO](op: Op[I, E, O, SI, SO]): Int = 0
                def version: String = ""
                val endpoints: IndexedSeq[Endpoint[?, ?, ?, ?, ?]] = IndexedSeq(
                  new smithy4s.Endpoint[Op, A, Nothing, Unit, Nothing, Nothing] {
                    val schema: OperationSchema[A, Nothing, Unit, Nothing, Nothing] = Schema
                      .operation(ShapeId("demo", "MyOp"))
                      .withInput(summon[Schema[A]])
                      .withHints(
                        summon[Schema[A]].hints.get(Http).map(a => a: Hints.Binding).toList*
                      )
                    def wrap(input: A): Op[A, Nothing, Unit, Nothing, Nothing] = Op(input)
                  }
                )
              }

            Deferred[IO, Either[String, A]].flatMap { deff =>
              SimpleRestJsonBuilder
                .routes(
                  svc.fromPolyFunction(new PolyFunction5[Op, smithy4s.kinds.Kind1[IO]#toKind5] {
                    def apply[I, E, O, SI, SO](fa: Op[I, E, O, SI, SO]): IO[O] =
                      deff.complete(svc.input(fa).asInstanceOf[A].asRight) *>
                        IO.raiseError(new Exception("shouldn't happen"))
                  })
                )(
                  using svc
                )
                .make
                .liftTo[IO]
                .flatMap { route =>
                  EncoderHack
                    .requestFromString {
                      if input.contains("\r\n") then input
                      else
                        // need to restore \r\n from the textarea because browsers swallow it
                        input.replace("\n", "\r\n")
                    }
                    .flatMap(route.orNotFound.apply(_))
                    .attempt
                    .flatMap {
                      case Left(e)                        => deff.complete(Left(e.toString()))
                      case Right(r) if r.status.isSuccess => IO.unit
                      case Right(r) =>
                        r.bodyText.compile.string.flatMap { responseText =>
                          deff.complete(Left(s"HTTP ${r.status}: $responseText"))
                        }

                    }

                } *> deff.tryGet.flatMap(_.liftTo[IO](new Exception("promise not fulfilled")))

            }
        }
      def encode[A](
        v: A
      )(
        using Schema[A]
      ): IO[String] =
        this match {
          case JSON =>
            Json
              .writePrettyString(v)
              .pure[IO]

          case Protobuf =>
            smithy4s
              .protobuf
              .Protobuf
              .codecs
              .fromSchema(summon[Schema[A]])
              .writeBlob(v)
              .toBase64String
              .pure[IO]

          case XML =>
            Xml
              .write(v)
              .toUTF8String
              .pure[IO]

          case HTTP =>
            case class Op[I, E, O, SI, SO](i: I)

            val svc =
              new Service.Reflective[Op] {
                def hints: Hints = Hints(SimpleRestJson())
                def id: ShapeId = ShapeId("demo", "MyService")
                def input[I, E, O, SI, SO](op: Op[I, E, O, SI, SO]): I = op.i
                def ordinal[I, E, O, SI, SO](op: Op[I, E, O, SI, SO]): Int = 0
                def version: String = ""
                val endpoints: IndexedSeq[Endpoint[?, ?, ?, ?, ?]] = IndexedSeq(
                  new smithy4s.Endpoint[Op, A, Nothing, Unit, Nothing, Nothing] {
                    val schema: OperationSchema[A, Nothing, Unit, Nothing, Nothing] = Schema
                      .operation(ShapeId("demo", "MyOp"))
                      .withInput(summon[Schema[A]])
                      .withHints(
                        summon[Schema[A]].hints.get(Http).map(a => a: Hints.Binding).toList*
                      )
                    def wrap(input: A): Op[A, Nothing, Unit, Nothing, Nothing] = Op(input)
                  }
                )
              }

            IO.deferred[String]
              .flatMap { deff =>
                SimpleRestJsonBuilder(svc)
                  .client(
                    Client[IO] { req =>
                      (EncoderHack
                        .requestToString(req)
                        .flatMap(deff.complete) *> IO.raiseError(
                        new Exception("encoding error in fake client")
                      )).toResource
                    }
                  )
                  .make
                  .toTry
                  .get
                  .apply(Op(v))
                  .attempt *> deff.get
              }
        }
    }

    (
      State.init(initText).flatMap(SignallingRef[IO].of[State[?]]),
      Mutex[IO],
    ).tupled
      .toResource
      .flatMap { (state, mutex) =>
        def updateValue(newValue: String): IO[Unit] = mutex
          .lock
          .surround(state.get.flatMap(_.updateSource(newValue)).flatMap(state.set))

        def updateFormat(newFormat: Format): IO[Unit] = mutex
          .lock
          .surround(
            state
              .get
              .flatMap(_.updateFormat(newFormat))
              .flatMap(state.set)
          )

        def updateSchema(newModelIDL: String): IO[Unit] =
          mutex
            .lock
            .surround {
              dumper
                .dump(newModelIDL)
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
                .flatMap { schema =>
                  // todo: cleanup and so onnnnnn

                  state.get.flatMap { s =>
                    s.currentFormat
                      .decode(s.currentSource)(
                        using schema
                      )
                      .map { result =>
                        s.copy(currentSchema = schema, result = result)
                      }
                      .flatMap(state.set)
                  }
                }

            }
            // todo: errors should be displayed somewhere!
            .attempt
            .void

        val sourceBlock = textArea.withSelf { self =>
          (
            styleAttr := "flex: 2",
            // disabled := true,
            onInput(
              self.value.get.flatMap(updateSchema)
            ),
            value := initModel,
          )
        }

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
                    onInput(self.value.get.flatMap(IO.println)),
                    onChange(self.value.get.map(Format.valueOf).flatMap(updateFormat)),
                    disabled <-- state.map(_.result.isLeft),
                  )
                },
              )
            }),
            textArea.withSelf { self =>
              (
                value <-- state.map(_.currentSource),
                onInput(self.value.get.flatMap(updateValue)),
                rows := 7,
                styleAttr := "width:300px",
              )
            },
          ),
          div(
            styleAttr := "flex: 1",
            pre(code(styleAttr := "color: #aa0000", state.map(_.result).map(_.swap.toOption))),
          ),
        )

        div(
          h2(sampleLabel),
          div(
            styleAttr := "display: flex; gap: 20px",
            sourceBlock,
            demosBlock,
          ),
        )
      }
  }

}

object facades {

  @js.native
  trait Cheerpj extends js.Object {}

  @js.native
  trait JSDumper extends js.Object {
    def dump(s: String): js.Promise[String] = js.native
  }

  object Cheerpj {

    @js.native
    @JSGlobal
    def cheerpjInit(): js.Promise[Unit] = js.native

    @js.native
    @JSGlobal
    def cheerpjRunLibrary(classpath: String): js.Promise[Cheerpj] = js.native

    extension (c: Cheerpj) {

      def dumper: js.Promise[JSDumper] = c
        .asInstanceOf[js.Dynamic]
        .com
        .kubukoz
        .SmithyDump
        .asInstanceOf[js.Promise[JSDumper]]

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
        .flatMap(r => IO.fromPromise(IO(r.text())))
    }

}
