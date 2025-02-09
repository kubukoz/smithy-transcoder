//> using dep com.armanbilge::calico::0.2.3
//> using dep org.typelevel::kittens::3.4.0
//> using dep com.disneystreaming.smithy4s::smithy4s-xml::0.18.29
//> using dep com.disneystreaming.smithy4s::smithy4s-protobuf::0.18.29
//> using dep com.disneystreaming.smithy4s::smithy4s-http4s::0.18.29
//> using dep com.disneystreaming.smithy4s::smithy4s-dynamic::0.18.29
//> using dep org.http4s::http4s-ember-client::0.23.27
//> using dep org.http4s::http4s-ember-server::0.23.27
//> using dep com.thesamet.scalapb::protobuf-runtime-scala::0.8.14
//> using platform js
//> using jsModuleKind common
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
import cats.effect.unsafe.IORuntime
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlDivElement
import fs2.dom.HtmlElement
import monocle.syntax.all.*
import org.http4s.Request
import org.http4s.client.Client
import org.http4s.ember.hack.EncoderHack
import smithy.api.HttpHeader
import smithy.api.HttpLabel
import smithy.api.HttpPayload
import smithy4s.Blob
import smithy4s.Hints
import smithy4s.Service
import smithy4s.ShapeId
import smithy4s.http4s.SimpleRestJsonBuilder
import smithy4s.http4s.internals.SimpleRestJsonCodecs
import smithy4s.json.Json
import smithy4s.schema.OperationSchema
import smithy4s.schema.Schema
import smithy4s.xml.Xml

import java.util.Base64

object App extends IOWebApp {

  val render: Resource[IO, HtmlElement[IO]] = div(
    SampleComponent.make(
      "Document",
      Schema.document,
      """{"foo": "bar"}""",
    ),
    SampleComponent.make(
      "String",
      Schema.string,
      """"foo"""",
    ),
    SampleComponent.make(
      "Struct",
      Schema.tuple(Schema.string, Schema.int).withId("demo", "Struct"),
      """{"_1": "foo", "_2": 42}""",
    ),
    SampleComponent.make(
      "Union",
      Schema.either(Schema.string, Schema.int).withId("demo", "Union"),
      """{"left": "hello"}""",
    ),
    SampleComponent.make(
      "HTTP input",
      Schema.struct[(String, String, String)](
        Schema.string.required[(String, String, String)]("id", _._1).addHints(HttpLabel()),
        Schema
          .string
          .required[(String, String, String)]("name", _._2)
          .addHints(HttpHeader("x-name")),
        Schema.string.required[(String, String, String)]("details", _._3),
      )(Tuple3.apply),
      """{"id": "foo", "name": "bar", "details": "baz"}""",
    ),
  )

}

object SampleComponent {

  def make[A](sampleLabel: String, schema: Schema[A], initText: String)
    : Resource[IO, HtmlElement[IO]] = {

    case class State(
      currentSource: String,
      currentFormat: Format,
      result: Either[String, A],
    ) {
      def updateSource(newSource: String): State = copy(
        currentSource = newSource,
        result = currentFormat.decode(newSource),
      )

      def updateFormat(newFormat: Format): IO[State] =
        result match {
          case Left(_) => this.pure[IO]
          case Right(v) =>
            newFormat.encode(v).map { encoded =>
              copy(
                currentFormat = newFormat,
                currentSource = encoded,
              )
            }
        }
    }

    object State {

      private def zero = State("", Format.JSON, Left("default value! you shouldn't see this."))

      def init(s: String): State = zero.updateSource(s)

    }

    enum Format derives Eq {
      case JSON
      case Protobuf
      case XML
      case HTTP

      def name = productPrefix

      // todo: look into whether this caches decoders properly
      def decode(input: String): Either[String, A] =
        this match {
          case JSON =>
            Json
              .read(Blob(input))(
                using schema
              )
              .leftMap(_.toString)

          case Protobuf =>
            smithy4s
              .protobuf
              .Protobuf
              .codecs
              .fromSchema(schema)
              .readBlob(
                Blob(
                  Base64.getDecoder.decode(input)
                )
              )
              .leftMap(_.toString)
          case XML => Xml.decoders.fromSchema(schema).decode(Blob(input)).leftMap(_.toString)

          // todo
          case HTTP => Left("decoding http is not possible yet")
        }
      def encode(v: A): IO[String] =
        this match {
          case JSON =>
            Json
              .writeBlob(v)(
                using schema
              )
              .toUTF8String
              .pure[IO]

          case Protobuf =>
            smithy4s
              .protobuf
              .Protobuf
              .codecs
              .fromSchema(schema)
              .writeBlob(v)
              .toBase64String
              .pure[IO]

          case XML =>
            Xml
              .write(v)(
                using schema
              )
              .toUTF8String
              .pure[IO]

          case HTTP =>
            case class Op[I, E, O, SI, SO](i: I)

            val s = schema

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
                      .withInput(s)
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
                        .flatMap(deff.complete) *> IO.stub).toResource
                    }
                  )
                  .make
                  .toTry
                  .get
                  .apply(Op(v))
                  .voidError *> deff.get
              }
        }
    }

    (
      SignallingRef[IO]
        .of(State.init(initText)),
      Mutex[IO],
    ).tupled
      .toResource
      .flatMap { (state, mutex) =>
        def updateValue(newValue: String)
          : IO[Unit] = mutex.lock.surround(state.update(_.updateSource(newValue)))
        def updateFormat(newFormat: Format): IO[Unit] = mutex
          .lock
          .surround(
            state
              .get
              .flatMap(_.updateFormat(newFormat))
              .flatMap(state.set)
          )

        div(
          h2(sampleLabel),
          form(Format.values.toList.map { fmt =>
            label(
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
              rows := 3,
              styleAttr := "width:200px;",
            )
          },
          div(pre(code(state.map(_.result).map(_.swap.toOption)))),
        )
      }
  }

}
