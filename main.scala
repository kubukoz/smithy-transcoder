//> using dep com.armanbilge::calico::0.2.3
//> using dep org.typelevel::kittens::3.4.0
//> using dep com.disneystreaming.smithy4s::smithy4s-xml::0.18.29
//> using dep com.disneystreaming.smithy4s::smithy4s-protobuf::0.18.29
//> using dep com.disneystreaming.smithy4s::smithy4s-http4s::0.18.29
//> using dep com.disneystreaming.smithy4s::smithy4s-dynamic::0.18.29
//> using dep "com.thesamet.scalapb::protobuf-runtime-scala::0.8.14"
//> using platform js
//> using jsModuleKind common
//> using option -no-indent
//> using option -deprecation
//> using option -Wunused:all
//> using option -Xkind-projector
//> using scala 3.6.3
import calico.IOWebApp
import calico.html.io.*
import calico.html.io.given
import cats.derived.*
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlDivElement
import fs2.dom.HtmlElement
import monocle.syntax.all.*
import smithy4s.Blob
import smithy4s.json.Json
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

      def updateFormat(newFormat: Format): State =
        result match {
          case Left(_) => this
          case Right(v) =>
            copy(
              currentFormat = newFormat,
              currentSource = newFormat.encode(v),
            )
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
        }
      def encode(v: A): String =
        this match {
          case JSON =>
            Json
              .writeBlob(v)(
                using schema
              )
              .toUTF8String
          case Protobuf =>
            smithy4s.protobuf.Protobuf.codecs.fromSchema(schema).writeBlob(v).toBase64String
          case XML =>
            Xml
              .write(v)(
                using schema
              )
              .toUTF8String
        }
    }

    SignallingRef[IO]
      .of(State.init(initText))
      .toResource
      .flatMap { state =>
        def updateValue(newValue: String): IO[Unit] = state.update(_.updateSource(newValue))
        def updateFormat(newFormat: Format): IO[Unit] = state.update(_.updateFormat(newFormat))

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
