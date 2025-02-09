//> using dep com.armanbilge::calico::0.2.3
//> using dep org.typelevel::kittens::3.4.0
//> using dep com.disneystreaming.smithy4s::smithy4s-xml::0.18.29
//> using dep com.disneystreaming.smithy4s::smithy4s-protobuf::0.18.29
//> using dep com.disneystreaming.smithy4s::smithy4s-http4s::0.18.29
//> using dep com.disneystreaming.smithy4s::smithy4s-dynamic::0.18.29
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
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import monocle.syntax.all.*
import smithy4s.Document
import smithy4s.json.Json

object App extends IOWebApp {

  case class State(
    source: String,
    result: Either[String, Document],
  )

  object State {

    def make(s: String): State = State(
      source = s,
      result = Json.readDocument(s).leftMap(_.toString),
    )

  }

  def render: Resource[IO, HtmlElement[IO]] = SignallingRef[IO]
    .of(State.make("""{"foo": "bar"}"""))
    .toResource
    .flatMap { state =>
      def updateValue(newValue: String): IO[Unit] = state.set(State.make(newValue))

      div(
        textArea.withSelf { self =>
          (
            value <-- state.map(_.source),
            onInput(self.value.get.flatMap(updateValue)),
          )
        },
        div(pre(code(state.map(_.result).map(_.swap.toOption)))),
      )
    }

}
