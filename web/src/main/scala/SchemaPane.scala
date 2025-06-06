import calico.html.io.*
import calico.html.io.given
import cats.effect.IO
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import smithy4s.schema.Schema
import util.chaining.*

object SchemaPane {

  def make(
    currentIDL: SignallingRef[IO, String],
    schema: Signal[IO, Either[Throwable, Schema[?]]],
    modelErrors: Signal[IO, Option[String]],
    transcoderPreludeText: String,
  )(
    using
    dumperOption: DumperOptionSig
  ) = div(
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

}
