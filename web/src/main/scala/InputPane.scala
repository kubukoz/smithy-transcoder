import calico.html.io.*
import calico.html.io.given
import cats.effect.IO
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import org.scalajs.dom.HTMLTextAreaElement
import smithytranscoder.FieldFilter
import smithytranscoder.RenderName

// the one where you write your data
object InputPane {

  // some of these should be defined inside, as well as the signals
  // but no time for that right now, I'm just splitting files
  def make(
    writeFormatKind: SignallingRef[IO, FormatKind],
    inputErrors: Signal[IO, Option[String]],
    fieldFilter: SignallingRef[IO, FieldFilter],
    currentInput: SignallingRef[IO, String],
    canonicalValueSignal: Signal[IO, String],
  ) = div(
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
            Option.when(fmt.usesFieldFilter)(
              label(
                styleAttr := "display: block; margin-left: 20px",
                "Field filter",
                select.withSelf { self =>
                  val values = FieldFilter.values.map { ff =>
                    ff.hints
                      .get(RenderName)
                      .getOrElse(
                        sys.error("missing renderName, this is a codegen bug or something")
                      )
                      .value -> ff
                  }

                  (
                    disabled <-- writeFormatKind.map(_ =!= fmt),
                    values.map { (name, _) =>
                      option(
                        value := name,
                        name,
                      )
                    },
                    onInput(
                      self
                        .value
                        .get
                        .map { choice =>
                          values.find(_._1 === choice).map(_._2).get
                        }
                        .flatMap(fieldFilter.set)
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
        rows := 7,
        styleAttr := "width:300px",
        currentInput
          .changes
          .discrete
          .map { s =>
            val pattern = """(?s)(.+Content-Length: )\d+(.+)""".r
            val newText = pattern.replaceAllIn(
              s,
              m => {
                val actualContentLength = s.split("\n\n").last.length
                m.group(1) + actualContentLength + m.group(2)
              },
            )

            Option.when(newText != s)(newText)
          }
          .unNone
          .foreach { newValue =>
            val ta = self.asInstanceOf[HTMLTextAreaElement]
            val currentSelectionStart = ta.selectionStart

            currentInput.set(newValue) *>
              // wait for textarea and its cursor to update
              IO.cede *>
              // todo: when we run this, the text could've changed already.
              // if it's different, chances are the user has simply appended some text and that could be added before the cursor position (and then the cursor can be shifted)
              // but there's no guarantee that we can run all that without ceding...
              // probably should just move the content-length to a separate input.
              IO(
                ta.setSelectionRange(
                  currentSelectionStart,
                  currentSelectionStart,
                )
              )
          }
          .compile
          .drain
          .background
          .void,
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

}
