import calico.html.io.*
import calico.html.io.given
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import monocle.Focus
import monocle.Lens
import smithy.api.Http
import smithy.api.NonEmptyString
import smithy4s.Document
import smithy4s.Hints
import smithy4s.schema.FieldFilter
import smithy4s.schema.Schema

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
      fieldFilter: FieldFilter,
    )

    object State {
      val init: State = {
        val initFmt = FormatKind.JSON

        new State(
          currentIDL = initModel,
          currentInput = initInput,
          readFormatKind = initFmt,
          writeFormatKind = initFmt,
          fieldFilter = FieldFilter.Default,
        )
      }
    }
    case class ValueWithSchema[A](a: A, s: Schema[A])

    for {
      state <- SignallingRef[IO].of(State.init).toResource

      currentIDL = state.lens(_.currentIDL)
      currentInput = state.lens(_.currentInput)
      fieldFilter = state.lens(_.fieldFilter)

      readFormatKind = state.lens(_.readFormatKind)
      writeFormatKind = state.lens(_.writeFormatKind)

      _ <-
        externalUpdates
          .evalMap { update =>
            currentIDL.set(update.model) *>
              currentInput.set(update.input)
          }
          .compile
          .drain
          .background

      schema <- (currentIDL.changes, summon[DumperOptionSig])
        .tupled
        .discrete
        .switchMap {
          case (idl, Some(dumper)) =>
            fs2
              .Stream
              .eval(
                SchemaBuilder
                  .buildNewSchema(prelude = transcoderPreludeText, idl = idl, dumper = dumper)
                  .attempt
              )

          case _ => fs2.Stream.empty
        }
        // for offline / jvmless mode, general "instantness"
        .holdResource(initSchema.asRight)

      modelSourceBlock = {
        val modelErrors = schema.map(_.swap.toOption.map(_.getMessage))

        SchemaPane.make(
          currentIDL = currentIDL,
          schema = schema,
          modelErrors = modelErrors,
          transcoderPreludeText = transcoderPreludeText,
        )
      }

      readFormat = (readFormatKind.sig, fieldFilter.sig).mapN(_.toFormat(_))
      writeFormat = (writeFormatKind.sig, fieldFilter.sig).mapN(_.toFormat(_))

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

      inputErrors = currentValueSignal.map(_.swap.toOption)

      canonicalValueSignal = currentValueSignal.map {
        _.map { vws =>
          Document
            .Encoder
            .withFieldFilter(FieldFilter.EncodeAll)
            .fromSchema(vws.s)
            .encode(vws.a)
        }.fold(_ => "-", _.show)
      }

      inputView = InputPane.make(
        writeFormatKind = writeFormatKind,
        inputErrors = inputErrors,
        fieldFilter = fieldFilter,
        currentInput = currentInput,
        canonicalValueSignal = canonicalValueSignal,
      )

      e <- div(
        styleAttr := "display: flex; gap: 20px",
        modelSourceBlock,
        inputView,
      )
    } yield e
  }

}

private val defaultHttpHint = Http(NonEmptyString("POST"), NonEmptyString("/"))

extension [A](s: Schema[A]) {

  private def toHttpInputSchema: Schema[A] = s.addHints(defaultHttpHint)

}
