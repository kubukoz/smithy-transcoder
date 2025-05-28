import calico.html.io.*
import calico.html.io.given
import cats.data.OptionT
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
import smithy.api.Trait
import smithy4s.Blob
import smithy4s.Document
import smithy4s.Hints
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.dynamic.model.Model
import smithy4s.json.Json
import smithy4s.schema.FieldFilter
import smithy4s.schema.Schema
import util.chaining.*

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

      dumperOption = summon[DumperOptionSig]

      currentIDL = state.lens(_.currentIDL)
      currentInput = state.lens(_.currentInput)
      fieldFilter = state.lens(_.fieldFilter)

      readFormatKind = state.lens(_.readFormatKind)
      readFormat = (readFormatKind.sig, fieldFilter.sig).mapN(_.toFormat(_))

      writeFormatKind = state.lens(_.writeFormatKind)
      writeFormat = (writeFormatKind.sig, fieldFilter.sig).mapN(_.toFormat(_))

      schema <- (currentIDL.changes, dumperOption)
        .tupled
        .discrete
        .switchMap {
          case (idl, Some(dumper)) =>
            fs2
              .Stream
              .eval(
                buildNewSchema(prelude = transcoderPreludeText, idl = idl, dumper = dumper).attempt
              )

          case _ => fs2.Stream.empty
        }
        // for offline / jvmless mode, general "instantness"
        .holdResource(initSchema.asRight)

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

      canonicalValueSignal = currentValueSignal.map {
        _.map { vws =>
          Document
            .Encoder
            .withFieldFilter(FieldFilter.EncodeAll)
            .fromSchema(vws.s)
            .encode(vws.a)
        }.fold(_ => "-", _.show)
      }

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

      _ <-
        externalUpdates
          .evalMap { update =>
            currentIDL.set(update.model) *>
              currentInput.set(update.input)
          }
          .compile
          .drain
          .background

      modelErrors = schema.map(_.swap.toOption.map(_.getMessage))

      modelSourceBlock = SchemaPane.make(
        currentIDL = currentIDL,
        schema = schema,
        modelErrors = modelErrors,
        transcoderPreludeText = transcoderPreludeText,
      )

      inputErrors = currentValueSignal.map(_.swap.toOption)

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

private def buildNewSchema(
  prelude: String,
  idl: String,
  dumper: Dumper,
): IO[Schema[?]] = dumper
  .dump("prelude.smithy" -> prelude, "input.smithy" -> idl)
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
    def deconflict[T](
      items: List[T],
      kindPlural: String,
    )(
      hints: T => Hints
    ) =
      items.match {
        case Nil        => IO.raiseError(new Exception(s"no $kindPlural found"))
        case one :: Nil => IO.pure(one)
        case more =>
          more
            .filter(hints(_).has[TranscoderSelect])
            .match {
              case Nil        => IO.raiseError(multipleError(kindPlural))
              case one :: Nil => IO.pure(one)
              case _          => IO.raiseError(multipleWithTraitError(kindPlural))
            }
      }

    def multipleError(kindPlural: String) =
      new Exception(
        s"""Multiple $kindPlural found but none have the ${TranscoderSelect.id} trait.
           |Try adding @${TranscoderSelect.id} to the shape you want to use.""".stripMargin
      )

    def multipleWithTraitError(kindPlural: String) =
      new Exception(
        s"""Multiple $kindPlural with the ${TranscoderSelect.id} trait found.
           |Choose one you want to use, and remove the trait from the others.""".stripMargin
      )

    val input = dsi
      .allSchemas
      .toList
      .filterNot(_.shapeId.namespace == "smithy.api")
      .filterNot(_.shapeId.namespace.startsWith("alloy"))
      .filterNot(_.hints.has[Trait])
      .pipe(deconflict(_, "schemas")(_.hints))

    val op =
      dsi
        .allServices
        .toList
        .match {
          case Nil  => IO.pure(None)
          case more => deconflict(more, "services")(_.service.hints).map(_.service.some)
        }
        .pipe(OptionT(_))
        .map(_.endpoints.toList)
        .semiflatMap(deconflict(_, "endpoints")(_.hints))
        .value

    // transplant the Http hint from the operation, if one is present.
    // otherwise, a default value will be used.
    (input, op).mapN {
      case (input, None) => input.toHttpInputSchema
      case (input, Some(op)) =>
        input.addHints(op.hints.get[Http].toList.map(h => h: Hints.Binding)*)
    }
  }

extension [A](s: Schema[A]) {

  private def toHttpInputSchema: Schema[A] = s.addHints(defaultHttpHint)

}
