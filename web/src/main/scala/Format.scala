import alloy.SimpleRestJson
import cats.derived.*
import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.kernel.Eq
import cats.syntax.all.*
import com.github.plokhotnyuk.jsoniter_scala.core.WriterConfig
import monocle.syntax.all.*
import org.http4s.client.Client
import org.http4s.ember.hack.EncoderHack
import smithy.api.Http
import smithy.api.HttpPayload
import smithy4s.Blob
import smithy4s.Hints
import smithy4s.Service
import smithy4s.ShapeId
import smithy4s.http4s.SimpleRestJsonBuilder
import smithy4s.json.Json
import smithy4s.kinds.PolyFunction5
import smithy4s.schema.OperationSchema
import smithy4s.schema.Schema
import smithy4s.schema.Schema.StructSchema
import smithy4s.xml.Xml

import java.util.Base64

enum FormatKind derives Eq {
  case JSON
  case Protobuf
  case XML
  case SimpleRestJson

  def name = productPrefix

  def usesExplicitDefaults =
    this match {
      case JSON | SimpleRestJson => true
      case _                     => false
    }

  def toFormat(explicitDefaults: Boolean): Format =
    this match {
      case JSON           => Format.JSON(explicitDefaults)
      case Protobuf       => Format.Protobuf
      case XML            => Format.XML
      case SimpleRestJson => Format.SimpleRestJson(explicitDefaults)
    }

}

enum Format derives Eq {

  def kind: FormatKind =
    this match {
      case JSON(_)           => FormatKind.JSON
      case Protobuf          => FormatKind.Protobuf
      case XML               => FormatKind.XML
      case SimpleRestJson(_) => FormatKind.SimpleRestJson
    }

  def jsonExplicitDefaults: Option[Boolean] =
    this match {
      case JSON(explicitDefaults) => explicitDefaults.some
      case _                      => None
    }

  case JSON(explicitDefaults: Boolean)
  case Protobuf
  case XML
  case SimpleRestJson(explicitDefaults: Boolean)

  // todo: look into whether this caches decoders properly
  def decode[A](
    input: String
  )(
    using Schema[A]
  ): IO[Either[String, A]] =
    this match {
      case JSON(explicitDefaults) =>
        Json
          .payloadCodecs
          .configureJsoniterCodecCompiler(_.withExplicitDefaultsEncoding(explicitDefaults))
          .decoders
          .fromSchema(summon[Schema[A]])
          .decode(Blob(input))
          .leftMap(_.toString)
          .pure[IO]

      case Protobuf =>
        Either
          .catchNonFatal(Base64.getDecoder.decode(input))
          .map(Blob(_))
          .flatMap { bytes =>
            smithy4s
              .protobuf
              .Protobuf
              .codecs
              .fromSchema(summon[Schema[A]])
              .readBlob(bytes)
          }
          .leftMap(_.toString)
          .pure[IO]
      case XML =>
        Xml
          .decoders
          .fromSchema(summon[Schema[A]])
          .decode(Blob(input))
          .leftMap(_.toString)
          .pure[IO]

      case SimpleRestJson(explicitDefaults) =>
        val svc = Format.mkFakeService[A]

        Deferred[IO, Either[String, A]]
          .flatMap { deff =>
            val send = SimpleRestJsonBuilder
              .withExplicitDefaultsEncoding(explicitDefaults)
              .routes(
                svc.fromPolyFunction(
                  new PolyFunction5[[I, _, _, _, _] =>> I, smithy4s.kinds.Kind1[IO]#toKind5] {
                    def apply[I, E, O, SI, SO](fa: I): IO[O] =
                      deff.complete(fa.asInstanceOf[A].asRight) *>
                        IO.raiseError(new Exception("shouldn't happen"))
                  }
                )
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

              }

            send *> deff.tryGet.flatMap(_.liftTo[IO](new Exception("promise not fulfilled")))
          }
          .attempt
          .map(_.leftMap(_.getMessage).flatten)
    }

  def encode[A](
    v: A
  )(
    using Schema[A]
  ): IO[String] =
    this match {
      case JSON(explicitDefaults) =>
        Json
          .payloadCodecs
          .configureJsoniterCodecCompiler(_.withExplicitDefaultsEncoding(explicitDefaults))
          .withJsoniterWriterConfig(WriterConfig.withIndentionStep(2))
          .encoders
          .fromSchema(summon[Schema[A]])
          .encode(v)
          .toUTF8String
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

      case SimpleRestJson(explicitDefaults) =>
        val svc = Format.mkFakeService[A]

        IO.deferred[String]
          .flatMap { deff =>
            SimpleRestJsonBuilder
              .withExplicitDefaultsEncoding(explicitDefaults)
              .withMaxArity(Int.MaxValue)
              .apply(svc)
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
              .apply(v)
              .attempt *> deff.get
          }
    }

}

object Format {

// Make a single-operation service using the given schema as input, also copying the Http hint from said schema to the fake operation.
  private def mkFakeService[A: Schema]: Service.Reflective[[I, _, _, _, _] =>> I] = {
    // todo: uncopy paste
    type Op[I, E, O, SI, SO] = I

    new Service.Reflective[Op] {
      def hints: Hints = Hints(alloy.SimpleRestJson())
      def id: ShapeId = ShapeId("demo", "MyService")
      def input[I, E, O, SI, SO](op: Op[I, E, O, SI, SO]): I = op
      def ordinal[I, E, O, SI, SO](op: Op[I, E, O, SI, SO]): Int = 0
      def version: String = ""
      val endpoints: IndexedSeq[Endpoint[?, ?, ?, ?, ?]] = IndexedSeq(
        new smithy4s.Endpoint[Op, A, Nothing, Unit, Nothing, Nothing] {
          val schema: OperationSchema[A, Nothing, Unit, Nothing, Nothing] = Schema
            .operation(ShapeId("demo", "MyOp"))
            .withInput(summon[Schema[A]] match {
              case s: StructSchema[?] => s
              case other              =>
                // non-structs can't directly be inputs, so we wrap them in fake structs with a HttpPayload member
                Schema
                  .struct[A](other.required[A]("body", identity).addHints(HttpPayload()))(identity)
            })
            .withHints(
              summon[Schema[A]].hints.get(Http).map(a => a: Hints.Binding).toList*
            )
          def wrap(input: A): Op[A, Nothing, Unit, Nothing, Nothing] = input
        }
      )
    }
  }

}
