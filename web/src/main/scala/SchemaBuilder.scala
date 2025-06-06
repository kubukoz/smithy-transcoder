import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import smithy.api.Http
import smithy.api.Trait
import smithy4s.Blob
import smithy4s.Hints
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.dynamic.model.Model
import smithy4s.json.Json
import smithy4s.schema.Schema
import util.chaining.*

object SchemaBuilder {

  def buildNewSchema(
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

  private def deconflict[T](
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

  private def multipleError(kindPlural: String) =
    new Exception(
      s"""Multiple $kindPlural found but none have the ${TranscoderSelect.id} trait.
         |Try adding @${TranscoderSelect.id} to the shape you want to use.""".stripMargin
    )

  private def multipleWithTraitError(kindPlural: String) =
    new Exception(
      s"""Multiple $kindPlural with the ${TranscoderSelect.id} trait found.
         |Choose one you want to use, and remove the trait from the others.""".stripMargin
    )

}
