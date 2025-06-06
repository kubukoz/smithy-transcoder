import smithy4s.ShapeId
import smithy4s.ShapeTag
import smithy4s.schema.Schema

case class TranscoderSelect()

object TranscoderSelect extends ShapeTag.Companion[TranscoderSelect] {
  val id: ShapeId = ShapeId("st", "select")

  val schema: Schema[TranscoderSelect] = Schema.constant(TranscoderSelect())
}
