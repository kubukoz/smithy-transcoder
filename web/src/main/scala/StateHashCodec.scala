import smithy4s.Blob
import smithy4s.json.Json
import smithytranscoder.InputData

object StateHashCodec {

  def encode(state: InputData): String = lzstring.compressToEncodedURIComponent(
    Json.writeBlob(state).toUTF8String
  )

  def decode(compressed: String): HashState =

    compressed.stripPrefix("#") match {
      case "" => HashState.Missing
      case stripped =>
        Option(
          lzstring.decompressFromEncodedURIComponent(stripped)
        ).toRight(new IllegalArgumentException(s"Invalid compressed input: $stripped"))
          .map(Blob(_))
          .flatMap(Json.read[InputData])
          .fold(
            HashState.Invalid(_),
            HashState.Valid(_),
          )
    }

}

enum HashState {
  case Valid(data: InputData)
  case Invalid(e: Throwable)
  case Missing
}
