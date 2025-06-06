import scalajs.js

import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.annotation.JSImport

object facades {

  @js.native
  trait Cheerpj extends js.Object {}

  @js.native
  trait JSDumper extends js.Object {
    def dump(inputs: scalajs.js.Array[scalajs.js.Array[String]]): js.Promise[String] = js.native
    def format(s: String): js.Promise[String] = js.native
  }

  @js.native
  trait JavaException extends js.Object {
    def getMessage(): js.Promise[String] = js.native
  }

  object Cheerpj {

    @js.native
    @JSGlobal
    def cheerpjInit(options: scalajs.js.UndefOr[scalajs.js.Any]): js.Promise[Unit] = js.native

    @js.native
    @JSGlobal
    def cheerpjRunLibrary(classpath: String): js.Promise[Cheerpj] = js.native

    extension (c: Cheerpj) {

      def dumper: js.Promise[JSDumper] = c
        .asInstanceOf[js.Dynamic]
        .com
        .kubukoz
        .SmithyDump
        .asInstanceOf[js.Promise[JSDumper]]

    }

  }

}

@JSImport("lz-string", JSImport.Namespace)
@js.native
object lzstring extends js.Object {
  def compressToEncodedURIComponent(input: String): String = js.native
  def decompressFromEncodedURIComponent(compressed: String): String = js.native
}
