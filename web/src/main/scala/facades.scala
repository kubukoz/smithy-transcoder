import scalajs.js

import scala.scalajs.js.annotation.JSGlobal

object facades {

  @js.native
  trait Cheerpj extends js.Object {}

  @js.native
  trait JSDumper extends js.Object {
    def dump(s: String): js.Promise[String] = js.native
  }

  object Cheerpj {

    @js.native
    @JSGlobal
    def cheerpjInit(): js.Promise[Unit] = js.native

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
