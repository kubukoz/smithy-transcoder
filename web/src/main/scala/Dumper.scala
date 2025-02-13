import calico.html.io.*
import calico.html.io.given
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.effect.std.Mutex
import cats.syntax.all.*
import facades.JavaException
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import monocle.syntax.all.*
import org.scalajs.dom.Fetch
import org.scalajs.dom.HttpMethod
import org.scalajs.dom.RequestInit

import scala.scalajs.js.JavaScriptException

type DumperSig = Signal[IO, Dumper.State]

trait Dumper {
  def dump(s: String): IO[String]
}

object Dumper {

  enum State {
    case Init
    case LoadingCheerp(progress: Int, total: Int)
    case LoadingLibrary
    case Loaded(dumper: Dumper)
  }

  def progressBar(signal: Signal[IO, Dumper.State]): Resource[IO, HtmlElement[IO]] = div(
    styleAttr <-- signal
      .map {
        case State.Loaded(_) => "display: none"
        case _               => "display: flex; gap: 10px"
      },
    signal.map {
      case State.Init                => "Initializing..."
      case State.LoadingCheerp(_, _) => "Loading JVM..."
      case State.LoadingLibrary      => "Loading library..."
      case _: State.Loaded           => "Loaded"
    },
    progressTag(
      maxAttr <--
        signal.map {
          case State.LoadingCheerp(_, total) => total.show.some
          case State.LoadingLibrary          => "100".some
          case _                             => None
        },
      value <-- signal.map {
        case State.Init                       => None
        case State.LoadingCheerp(progress, _) => progress.show.some
        case State.LoadingLibrary             => "99".some
        case State.Loaded(_)                  => "100".some
      },
    ),
  )

  def inBrowser: Resource[IO, Signal[IO, State]] =
    (
      SignallingRef[IO]
        .of[State](State.Init)
        .toResource,
      Dispatcher.sequential[IO],
    )
      .flatMapN { (state, dispatchito) =>
        val preload = IO.fromPromise(
          IO(
            facades
              .Cheerpj
              .cheerpjInit {
                scalajs
                  .js
                  .Dynamic
                  .literal(
                    preloadResources = scalajs
                      .js
                      .JSON
                      .parse(
                        // result of cjGetRuntimeResources (you can call it from the console, it's a global)
                        """{"/lt/8/jre/lib/javaws.jar":[0,131072,1441792,1703936],"/lt/8/jre/lib/rt.jar":[0,131072,10223616,12451840,15204352,15335424,15466496,15597568,17694720,17825792,18350080,18612224,19005440,19136512,20840448,21233664,21364736,21757952,22020096,26869760],"/lt/8/jre/lib/resources.jar":[0,131072,917504,1179648],"/lt/8/jre/lib/charsets.jar":[0,131072,1703936,1835008],"/lt/8/jre/lib/jce.jar":[0,131072],"/lt/8/jre/lib/jsse.jar":[0,131072,786432,917504],"/lt/8/lib/ext/meta-index":[0,131072],"/lt/etc/passwd":[0,131072],"/lt/8/jre/lib/cheerpj-awt.jar":[0,131072],"/lt/8/lib/logging.properties":[0,131072],"/lt/etc/localtime":[],"/lt/8/lib/ext":[],"/lt/8/lib/ext/index.list":[],"/lt/8/jre/lib/meta-index":[0,131072],"/lt/8/lib/ext/localedata.jar":[],"/lt/8/lib/ext/sunjce_provider.jar":[],"/lt/8/jre/lib":[]}"""
                      ),
                    preloadProgress = { (preloadDone: Int, preloadTotal: Int) =>
                      val target = State.LoadingCheerp(preloadDone - 1, preloadTotal)

                      val bump = state.update {
                        case State.Init | _: State.LoadingCheerp => target
                        // just in case of race conditions. We don't wanna be stuck in loading
                        case s => s
                      }

                      dispatchito.unsafeRunAndForget(bump)
                    }: scalajs.js.Function2[Int, Int, Unit],
                  )

              }
          )
        )

        val loadLib = IO
          .fromPromise(IO(facades.Cheerpj.cheerpjRunLibrary("/app/SmithyDump.jar")))
          .flatMap { c =>
            IO.fromPromise(IO(c.dumper))
          }

        val process =
          preload
            *>
              loadLib
                .flatMap { underlying =>
                  Mutex[IO].map { m =>
                    new Dumper {
                      def dump(s: String): IO[String] = m.lock.surround {
                        IO.fromPromise(IO(underlying.dump(s)))
                          .recoverWith { case JavaScriptException(e) =>
                            IO.fromPromise(
                              IO(
                                e.asInstanceOf[JavaException].getMessage()
                              )
                            ).map(new Exception(_))
                              .flatMap(IO.raiseError)
                          }
                      }
                    }
                  }
                }
                .flatTap(_ => state.set(State.LoadingLibrary))
                // just to finish loading
                .flatTap(_.dump("").attempt)
                .flatTap(dumper => state.set(State.Loaded(dumper)))

        process.background.as(state)
      }

  def remote: Dumper =
    new {
      // http POST /api/dump s=$s
      def dump(s: String): IO[String] = IO
        .fromPromise {
          IO {
            Fetch.fetch(
              "/api/dump",
              new RequestInit {
                this.method = HttpMethod.POST
                this.body = s
              },
            )
          }
        }
        .flatMap(r =>
          IO.fromPromise(IO(r.text())).flatMap {
            case body if r.status == 200 => IO.pure(body)
            case body                    => IO.raiseError(new Exception(body))
          }
        )
    }

}
