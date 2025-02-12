package com.kubukoz

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder

object SmithyDumpApi extends IOApp.Simple {

  def run: IO[Unit] =
    EmberServerBuilder
      .default[IO]
      .withHost(host"0.0.0.0")
      .withHttpApp(
        HttpRoutes
          .of[IO] { case req @ POST -> Root / "api" / "dump" =>
            req.bodyText.compile.string.flatMap { input =>
              IO(SmithyDump.dump(input)).attempt.flatMap {
                case Right(v) => Ok(v)
                case Left(e)  => InternalServerError(e.getMessage())
              }
            }
          }
          .orNotFound
      )
      .withErrorHandler { case e => IO.consoleForIO.printStackTrace(e) *> IO.raiseError(e) }
      .build
      .evalTap(srv => IO.println(show"Running at ${srv.baseUri}"))
      .useForever

}
