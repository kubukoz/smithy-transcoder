package com.kubukoz

import cats.effect.IO
import cats.effect.IOApp
import com.comcast.ip4s.*
import org.http4s.HttpRoutes
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
              Ok(SmithyDump.dump(input))
            }
          }
          .orNotFound
      )
      .build
      .useForever

}
