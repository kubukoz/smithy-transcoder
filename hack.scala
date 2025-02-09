package org.http4s.ember.hack

import cats.effect.IO
import org.http4s.Request

object EncoderHack {

  def requestToString(req: Request[IO]): IO[String] =
    org
      .http4s
      .ember
      .core
      .Encoder
      .reqToBytes(req)
      .through(fs2.text.utf8.decode[IO])
      .compile
      .string

  def requestFromString(string: String): IO[Request[IO]] = org
    .http4s
    .ember
    .core
    .Parser
    .Request
    .parser(1024)(string.getBytes(), IO.stub)
    .map(_._1)

}
