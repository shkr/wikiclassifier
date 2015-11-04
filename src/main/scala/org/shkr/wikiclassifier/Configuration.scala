package org.shkr.wikiclassifier

import akka.actor.ActorSystem
import akka.stream.{Materializer, ActorMaterializerSettings, ActorMaterializer}
import com.typesafe.config.ConfigFactory

import scala.concurrent.ExecutionContext

object Configuration {

  val config = ConfigFactory.parseString(
    """
        actor {
            # FQCN of the ActorRefProvider to be used; the below is the built-in default,
            # another one is akka.remote.RemoteActorRefProvider in the akka-remote bundle.
            provider = "akka.actor.LocalActorRefProvider"
        }
    """.stripMargin)

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val executionContext: ExecutionContext = actorSystem.dispatcher
  val settings: ActorMaterializerSettings = ActorMaterializerSettings(actorSystem)
  implicit val actorMetrializer: Materializer = ActorMaterializer(settings)
}
