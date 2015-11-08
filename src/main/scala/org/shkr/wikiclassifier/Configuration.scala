package org.shkr.wikiclassifier

import akka.actor.ActorSystem
import akka.stream.{Materializer, ActorMaterializerSettings, ActorMaterializer}
import scala.concurrent.ExecutionContext

object Configuration {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val executionContext: ExecutionContext = actorSystem.dispatcher
  val settings: ActorMaterializerSettings = ActorMaterializerSettings(actorSystem)
  implicit val actorMetrializer: Materializer = ActorMaterializer(settings)

  //Size of the thread pool
  val parallelism: Int = 3

  //Error Analysis Output file
  val errorOutputFile: String = "output/error_analysis.txt"
}
