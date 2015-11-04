package org.shkr.wikiclassifier.classifier

import java.nio.file.{Files, Paths}
import akka.stream.scaladsl._
import org.shkr.wikiclassifier.model.{NotDisease, Disease, Category, Article}
import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.util.{Failure, Success}
object Training {

  import org.shkr.wikiclassifier.Configuration._

  val parallelism: Int = 3

  val positivePath: String = "training/positive/"
  val negativePath: String = "training/negative/"

  def article(directoryPath: String*): Source[Article, Unit]={
    Source(() => directoryPath.flatMap(dirPath => Files.newDirectoryStream(Paths.get(dirPath)).iterator().asScala.map(_.toFile)).toIterator)
      .mapAsyncUnordered[Option[Article]](parallelism)(e => 
      Future { Article.fromWikipediaHTML(e) }
    ).collect({
      case Some(article) => article
    })
  }

  val wordSetInSentence: Flow[Article, (Category, Set[String]), Unit]={
    Flow[Article]
      .mapAsyncUnordered[(Category, IndexedSeq[IndexedSeq[String]])](parallelism)(article => Future {
        (article.label, article.introduction.sentences)
      })
      .mapConcat[(Category, Set[String])](e => e._2.map(sentence => (e._1, sentence.toSet)).toList)
  }

  val sinkWordProbabilityOverSentences: Sink[(Category, Set[String]), Future[BinaryClassifier]] =
    Sink.fold(new BinaryClassifier(Disease, NotDisease))(
      (acc: BinaryClassifier, item: (Category, Set[String])) => {
        acc.observation(item._1, item._2)
        acc
      }
    )

  def wordProbability(directoryPath: String*): Future[BinaryClassifier] = article(directoryPath:_*)
    .via(wordSetInSentence)
    .runWith(sinkWordProbabilityOverSentences)

  def main(args: Array[String]): Unit={

    val startTimer: Long = System.currentTimeMillis()

    wordProbability(positivePath, negativePath).onComplete({
      case Success(value) => {
        println(Console.GREEN + s" Total No. of Positive Sentences = ${value.totalPositiveObservation}" + Console.RESET)
        println(Console.GREEN + s" Total No. of Negative Sentences = ${value.totalNegativeObservation}" + Console.RESET)
        Files.write(Paths.get("result.txt"),
          value.distribution.map(w => s"P(${w._1} in sentence) = ${w._2}").mkString("\n").getBytes()
        )
        actorSystem.terminate()
        println(s"training completed in ${(System.currentTimeMillis()-startTimer)/1000.0} seconds")
      }
      case Failure(throwable) => {
        println(Console.RED + throwable.getMessage + "\n" + throwable.getStackTrace.mkString("\n") + Console.RESET)
        actorSystem.terminate()
      }
    })
  }
}
