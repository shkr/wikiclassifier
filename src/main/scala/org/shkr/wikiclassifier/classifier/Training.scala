package org.shkr.wikiclassifier.classifier

import java.io.File
import java.nio.file.{Files, Paths}
import akka.stream.io.SynchronousFileSink
import akka.stream.scaladsl._
import akka.util.ByteString
import org.shkr.wikiclassifier.model._
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

  val sinkWordProbabilityOverSentences: Sink[Article, Future[NaiveBayesWikiClassifier]] =
    Sink.fold(new NaiveBayesWikiClassifier(Disease, NotDisease))(
      (acc: NaiveBayesWikiClassifier, article: Article) => {
        acc.observation(article)
        acc
      }
    )

  def classifyArticle(naiveBayesClassifier: NaiveBayesWikiClassifier): Flow[Article, TwoCategoryResult, Unit] =
    Flow[Article]
      .mapAsyncUnordered[TwoCategoryResult](parallelism)(
      article => {
        Future { naiveBayesClassifier.classifyArticle(article) }
      }
    )

  def wordProbability(directoryPath: String*): Future[NaiveBayesWikiClassifier] =
    article(directoryPath:_*)
    .runWith(sinkWordProbabilityOverSentences)

  def calculateError(naiveBayesClassifier: NaiveBayesWikiClassifier, dirPath: String*) =
    FlowGraph.closed(sinkErrorTable, sinkWriteResult)((_, _)) { implicit builder =>
      (errorTable, resultFile) => {

        import FlowGraph.Implicits._

        val results = article(positivePath, negativePath).via(classifyArticle(naiveBayesClassifier))
        val broadcast = builder.add(Broadcast[TwoCategoryResult](2))

        results ~> broadcast.in
        broadcast.out(0) ~> errorTable.inlet
        broadcast.out(1) ~> resultFile.inlet
      }
  }

  val sinkErrorTable: Sink[TwoCategoryResult, Future[StatisticalErrorTable]] = Sink.fold[StatisticalErrorTable, TwoCategoryResult](new StatisticalErrorTable(Disease, NotDisease))(
    (errorTable, twoCategoryResult) => {
      errorTable.update(twoCategoryResult.article.category, twoCategoryResult.classification)
      errorTable
    }
  )

  val sinkWriteFile = SynchronousFileSink(new File("classification.txt"), false)

  val sinkWriteResult: Sink[TwoCategoryResult, Unit] = Flow[TwoCategoryResult]
    .mapAsyncUnordered[ByteString](parallelism)(e =>
      Future { ByteString.fromString(s"Article: ${e.article.title} " +
      s"category: ${e.article.category} classification: ${e.classification} " +
        s"positiveScore: ${e.positiveScore} negativeScore: ${e.negativeScore}\n"
      )
    }
  ).to(sinkWriteFile)

  def main(args: Array[String]): Unit={

    val startTimer: Long = System.currentTimeMillis()

    wordProbability(positivePath, negativePath).onComplete({
      case Success(value) => {

      println(Console.GREEN + s" Total No. of Positive Article = ${value.totalPositiveArticleObservation}" + Console.RESET)
      println(Console.GREEN + s" Total No. of Negative Article = ${value.totalNegativeArticleObservation}" + Console.RESET)
      println(Console.GREEN + s" Total No. of Positive Sentences = ${value.totalPositiveSentenceObservation}" + Console.RESET)
      println(Console.GREEN + s" Total No. of Negative Sentences = ${value.totalNegativeSentenceObservation}" + Console.RESET)
      println(s"training completed in ${(System.currentTimeMillis()-startTimer)/1000.0} seconds")

//        val testArticles = Files.newDirectoryStream(Paths.get(negativePath)).iterator().asScala.map(_.toFile)
//          .filter(_.getName=="9194")
//          .map(f => Article.fromWikipediaHTML(f))
//
//        testArticles.collect({case Some(article) => article}).foreach(article => {
//          println(s"classifying ${article.title}")
//          value.classifyArticle(article, true)
//        })
//        actorSystem.terminate()


        /**TRAINING ERROR**/
        val trainingError = calculateError(value, positivePath, negativePath).run()
        trainingError._1.onComplete({
          case Success(errorTable) => {
            println("training error")
            println(errorTable.toString)
            println("completed training error")
            actorSystem.terminate()
          }
          case Failure(exception) => {
            println(exception.getMessage)
            actorSystem.terminate()
          }
        })

        /** TEST ERROR **/
      }
      case Failure(throwable) => {
        println(Console.RED + throwable.getMessage + "\n" + throwable.getStackTrace.mkString("\n") + Console.RESET)
        actorSystem.terminate()
      }
    })
  }
}
