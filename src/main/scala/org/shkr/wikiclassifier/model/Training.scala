package org.shkr.wikiclassifier.reader

import java.io.File
import java.nio.file.{Files, Paths}
import akka.stream.scaladsl._
import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.collection.mutable
object Training {

  import org.shkr.wikiclassifier.Configuration._

  val parallelism: Int = 3

  val directoryPath: String = "training/positive/"

  val article: Source[Article, Unit]={
    Source(() => Files.newDirectoryStream(Paths.get(directoryPath)).iterator().asScala.map(_.toFile).map(file => {
      try {
        scala.io.Source.fromFile(file).getLines().mkString
      } catch {
        case e: java.nio.charset.MalformedInputException => println(Console.RED + s"skipped file: ${file.getName}" + Console.RESET)
          None
      }
    })).collect[Article]({
      case html: String => Article.fromWikipediaHTML(html)
    })
  }

  val sentences: Flow[Article, IndexedSeq[String], Unit]={
    Flow[Article]
      .mapAsyncUnordered[IndexedSeq[IndexedSeq[String]]](parallelism)(article => Future{article.introduction.sentences})
      .mapConcat[IndexedSeq[String]](_.toList)
  }

  val wordSet: Flow[IndexedSeq[String], Set[String], Unit]={
    Flow[IndexedSeq[String]]
      .mapAsyncUnordered[Set[String]](parallelism)(seq => {
      Future { seq.toSet }
    })
  }

  val sinkTotalSentences: Sink[Set[String], Future[Int]] = Sink.fold(0)((acc: Int, set: Set[String]) => acc + 1)

  val sinkWordFrequencyOverSentences: Sink[Set[String], Future[mutable.HashMap[String, Int]]] =
    Sink.fold({new mutable.HashMap[String, Int]()  { override def default(key: String) = 0 }})(
      (acc: mutable.HashMap[String, Int], set: Set[String]) => {
        set.foreach(w => acc.update(w, acc(w) + 1))
        acc
      }
    )

  val wordProbability = FlowGraph.closed(sinkTotalSentences, sinkWordFrequencyOverSentences)((_, _)){ implicit builder =>
    (totalSentences, wordFrequency) =>
      import FlowGraph.Implicits._
      val wordObservationSource = builder.add(article.via(sentences).via(wordSet))
      val broadcast = builder.add(Broadcast[Set[String]](2))
      wordObservationSource ~> broadcast.in
      broadcast.out(0) ~> totalSentences
      broadcast.out(1) ~> wordFrequency
  }

  def main(args: Array[String]): Unit={

    val output = wordProbability.run()

    val result: Future[(Int, mutable.HashMap[String, Int])] = for {
      totalSentences <- output._1
      wordFrequency <- output._2
    } yield (totalSentences, wordFrequency)

    result.onComplete({
      case Success(value) => {
        println(Console.GREEN + s" Total No. of Sentences = ${value._1}" + Console.RESET)
        value._2.take(10).foreach(w => println(Console.GREEN + s"Sentences With Word (${w._1})= ${w._2.toDouble/value._1}" + Console.RESET))
        actorSystem.terminate()
      }
      case Failure(throwable) => {
        println(Console.RED + throwable.getMessage + "\n" + throwable.getStackTrace.mkString("\n") + Console.RESET)
        actorSystem.terminate()
      }
    })
  }
}
