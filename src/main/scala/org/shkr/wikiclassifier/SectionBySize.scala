package org.shkr.wikiclassifier

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}
import akka.stream.io.SynchronousFileSink
import akka.stream.scaladsl._
import akka.util.ByteString
import org.shkr.wikiclassifier.model._
import play.api.libs.json.Json
import scala.concurrent.Future
/**
 * Classify SectionName using the length of the text
 */
object SectionBySize {


  import org.shkr.wikiclassifier.Configuration._

  val positivePath: String = "training/positive/"

  /** Flow of SectionName and Number of Segments from all Disease articles **/
  def sectionNameWithLength(fileFilter: File => Boolean, directoryPath: String*): Source[(String, Int), Unit]= {
    Training.fileArticle(fileFilter, directoryPath: _*)
      .mapConcat[(String, Int)](article => Article.getDiseaseInformation(article)
      .map(item => (item._1, item._2.size)))
  }

  /** Calculates error table from a Flow of TwoCategoryResults**/
  val errorTable: Sink[TwoCategoryResult, Future[StatisticalErrorTable]] =
    Sink.fold[StatisticalErrorTable, TwoCategoryResult](new StatisticalErrorTable(Disease, NotDisease))(
      (errorTable, twoCategoryResult) => {
        errorTable.update(twoCategoryResult.article.category, twoCategoryResult.classification)
        errorTable
      }
    )

  /** FlowGraph to write result to file and create error table from a Flow of TwoCategoryResults in one iteration**/
  def calculateError(results: Source[TwoCategoryResult, Unit]): RunnableGraph[(Future[StatisticalErrorTable], Unit)] =
    FlowGraph.closed(errorTable, appendResultInfoToFile)((_, _)) { implicit builder =>
      (errorTable, resultFile) => {

        import FlowGraph.Implicits._

        val broadcast = builder.add(Broadcast[TwoCategoryResult](2))

        results ~> broadcast.in
        broadcast.out(0) ~> errorTable.inlet
        broadcast.out(1) ~> resultFile.inlet
      }
    }

  /** Writes extracted info with in a JSON new file and adds result classification in the fileName
    * from a Flow of Results
    */
  val writeResultToFile: Sink[TwoCategoryResult, Future[Unit]] = Sink.foreach[TwoCategoryResult](
    result => {

      Files.write(
        Paths.get("output/"+result.article.title.mkString("_") + "_" + result.classification +".json"),
        Json.prettyPrint(result.article.asJsValue(result)).getBytes(StandardCharsets.UTF_8))
    }
  )

  /** Appends Result Info to error_analysis.txt from a Flow of Results**/
  val appendResultInfoToFile: Sink[TwoCategoryResult, Unit] = Flow[TwoCategoryResult]
    .mapAsyncUnordered[ByteString](parallelism)(e =>
    Future { ByteString.fromString(
      s"Article: ${e.article.title.mkString(" ")} //" +
        s"Category: ${e.article.category} // " +
        s"Classification: ${e.classification} //" +
        s"PositiveScore: ${e.positiveScore} // " +
        s"NegativeScore: ${e.negativeScore}\n") }
    ).to(SynchronousFileSink(new File(errorOutputFile), false))
}
