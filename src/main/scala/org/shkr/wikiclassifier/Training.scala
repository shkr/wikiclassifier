package org.shkr.wikiclassifier

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import akka.http.scaladsl.Http
import akka.stream.io.SynchronousFileSink
import akka.stream.scaladsl._
import akka.util.ByteString
import org.shkr.wikiclassifier.util.sampler.Select
import org.shkr.wikiclassifier.model._
import play.api.libs.json.Json
import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.util.{Failure, Success}

object Training {

  import org.shkr.wikiclassifier.Configuration._

  val positivePath: String = "training/positive/"
  val negativePath: String = "training/negative/"

  /** Source of Wikipedia Articles from fileSystem**/
  def fileArticle(fileFilter: File => Boolean, directoryPath: String*): Source[Article, Unit]={
    Source(() => directoryPath.flatMap(dirPath => Files.newDirectoryStream(Paths.get(dirPath)).iterator().asScala
      .map(_.toFile).filter(fileFilter)).toIterator)
      .mapAsyncUnordered[Option[Article]](parallelism)(e =>
      Future { Article.fromWikipediaHTMLFile(e) }
      ).collect({
      case Some(article) => article
    })
  }

  /** Classifies a Flow of Wikipedia Articles**/
  def classifyArticle(naiveBayesClassifier: NaiveBayesWikiClassifier,
                      handleEdgeCase: Boolean): Flow[Article, TwoCategoryResult, Unit] =
    Flow[Article]
      .mapAsyncUnordered[TwoCategoryResult](parallelism)(
        article => {
          Future { naiveBayesClassifier.classifyArticle(article, handleEdgeCase) }
        }
      )

  /** Trains a classifier from a Flow of Wikipedia Articles **/
  val trainClassifier: Sink[Article, Future[NaiveBayesWikiClassifier]] =
    Sink.fold(new NaiveBayesWikiClassifier(Disease, NotDisease))(
      (acc: NaiveBayesWikiClassifier, article: Article) => {
        acc.observation(article)
        acc
      }
    )

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


  def main(args: Array[String]): Unit={

    val startTimer: Long = System.currentTimeMillis()
    val handleEdgeCase: Boolean = args.contains("--edgecase")

    if(args.headOption.getOrElse("null").contentEquals("train")){
      fileArticle(f => true, positivePath, negativePath)
        .runWith(trainClassifier)
        .onComplete({
        case Success(value) => {

          println(Console.GREEN + s" Total No. of Positive Article = ${value.totalPositiveArticleObservation}" + Console.RESET)
          println(Console.GREEN + s" Total No. of Negative Article = ${value.totalNegativeArticleObservation}" + Console.RESET)
          println(s"training completed in ${(System.currentTimeMillis()-startTimer)/1000.0} seconds")

          /**TRAINING ERROR**/
          val trainingResults: Source[TwoCategoryResult, Unit] =
            fileArticle(f => true, positivePath, negativePath)
              .via(classifyArticle(value, handleEdgeCase))

          val trainingError = calculateError(trainingResults).run()
          trainingError._1.onComplete({
            case Success(trainingErrorTable) => {
              println("training error")
              println(Console.YELLOW + trainingErrorTable.toString() + Console.RESET)
              println("completed training error")
              actorSystem.terminate()
            }
            case Failure(exception) => {
              println(exception.getMessage)
              actorSystem.terminate()
            }
          })
        }
        case Failure(throwable) => {
          println(Console.RED + throwable.getMessage + "\n" + throwable.getStackTrace.mkString("\n") + Console.RESET)
          actorSystem.terminate()
        }
      })
    } else if(args.headOption.getOrElse("null")=="cv"){

      /** Cross-Validation **/
      val holdOutSize: Int = args.drop(1).headOption.getOrElse("500").toInt
      val allLabeledFiles: List[File] = List(positivePath, negativePath)
        .flatMap(dirPath => Files.newDirectoryStream(Paths.get(dirPath)).iterator().asScala.map(_.toFile))
      val testSet: List[File] = Select.randomSelect(holdOutSize, allLabeledFiles)
      def isTrainingFile(f: File): Boolean = !testSet.contains(f)
      def isTestFile(f: File): Boolean = testSet.contains(f)

      fileArticle(isTrainingFile, positivePath, negativePath)
        .runWith(trainClassifier)
        .onComplete({
        case Success(value) => {

          println(Console.GREEN + s" Total No. of Positive Article = ${value.totalPositiveArticleObservation}" + Console.RESET)
          println(Console.GREEN + s" Total No. of Negative Article = ${value.totalNegativeArticleObservation}" + Console.RESET)
          println(s"training completed in ${(System.currentTimeMillis()-startTimer)/1000.0} seconds")

          /**TEST ERROR**/
          val testResults: Source[TwoCategoryResult, Unit] =
            fileArticle(isTestFile, positivePath, negativePath)
              .via(classifyArticle(value, handleEdgeCase))

          val testError = calculateError(testResults).run()
          testError._1.onComplete({
            case Success(testErrorTable) => {
              println(s"cross-validation with hold-out set size =$holdOutSize")
              println(Console.YELLOW + testErrorTable.toString() + Console.RESET)
              println("completed cross-validation")
              actorSystem.terminate()
            }
            case Failure(exception) => {
              println(exception.getMessage)
              actorSystem.terminate()
            }
          })
        }
        case Failure(throwable) => {
          println(Console.RED + throwable.getMessage + "\n" + throwable.getStackTrace.mkString("\n") + Console.RESET)
          actorSystem.terminate()
        }
      })
    } else if(args.headOption.getOrElse("null")=="classify"){
      if(args.drop(1).headOption.isDefined) {
        fileArticle(f => true, positivePath, negativePath)
          .runWith(trainClassifier)
          .onComplete({
          case Success(value) => {

            println(Console.GREEN + s" Total No. of Positive Article = ${value.totalPositiveArticleObservation}" + Console.RESET)
            println(Console.GREEN + s" Total No. of Negative Article = ${value.totalNegativeArticleObservation}" + Console.RESET)
            println(s"training completed in ${(System.currentTimeMillis()-startTimer)/1000.0} seconds")

            /** Classification of Online Articles **/
            WikipediaAPI.onlineArticle(args.drop(1).head.split(","):_*)
              .via(classifyArticle(value, handleEdgeCase))
              .runWith(writeResultToFile)
              .onComplete {
              case Success(_) => {
                println(Console.GREEN + s" Successfully classified and written output to file" + Console.RESET)
                Http().shutdownAllConnectionPools().onComplete({
                  case Success(_) =>
                    println("Successfuly shutdown Http Host level Connection")
                    actorSystem.terminate()

                  case Failure(_) =>
                    println("Failed in properly shutting down Http Host level Connection")
                    actorSystem.terminate()
                })
              }
              case Failure(throwable) => {

                println(Console.RED + throwable.getMessage + "\n" + throwable.getStackTrace.mkString("\n") + Console.RESET)

                Http().shutdownAllConnectionPools().onComplete({
                  case Success(_) =>
                    println("Successfuly shutdown Http Host level Connection")
                    actorSystem.terminate()

                  case Failure(_) =>
                    println("Failed in properly shutting down Http Host level Connection")
                    actorSystem.terminate()
                })
              }
            }
          }
          case Failure(throwable) => {
            println(Console.RED + throwable.getMessage + "\n" + throwable.getStackTrace.mkString("\n") + Console.RESET)
            actorSystem.terminate()
          }
        })

      } else {
        println(Console.RED + "second argument must be a comma separated list of wiki page names")
        println("Example :")
        println("sbt 'runMain org.shkr.wikiclassifier.classifier.Training classify Hurwitz_matrix,Polymicrogyria,HIV/AIDS,Baseball'")
        sys.exit(-1)
      }
    }
  }
}

