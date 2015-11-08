package org.shkr.wikiclassifier.model

import java.io.File
import epic.preprocess.MLSentenceSegmenter
import play.api.libs.json.{JsValue, Json}
import scala.util.matching.Regex

/**To Store Information from A Wikipedia HTML file**/
case class Article(title: List[String],
                   introduction: ArticleText,
                   contentTypes: ContentType,
                   html: String,
                   category: Category){

  def asJsValue(result: TwoCategoryResult = TwoCategoryResult.empty): JsValue={

    val basic: Map[String, JsValue] = Map("title" -> Json.toJson(title.mkString(" ")),
                                          "category" -> Json.toJson(result.classification.toString))

    /** Extracted here instead of in the Classifier because it does not aid in classification **/
    val diseaseRelated: Map[String, JsValue] = {
      result.classification match {
        case Disease => Map(
          "diseases_information" -> Json.toJson(
              Article.getDiseaseInformation(this).map(kV => (kV._1, Json.toJson(kV._2.toIterable)))
          )
        )
        case _ => Map.empty[String, JsValue]
      }
    }

    val otherInfo: Map[String, JsValue] = result.info

    Json.toJson(basic ++ diseaseRelated ++ otherInfo)
  }

  override def toString: String={
    asJsValue().toString()
  }
}

object Article {

  def empty: Article = Article(List.empty[String], ArticleText.empty, ContentType.empty, "null", Undefined)

  object HTMLTagParser {

    val h1: Regex = "<h1.+?>(.+?)</h1>".r
    val p: Regex = "<p>(.+?)</p>".r
    val bracket: Regex = "<.+?>|\\[.+?]".r
    val tocNumber: Regex = """<span class="tocnumber">([0-9\\.]+)</span>""".r
    val tocText: Regex = """<span class="toctext">(.+?)<""".r
    def fetchContentText(level: Int, id: String): Regex = {
      s"""(?s)<h${2+level}><span class="mw-headline" id="$id">(.+?)<h[0-9]+>""".r
    }
    val contentText: Regex = {
      "(?<=<p>|<li>)(.+?)(?=</p>|</li>)".r
    }
    val infoBox: Regex = "(?s)(<table class=\"infobox.+?</table>)".r
  }

  object EpicNLP {

    val sentenceSplitter = MLSentenceSegmenter.bundled().get
    val tokenizer = new epic.preprocess.TreebankTokenizer()
  }

  object NLPCollection {
    val letters: Array[Char] = "abcdefghijklmnopqrstuvwxyz".toCharArray
  }

  def cleanText(item: String): String={
    item.replaceAll("&amp;", "and")
        .replaceAll("<a .+?>", " ")
        .replaceAll("</a>", " ")
        .replaceAll("<.+?>|\\[.+?]", " ")
        .replaceAll("\\n+", " ")
        .replaceAll("\\s+", " ").trim
  }

  def findCategory(dirName: String): Category={
    dirName match {
      case "positive" => Disease
      case "negative" => NotDisease
      case _ => Undefined
    }
  }

  def fromWikipediaHTMLFile(file: File): Option[Article]={

    val someHtml: Option[(Category, String)] =
      try {
        Some(findCategory(file.getParentFile.getName), scala.io.Source.fromFile(file).getLines().mkString)
      } catch {
        case e: java.nio.charset.MalformedInputException =>
          println(Console.RED + s"invalid wikipedia html file: ${file.getName}" + Console.RESET)
          None
      }

    someHtml.collect({
      case (classification, html) => fromWikipediaHTML(classification, html)
    })
  }

  def fromWikipediaHTML(classification: Category, html: String): Article={

      import HTMLTagParser._
      import NLPCollection._
      import EpicNLP._

      val title: List[String] = h1.findFirstMatchIn(html).map(subStringMatch =>  subStringMatch.group(1))
        .map(text => cleanText(text)).getOrElse("").split(" ")
        .filter(w => w.length>0 && w.exists(letters.contains))
        .toList

      val introduction: ArticleText = {

        val text = p.findAllMatchIn(html).map(subStringMatch => subStringMatch.group(1))
          .map(text => cleanText(text)).mkString

        ArticleText(sentenceSplitter(text)
          .map(tokenizer)
          .map(wordSeq => wordSeq.map(_.toLowerCase).filter(w => w.length>0 && w.exists(letters.contains))))
      }

      val contentTypes: ContentType = {
        val contentTypeNames: List[String] = tocText.findAllMatchIn(html).map(subStringMatch => subStringMatch.group(1)).toList
        val contentTypeSentence: List[Array[String]] = contentTypeNames
          .map(text => cleanText(text).split(" ").filter(w => w.length>0 && w.exists(letters.contains)))
        val contentTypeIndex = tocNumber.findAllMatchIn(html).map(subStringMatch => subStringMatch.group(1)).toList

        contentTypeNames.size==contentTypeIndex.size match {
          case true => {
            val map: Map[String, String] = contentTypeIndex.zip(contentTypeNames).toMap
            ContentType(contentTypeSentence, map)
          }
          case false => ContentType(contentTypeSentence, Map.empty[String, String])
        }
      }

      Article(title, introduction, contentTypes, html, classification)
  }

  def getDiseaseInformation(article: Article): Map[String, Iterator[String]]={

    import Article.HTMLTagParser._

    article.contentTypes.map
      .filter(_._2!="References")
      .map(indexWithName => {

      val content =  fetchContentText(indexWithName._1.count(_=='.'), indexWithName._2).findFirstMatchIn(article.html)
      (indexWithName._2, content.map(t => contentText.findAllMatchIn(t.group(1))
        .flatMap(t => EpicNLP.sentenceSplitter(cleanText(t.group(1))))))
    }).collect({
      case (heading, Some(text)) => (heading, text)
    })
  }
}