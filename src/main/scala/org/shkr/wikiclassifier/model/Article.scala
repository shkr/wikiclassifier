package org.shkr.wikiclassifier.model

import java.io.File
import epic.preprocess.MLSentenceSegmenter
import scala.util.matching.Regex

case class Article(title: String, introduction: ArticleText, contentTypes: List[String], references: Set[String], label: Category)

object Article {

  def empty: Article = Article("null", ArticleText.empty, List.empty[String], Set.empty[String], Undefined)

  object HTMLTagParser {

    val h1: Regex = "<h1.+?>(.+?)</h1>".r
    val p: Regex = "<p>(.+?)</p>".r
    val bracket: Regex = "<.+?>|\\[.+?]".r
    val tocNumber: Regex = """<span class="tocnumber">([0-9\\.]+)</span>""".r
    val tocText: Regex = """<span class="toctext">(.+?)</span>""".r
    val referencedJournal: Regex = """<span class="citation journal">.+?<i>(.+?)</i>""".r
  }

  object EpicNLP {

    val sentenceSplitter = MLSentenceSegmenter.bundled().get
    val tokenizer = new epic.preprocess.TreebankTokenizer()
  }

  object NLPCollection {
    val letters: Array[Char] = "abcdefghijklmnopqrstuvwxyz".toCharArray
  }

  def fullContentTypeNames(l: Map[String, String]): List[String]={
    l.map(e => {
      val key = e._1.split("\\.").dropRight(1).mkString
      l.get(key).collect({case prefix => prefix + " " }).getOrElse("") + e._2
    }).toList
  }

  def cleanText(item: String): String={
    item.replaceAll("&amp;", "and")
        .replaceAll("<a .+?>", " ")
        .replaceAll("</a>", " ")
        .replaceAll("<.+?>|\\[.+?]", " ")
        .replaceAll("\\n+", " ")
        .replaceAll("\\s+", " ")
  }

  def readClassification(dirName: String): Category={
    dirName match {
      case "positive" => Disease
      case "negative" => NotDisease
      case _ => Undefined
    }
  }

  def fromWikipediaHTML(file: File): Option[Article]={

    val someHtml: Option[(Category, String)] =
      try {
        Some(readClassification(file.getParentFile.getName), scala.io.Source.fromFile(file).getLines().mkString)
      } catch {
        case e: java.nio.charset.MalformedInputException =>
          println(Console.RED + s"invalid wikipedia html file: ${file.getName}" + Console.RESET)
        None
      }

    someHtml.collect({

      case (classification, html) => {
        import HTMLTagParser._
        import NLPCollection._
        import EpicNLP._

        val title: String = h1.findFirstMatchIn(html).map(subStringMatch =>  subStringMatch.group(1))
          .map(text => cleanText(text)).getOrElse("null")

        val introduction: ArticleText = {

          val text = p.findAllMatchIn(html).map(subStringMatch => subStringMatch.group(1))
            .map(text => cleanText(text)).mkString

          ArticleText(sentenceSplitter(text)
            .map(tokenizer)
            .map(wordSeq => wordSeq.map(_.toLowerCase).filter(w => w.length>0 && w.exists(letters.contains))))
        }

        val contentTypes: List[String] = {

          val text = tocText.findAllMatchIn(html).map(subStringMatch => subStringMatch.group(1))
            .map(text => cleanText(text)).toList

          val number = tocNumber.findAllMatchIn(html).map(subStringMatch => subStringMatch.group(1)).toList

          text.size==number.size match {
            case true => fullContentTypeNames(number.zip(text).toMap)
            case false => text
          }
        }

        val references: Set[String] = referencedJournal.findAllMatchIn(html).map(subStringMatch => subStringMatch.group(1))
          .map(text => cleanText(text)).toSet

        Article(title, introduction, contentTypes, references, classification)
      }
    })
  }
}