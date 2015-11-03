import org.shkr.wikiclassifier.reader.Article
import scala.io.Source

val html = Source.fromFile("/Users/shashank/Documents/personal/repositories/wikiclassifier/training/positive/Abdominal_aortic_aneurysm").mkString
val a = Article.fromWikipediaHTML(html)
a.introduction.sentences.foreach(println)
a.contentTypes.foreach(println)
a.references.foreach(println)
