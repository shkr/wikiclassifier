package org.shkr.wikiclassifier.model

import java.net.URLEncoder
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpResponse, HttpRequest}
import akka.stream.scaladsl.Source
import org.shkr.wikiclassifier.Configuration._
import scala.concurrent.duration._
import scala.concurrent.Future

object WikipediaAPI {

  val httpTimeout = 5000.millis

  def wikiPage(name: String): Future[HttpResponse] = {
    val request = HttpRequest(uri = s"https://en.wikipedia.org/wiki/${uriEncode(name)}")
    Http().singleRequest(request)
  }

  def fetchHTML(response: HttpResponse): Future[String]={
    response.entity.toStrict(httpTimeout).map[String](entity => entity.data.utf8String)
  }

  def onlineArticle(wikiName: String*): Source[Article, Unit]={
    Source(() => wikiName.toIterator)
      .mapAsyncUnordered[HttpResponse](parallelism)(name => WikipediaAPI.wikiPage(name))
      .mapAsyncUnordered[String](parallelism)(response => fetchHTML(response))
      .map[Article](html => Article.fromWikipediaHTML(Undefined, html))
  }

  private def uriEncode(uri: String): String = URLEncoder.encode(uri, "UTF-8")
}
