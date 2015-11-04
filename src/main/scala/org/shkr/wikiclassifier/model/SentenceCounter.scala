package org.shkr.wikiclassifier.model

import akka.actor.Actor
import org.shkr.wikiclassifier.reader.Article

class SentenceCounter extends Actor {

  var totalSentences: Int = 0

  def receive: Actor.Receive={
    case article: Article => totalSentences += article.introduction.sentences.size
  }
}
