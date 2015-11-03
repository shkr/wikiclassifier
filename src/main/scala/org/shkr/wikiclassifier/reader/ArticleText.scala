package org.shkr.wikiclassifier.reader

case class ArticleText(sentences: IndexedSeq[IndexedSeq[String]])

object ArticleText {
  def empty: ArticleText = ArticleText(IndexedSeq.empty[IndexedSeq[String]])
}
