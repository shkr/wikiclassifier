package org.shkr.wikiclassifier.model

case class ArticleText(sentences: IndexedSeq[IndexedSeq[String]])

object ArticleText {

  def empty: ArticleText = ArticleText(IndexedSeq.empty[IndexedSeq[String]])
}

case class ContentType(heading: List[Array[String]], map: Map[String, String])

object ContentType {

  def empty: ContentType = ContentType(List.empty[Array[String]], Map.empty[String, String])
}