package org.shkr.wikiclassifier.classifier

import org.shkr.wikiclassifier.model.{TwoCategoryResult, Article, Category}

import scala.collection.mutable

class NaiveBayesWikiClassifier(positive: Category, negative: Category) {

  /**
   * An Observation here is
   *  1. SentenceObservation
   *  2. TitleObservation
   *
   * An Event(w_i) in a SentenceObservation is equal to word = w_i and w is member of Sentence
   * An Event(w_i) in a TitleObservation is equal to word = w_i and w is member of Title
   */
  var debug: Boolean = false

  def logDebug(str: String): Unit={
    if(debug){
      println(str)
    }
  }

  def validCategory(category: Category): Boolean = category==positive || category==negative

  /** Article **/
  var totalPositiveArticleObservation: Long = 0
  var totalNegativeArticleObservation: Long = 0

  def pCategoryForArticle(category: Category): Double={

    if(category==positive){
      totalPositiveArticleObservation.toDouble/(totalPositiveArticleObservation + totalNegativeArticleObservation)
    } else if(category==negative){
      totalNegativeArticleObservation.toDouble/(totalNegativeArticleObservation + totalPositiveArticleObservation)
    } else {
      0.0
    }
  }

  def updateArticleObservationCount(category: Category): Unit={
    if(category.equals(positive)){
      totalPositiveArticleObservation +=1
    } else if(category.equals(negative)){
      totalNegativeArticleObservation +=1
    }
  }

  val contentTypeClassifier = new NaiveBayesClassifier(positive, negative)
  val titleClassifier = new NaiveBayesClassifier(positive, negative)
  val introductionClassifier = new NaiveBayesClassifier(positive, negative)

  def pContentTypeGivenCategory(article: Article, category: Category): Double={
    val set: Set[String] = article.contentTypes.flatMap(_.split("\\s+")).toSet
    contentTypeClassifier.pSentenceGivenCategory(set, category) match {
      case 0 => 1
      case x => x
    }
  }

  def pTitleGivenCategory(article: Article, category: Category): Double={
    val set: Set[String] = article.title.split("\\s+").toSet
    titleClassifier.pSentenceGivenCategory(set, category) match {
      case 0 => 1
      case x => x
    }
  }


  def pIntroductionGivenCategory(article: Article, category: Category): Double={
    /** We do not have all samples about the topic, so we make a Mean Estimate of the Disease Category for Each sentence **/
    if(article.introduction.sentences.nonEmpty){
      article.introduction.sentences
        .map(sentence => introductionClassifier.pSentenceGivenCategory(sentence.toSet, category)).sum /
          article.introduction.sentences.length
    } else {
      1.0
    }
  }

  /** Record Observation **/
  def observation(article: Article): Unit={
    validCategory(article.category) match {
      case true => {

        //Update Article Observation
        updateArticleObservationCount(article.category)

        //Update Content Observation
        val contentSet: Set[String] = article.contentTypes.flatMap(_.split("\\s+")).toSet
        contentTypeClassifier.updateObservationCount(contentSet, article.category)

        //Update Title Observation
        val titleSet: Set[String] = article.title.split("\\s+").toSet
        titleClassifier.updateObservationCount(titleSet, article.category)

        //Each Introduction has many Sentence Observation

        article.introduction.sentences.foreach(sentence => {
          //Update Sentence Observation
          val sentenceSet: Set[String] = sentence.toSet
          introductionClassifier.updateObservationCount(sentenceSet, article.category)
        })
      }
      case false => //DoNothing
    } 
  }

  def pArticleGivenCategory(article: Article, category: Category): Double={
    logDebug(s"finding pArticleGivenCategory for Category=${category} and article=${article.title}")

    if(validCategory(category)){
      val pTitle = pTitleGivenCategory(article, category)
      val pContentType = pContentTypeGivenCategory(article, category)
      val pIntro = pIntroductionGivenCategory(article, category)
      val pCategory = pCategoryForArticle(category)
      logDebug(s"finding pTitle for article=${article.title} score=${pTitle}")
      logDebug(s"finding pContentType for article=${article.title} score=${pContentType}")
      logDebug(s"finding pIntro for article=${article.title} score=${pIntro}")
      pTitle * pContentType * pIntro * pCategory
    } else {
      Double.MinValue
    }
  }

  def classifyArticle(article: Article, setDebug: Boolean = false): TwoCategoryResult={
    debug = setDebug

    val positiveConditionalProbability = pArticleGivenCategory(article, positive)
    logDebug(s"finding p for category=${positive} article=${article.title} is ${positiveConditionalProbability}")

    val negativeConditionalProbability = pArticleGivenCategory(article, negative)
    logDebug(s"finding p for category=${negative} article=${article.title} is ${negativeConditionalProbability}")

    if(positiveConditionalProbability>negativeConditionalProbability){
      TwoCategoryResult(positive, article, positiveConditionalProbability, negativeConditionalProbability)
    } else {
      TwoCategoryResult(negative, article, positiveConditionalProbability, negativeConditionalProbability)
    }
  }
}