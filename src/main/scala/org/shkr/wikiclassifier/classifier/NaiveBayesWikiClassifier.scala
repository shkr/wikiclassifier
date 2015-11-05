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

  //Laplacian Smoothing
  val laplaceFactor: Double = 1.0

  def logDebug(str: String): Unit={
    if(debug){
      println(str)
    }
  }

  private def getEventCounter: mutable.HashMap[String, Int] =
    new mutable.HashMap[String, Int]()  { override def default(key: String) = 0 }

  def validCategory(category: Category): Boolean = category==positive || category==negative

  /** Article **/
  var totalPositiveArticleObservation: Long = 0
  var totalNegativeArticleObservation: Long = 0

  def pCategoryForArticle(category: Category): Double={

    if(category==positive){
      totalPositiveArticleObservation.toDouble/(totalPositiveArticleObservation+ totalNegativeArticleObservation)
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

  /** Content **/
  val categoryBasedContentEventCounter: Map[Category, mutable.HashMap[String, Int]] = Map(
    positive -> getEventCounter,
    negative -> getEventCounter
  )

  def pWordInContentGivenCategory(word: String, category: Category): Double={
    validCategory(category) match {
      case true => {
        val categoryEventCounter = categoryBasedContentEventCounter(category)
        val totalCount: Double = if(category.equals(positive)){
          totalPositiveArticleObservation
        }  else {
          totalNegativeArticleObservation
        }

        //logDebug(s"for word = $word the probability given category = $category is ${categoryEventCounter(word).toDouble / totalCount}")
        (categoryEventCounter(word) + laplaceFactor)/ ((laplaceFactor + 1.0) * totalCount)
      }
      case false => 0
    }
  }

  def pContentGivenCategory(article: Article, category: Category): Double={
    article.contentTypes.flatMap(_.split(" ")).distinct.map(pWordInContentGivenCategory(_, category)).product * pCategoryForArticle(category)
  }

  /** Title **/
  val categoryBasedTitleEventCounter: Map[Category, mutable.HashMap[String, Int]] = Map(
    positive -> getEventCounter,
    negative -> getEventCounter
  )

  def pWordInTitleGivenCategory(word: String, category: Category): Double={
    validCategory(category) match {
      case true => {
        val categoryEventCounter = categoryBasedTitleEventCounter(category)
        val totalCount: Double = if(category.equals(positive)){
          totalPositiveArticleObservation
        }  else {
          totalNegativeArticleObservation
        }

        //logDebug(s"for word = $word the probability given category = $category is ${categoryEventCounter(word).toDouble / totalCount}")

        (categoryEventCounter(word) + laplaceFactor)/ ((laplaceFactor + 1.0) * totalCount)
      }
      case false => 0
    }
  }

  def pTitleGivenCategory(article: Article, category: Category): Double={
    article.title.split(" ").distinct.map(pWordInTitleGivenCategory(_, category)).product * pCategoryForArticle(category)
  }

  /** Sentence **/
  var totalPositiveSentenceObservation: Long = 0
  var totalNegativeSentenceObservation: Long = 0

  val categoryBasedSentenceEventCounter: Map[Category, mutable.HashMap[String, Int]] = Map(
    positive -> getEventCounter,
    negative -> getEventCounter
  )

  def updateSentenceObservationCount(category: Category): Unit={
    if(category.equals(positive)){
      totalPositiveSentenceObservation +=1
    } else if(category.equals(negative)){
      totalNegativeSentenceObservation +=1
    }
  }

  def pCategoryForSentences(category: Category): Double={

    if(category==positive){
      totalPositiveSentenceObservation.toDouble/(totalNegativeSentenceObservation+ totalPositiveSentenceObservation)
    } else if(category==negative){
      totalNegativeSentenceObservation.toDouble/(totalNegativeSentenceObservation + totalPositiveSentenceObservation)
    } else {
      0.0
    }
  }
  
  def pWordInSentenceGivenCategory(word: String, category: Category): Double={
    validCategory(category) match {
      case true => {
        val categoryEventCounter = categoryBasedSentenceEventCounter(category)
        val totalCount: Double = if(category.equals(positive)){
          totalPositiveSentenceObservation
        }  else {
          totalNegativeSentenceObservation
        }

        //logDebug(s"for word = $word the probability given category = $category is ${categoryEventCounter(word).toDouble / totalCount}")

        (categoryEventCounter(word) + laplaceFactor)/ ((laplaceFactor + 1.0)*totalCount)
      }
      case false => 0
    }
  }

  def pSentenceGivenCategory(sentence: IndexedSeq[String], category: Category): Double={
    sentence.distinct.map(pWordInSentenceGivenCategory(_, category)).product
  }

  def pIntroductionGivenCategory(article: Article, category: Category): Double={
    if(article.introduction.sentences.nonEmpty){
      (article.introduction.sentences
        .map(sentence => pSentenceGivenCategory(sentence, category))
        .sum / article.introduction.sentences.length)*pCategoryForSentences(positive)
    } else {
      0.0
    }
  }

  /** Record Observation **/
  def observation(article: Article): Unit={
    validCategory(article.category) match {
      case true => {

        //Update Article Observation
        updateArticleObservationCount(article.category)

        //Update Content Observation
        val categoryContentEventCounter = categoryBasedContentEventCounter(article.category)
        article.contentTypes.flatMap(_.split(" ")).distinct.foreach(word => categoryContentEventCounter.update(word, categoryContentEventCounter(word) + 1))

        //Update Title Observation
        val categoryTitleEventCounter = categoryBasedTitleEventCounter(article.category)
        article.title.split(" ").distinct.foreach(word => categoryTitleEventCounter.update(word, categoryTitleEventCounter(word) + 1))

        //Each Introduction has many Sentence Observation
        val categorySentenceEventCounter = categoryBasedSentenceEventCounter(article.category)
        article.introduction.sentences.foreach(sentence => {
          //Update Sentence Observation
          updateSentenceObservationCount(article.category)
          sentence.distinct.foreach(word => categorySentenceEventCounter.update(word, categorySentenceEventCounter(word) + 1))
        })

      }
      case false => //DoNothing
    } 
  }

  def pArticleGivenCategory(article: Article, category: Category): Double={
    logDebug(s"finding pArticleGivenCategory for Category=${category} and article=${article.title}")

    if(validCategory(category)){
      val pTitle = pTitleGivenCategory(article, category)
      val pContent = pContentGivenCategory(article, category)
      val pIntro = pIntroductionGivenCategory(article, category)
      logDebug(s"finding pTitle for article=${article.title} score=${pTitle}")
      logDebug(s"finding pIntro for article=${article.title} score=${pIntro}")
      pTitleGivenCategory(article, category) * pContent * pIntroductionGivenCategory(article, category)
    } else {
      0.0
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