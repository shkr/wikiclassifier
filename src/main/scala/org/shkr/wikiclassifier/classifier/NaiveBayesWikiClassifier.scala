package org.shkr.wikiclassifier.classifier

import org.shkr.wikiclassifier.model.{Article, Category, Disease, TwoCategoryResult}
import play.api.libs.json.{JsValue, Json}

import scala.collection.mutable

class NaiveBayesWikiClassifier(positive: Category, negative: Category) {

  /**
   * An Observation here is
   *  1. Sentence Observed in the text (also referred as introduction) provided in the wikipage
   *  2. Sentence Observed which is the Title of the wikipage
   *  3. Sentence Observed in each Content Type heading
   *
   * An Event = Wi is defined when W member of Vocabulary exists in a Sentence. It is a Bernoulli RV
   * which takes value 0 or 1 depending on whether it is part of Vocabulary or not.
   *
   * The Probability of P(W_i = W | C = c) where c is the category of the sentence
   * is estimated in training. Then the Classifier uses Naive Bayes to classify
   * a document either as Positive or Negative Category using the
   * argMax_(c member of {positive, negative}) Maximum A Posterior Probability =
   *    log(p(c)) + log(p(Title|c)) + log(p^(ContentType|c)) + log(p^(Introduction|c))
   *
   * ^ implies estimated probability
   * p^(ContentType|c) is the mean of log(P(Sentence|c)) = Sum(log(P(w|c))
   * here we make the naive assumption that each event W_i = w is independent of each other
   * and we also assume positional independence.
   *
   * In order to handle, situations where a word not existing in the vocabulary is encountered
   * then to avoid log(zero) we initialize each word in vocabulary with count 1.
   */

  /** To enable disable debugging logs **/
  var debug: Boolean = false

  /** Laplacian Smoothing **/
  val laplaceFactor: Int = 1

  /** debug Log**/
  def logDebug(str: Any*): Unit={
    if(debug){
      println(str)
    }
  }

  /** A HashMap to Count events **/
  private def getEventCounter: mutable.HashMap[String, Int] =
    new mutable.HashMap[String, Int]()  { override def default(key: String) = laplaceFactor }

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

  /** Content **/
  val categoryBasedContentEventCounter: Map[Category, mutable.HashMap[String, Int]] = Map(
    positive -> getEventCounter,
    negative -> getEventCounter
  )
  
  var totalPositiveContentTypesObservation: Long = 0
  var totalNegativeContentTypesObservation: Long = 0

  def updateCategoryTypeObservation(set: Set[String], category: Category): Unit={
    if(category==positive) {
      totalPositiveContentTypesObservation += set.size
    } else if(category==negative){
      totalNegativeContentTypesObservation += set.size
    }
  }

  def pWordInContentTypeGivenCategory(word: String, category: Category): Double={
    validCategory(category) match {
      case true => {
        val categoryEventCounter = categoryBasedContentEventCounter(category)
        val totalCount: Double = if(category.equals(positive)){
          totalPositiveContentTypesObservation
        }  else {
          totalNegativeContentTypesObservation
        }

        categoryEventCounter(word)/(totalCount + (categoryEventCounter.keys.size*laplaceFactor))
      }
      case false => 0
    }
  }

  def logProbContentTypeGivenCategory(article: Article, category: Category): Double={
    if(article.contentTypes.heading.nonEmpty) {
      article.contentTypes.heading.map(_.toSet).map(sentence => {
        sentence.map(w => Math.log(pWordInContentTypeGivenCategory(w, category))).sum
      }).sum / article.contentTypes.heading.length
    } else {
      1.0
    }
  }

  /** Title **/
  var totalPositiveTitleObservation: Long = 0
  var totalNegativeTitleObservation: Long = 0

  val categoryBasedTitleEventCounter: Map[Category, mutable.HashMap[String, Int]] = Map(
    positive -> getEventCounter,
    negative -> getEventCounter
  )

  def updateTitleObservation(set: Set[String], category: Category): Unit={
    if(category==positive) {
      totalPositiveTitleObservation += set.size
    } else if(category==negative){
      totalNegativeTitleObservation += set.size
    }
  }

  def pWordInTitleGivenCategory(word: String, category: Category): Double={
    validCategory(category) match {
      case true => {
        val categoryEventCounter = categoryBasedTitleEventCounter(category)
        val totalCount: Double = if(category.equals(positive)){
          totalPositiveTitleObservation
        }  else {
          totalNegativeTitleObservation
        }

        categoryEventCounter(word)/(totalCount + (categoryEventCounter.keys.size*laplaceFactor))
      }
      case false => 0
    }
  }

  def logProbTitleGivenCategory(article: Article, category: Category): Double={
    article.title.distinct.map(w => Math.log(pWordInTitleGivenCategory(w, category))).sum
  }

  /** Sentences from Introduction **/
  var totalPositiveSentenceObservation: Long = 0
  var totalNegativeSentenceObservation: Long = 0

  val categoryBasedSentenceEventCounter: Map[Category, mutable.HashMap[String, Int]] = Map(
    positive -> getEventCounter,
    negative -> getEventCounter
  )

  def updateSentenceObservation(set: Set[String], category: Category): Unit={
    if(category==positive) {
      totalPositiveSentenceObservation += set.size
    } else if(category==negative){
      totalNegativeSentenceObservation += set.size
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

        categoryEventCounter(word)/(totalCount + (categoryEventCounter.keys.size*laplaceFactor))
      }
      case false => 0
    }
  }

  def logProbSentenceGivenCategory(sentence: IndexedSeq[String], category: Category): Double={
    sentence.distinct.map(w => Math.log(pWordInSentenceGivenCategory(w, category))).sum
  }

  def logProbIntroductionGivenCategory(article: Article, category: Category): Double={
    if(article.introduction.sentences.nonEmpty){
      article.introduction.sentences
        .map(sentence => logProbSentenceGivenCategory(sentence, category))
        .sum / article.introduction.sentences.length
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

        //Update ContentTypes Observation
        val categoryContentEventCounter = categoryBasedContentEventCounter(article.category)
        article.contentTypes.heading.map(_.toSet).foreach(sentence => {
          updateCategoryTypeObservation(sentence, article.category)
          //Update Content Observation
          sentence.foreach(word => categoryContentEventCounter.update(word, categoryContentEventCounter(word) + 1))
        })

        //Update Title Observation
        val categoryTitleEventCounter = categoryBasedTitleEventCounter(article.category)
        updateTitleObservation(article.title.distinct.toSet, article.category)
        article.title.distinct.foreach(word => categoryTitleEventCounter.update(word, categoryTitleEventCounter(word) + 1))

        //Each Introduction has many Sentence Observation
        val categorySentenceEventCounter = categoryBasedSentenceEventCounter(article.category)
        article.introduction.sentences.map(_.toSet).foreach(sentence => {
          updateSentenceObservation(sentence, article.category)
          //Update Sentence Observation
          sentence.foreach(word => categorySentenceEventCounter.update(word, categorySentenceEventCounter(word) + 1))
        })

      }
      case false => //DoNothing
    }
  }

  def logProbArticleGivenCategory(article: Article, category: Category): Double={
    logDebug(s"finding logProbArticleGivenCategory for Category=$category and article=${article.title}")

    if(validCategory(category)){
      val pTitle = logProbTitleGivenCategory(article, category)
      val pContent = logProbContentTypeGivenCategory(article, category)
      val pIntro = logProbIntroductionGivenCategory(article, category)
      logDebug(s"finding pTitle for article=${article.title} score=$pTitle")
      logDebug(s"finding pIntro for article=${article.title} score=$pIntro")
      logDebug(s"finding pContent for article=${article.title} score=$pContent")
      pTitle + pContent + pIntro + Math.log(pCategoryForArticle(category))
    } else {
      0.0
    }
  }

  def expectedDiseaseEdgeCase(isChemicalSubstance: Boolean, isDiseaseGroup: Boolean): Boolean={
    !(isChemicalSubstance || isDiseaseGroup)
  }

  /**
   * This Object contains additional methods to handle two edge cases
   * 1. Disease Group articles
   * 2. Chemical Substance articles
   */
  object EdgeCaseHandler {

    def expectedDiseaseGroup(article: Article, category: Category): Boolean={
      //DiseaseGroup
      val diseaseGroup: Boolean = {
        val isDisease = category==Disease
        val multiplicativeFactor = 5 //Heuristically set threshold
        val expectedCountForWordDiseases = {
          Math.ceil(pWordInSentenceGivenCategory("diseases", Disease) * article.introduction.sentences.map(_.size).sum)
        }
        val countForWordDiseases = article.introduction.sentences.count(sentence => sentence.contains("diseases"))

        isDisease && (countForWordDiseases > (expectedCountForWordDiseases*multiplicativeFactor))
      }

      diseaseGroup
    }

    def expectedChemicalSubstance(article: Article): Boolean={
      //Chemical Substance
      val containsCASNumber: Boolean = Article.HTMLTagParser.infoBox.findFirstIn(article.html).getOrElse("null")
        .toLowerCase.contains("cas registry number")

      containsCASNumber
    }
  }

  def classifyArticle(article: Article, handleEdgeCase: Boolean, setDebug: Boolean = false): TwoCategoryResult={
    debug = setDebug

    val positiveConditionalProbability = logProbArticleGivenCategory(article, positive)
    logDebug(s"finding p for category=$positive article=${article.title} is $positiveConditionalProbability")

    val negativeConditionalProbability = logProbArticleGivenCategory(article, negative)
    logDebug(s"finding p for category=$negative article=${article.title} is $negativeConditionalProbability")

    if(positiveConditionalProbability>negativeConditionalProbability){

      /** Extracted here because aids in classification **/
      val isDiseaseGroup: Boolean = EdgeCaseHandler.expectedDiseaseGroup(article, Disease)
      val isChemicalSubstance: Boolean = EdgeCaseHandler.expectedChemicalSubstance(article)
      val isDiseaseEdgeCase: Boolean = expectedDiseaseEdgeCase(isChemicalSubstance, isDiseaseGroup)
      val additionalInfo: Map[String, JsValue] = Map(
        "disease_group_expected" -> Json.toJson(isDiseaseGroup),
        "chemical_substance_expected" -> Json.toJson(isChemicalSubstance)
      )
      val result: Category = !handleEdgeCase || isDiseaseEdgeCase match {
        case true => positive
        case false => negative
      }

      TwoCategoryResult(result, article, positiveConditionalProbability, negativeConditionalProbability, additionalInfo)
    } else {
      TwoCategoryResult(negative, article, positiveConditionalProbability, negativeConditionalProbability, Map.empty[String, JsValue])
    }
  }
}