package org.shkr.wikiclassifier.classifier

import org.shkr.wikiclassifier.model.Category
import scala.collection.mutable

class NaiveBayesClassifier(positive: Category, negative: Category) {

  var debug: Boolean = false

  /** Laplacian Smoothing **/
  val laplaceFactor: Int = 1

  def logDebug(str: String): Unit={
    if(debug){
      println(str)
    }
  }

  private def getEventCounter: mutable.HashMap[String, Int] =
    new mutable.HashMap[String, Int]()  { override def default(key: String) = laplaceFactor }

  def validCategory(category: Category): Boolean = category==positive || category==negative

  var totalPositiveObservation: Long = 0
  var totalNegativeObservation: Long = 0

  val categoryBasedContentEventCounter: Map[Category, mutable.HashMap[String, Int]] = Map(
    positive -> getEventCounter,
    negative -> getEventCounter
  )

  def updateObservationCount(set: Set[String], category: Category): Unit={
    if(category.equals(positive)){
      totalPositiveObservation += set.size
    } else if(category.equals(negative)){
      totalNegativeObservation += set.size
    }
  }

  def pWordGivenCategory(word: String, category: Category): Double={
    validCategory(category) match {
      case true => {
        val categoryEventCounter = categoryBasedContentEventCounter(category)
        val totalCount: Double = if(category.equals(positive)){
          totalPositiveObservation
        }  else {
          totalNegativeObservation
        }

        categoryEventCounter(word).toDouble/totalCount
      }
      case false => 0
    }
  }

  def pSentenceGivenCategory(sentence: Set[String], category: Category): Double={
    if(sentence.nonEmpty){
      sentence.map(w => Math.log(pWordGivenCategory(w, category))).sum
    } else {
      0
    }
  }
}
