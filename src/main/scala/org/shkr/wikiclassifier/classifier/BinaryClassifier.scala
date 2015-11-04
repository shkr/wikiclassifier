package org.shkr.wikiclassifier.classifier

import org.shkr.wikiclassifier.model.Category

import scala.collection.mutable

class BinaryClassifier(positive: Category, negative: Category) {
  
  val categoryBasedEventCounter: Map[Category, mutable.HashMap[String, Int]] = Map(
    positive -> getEventCounter,
    negative -> getEventCounter
  )

  private def getEventCounter: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()  { override def default(key: String) = 0 }

  var totalPositiveObservation: Long = 0
  var totalNegativeObservation: Long = 0
  
  def updateObservationCount(category: Category): Unit={
    if(category.equals(positive)){
      totalPositiveObservation +=1
    } else if(category.equals(negative)){
      totalNegativeObservation +=1
    }
  }

  def observation(category: Category, sentence: Set[String]): Unit={
    categoryBasedEventCounter.contains(category) match {
      case true => {
        //Update Category Observation Count
        updateObservationCount(category)
        //Update Category Event Counter
        val categoryEventCounter = categoryBasedEventCounter(category)
        sentence.foreach(word => categoryEventCounter.update(word, categoryEventCounter(word)+1))  
      }
      case false => //DoNothing
    } 
  }

  def distribution: Map[String, Double]={
   categoryBasedEventCounter(positive)
    .map(e => {

      val positiveProbability: Double = e._2.toDouble / totalPositiveObservation
      val negativeProbability: Double =categoryBasedEventCounter(negative)(e._1).toDouble / totalNegativeObservation
          (e._1, Math.max(positiveProbability-negativeProbability, 0.0))
    }).toMap
  }
}
