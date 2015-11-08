package org.shkr.wikiclassifier.model

import play.api.libs.json.JsValue
import scala.collection.mutable

case class TwoCategoryResult(classification: Category,
                             article: Article,
                             positiveScore: Double,
                             negativeScore: Double,
                             info: Map[String, JsValue])

object TwoCategoryResult {
  def empty: TwoCategoryResult = TwoCategoryResult(Undefined, Article.empty, 0.0, 0.0, Map.empty[String, JsValue])
}

sealed trait StatisticalError

case object TruePositive extends StatisticalError
case object TrueNegative extends StatisticalError
case object FalsePositive extends StatisticalError
case object FalseNegative extends StatisticalError


class StatisticalErrorTable(positive: Category, negative: Category) {

  val map: mutable.HashMap[StatisticalError, Long] = mutable.HashMap(
    TruePositive -> 0,
    TrueNegative -> 0,
    FalsePositive -> 0,
    FalseNegative -> 0
  )

  def update(category: Category, classification: Category): Unit={
    if(category==positive && category!=classification){
      map.update(FalseNegative, map(FalseNegative)+1)
    } else if(category==negative && category!=classification){
      map.update(FalsePositive, map(FalsePositive)+1)
    } else if(category==positive) {
      map.update(TruePositive, map(TruePositive)+1)
    } else {
      map.update(TrueNegative, map(TrueNegative)+1)
    }
  }

  override def toString(): String={
    val totalCount = map.values.sum
    s"TP  = ${map(TruePositive).toDouble/totalCount} \n" +
    s"FP  = ${map(FalsePositive).toDouble/totalCount} \n" +
    s"TN  = ${map(TrueNegative).toDouble/totalCount} \n" +
    s"FN  = ${map(FalseNegative).toDouble/totalCount} \n" +
    s"TPR = ${map(TruePositive)/(map(TruePositive) + map(FalseNegative)).toDouble} \n" +
    s"TNR = ${map(TrueNegative)/(map(TrueNegative) + map(FalsePositive)).toDouble} \n" +
    s"PPV = ${map(TruePositive)/(map(TruePositive) + map(FalsePositive)).toDouble} \n" +
    s"NPV = ${map(TrueNegative)/(map(TrueNegative) + map(FalseNegative)).toDouble} \n" +
    s"Accuracy = ${(map(TruePositive) + map(TrueNegative)).toDouble/totalCount}"
  }
}