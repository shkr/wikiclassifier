package org.shkr.wikiclassifier.model

import scala.collection.mutable

/**
 * Copyright (c) 2015 Lumiata Inc.
 */
class ProbabilityMassFunction {
  
  val eventCounter: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()  { override def default(key: String) = 0 }
  var totalObservation: Long = 0

  def observation(sentence: Set[String]): Unit={
    totalObservation += 1
    sentence.foreach(word => eventCounter.update(word, eventCounter(word)+1))
  }
}
