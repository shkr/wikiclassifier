package org.shkr.wikiclassifier.classifier

import scala.collection.mutable.ListBuffer


class GaussianDistribution(d: Int) {

  /** 
   * We have labeled samples x_1: c_1, x_2:c_2 ..... x_N: c_K: where
   * there are 1...K categories.
   * We assume each sample comes from an independent Gaussian Distribution
   * Our goal is to estimate the parameter theta = (mean, std_dev) for each
   * class 1..K and provide then use it to
   * classify the category for an unknown sample (x')
   * 
   * We will use this here to predict the relationship between the number of references and the
   * number of lines in each wikipedia article.
   * We will divide the articles into categories
   * 0-3 references
   * 3-6 references
   * 7-9 references
   * 9-12 references
   * 12-15 references
   * 15+ references
   * The range length of 3 has been arbitrary selected
   */
  private var samples: scala.collection.mutable.ListBuffer[Array[Int]] = ListBuffer[Array[Int]]()

  /** Add Sample**/
  def addSample(x: Array[Int]): Boolean={
    //Only if size of Array matches the initialized dimension then accept it
    x.length==d match {
      case true => samples += x; true
      case false => false
    }
  }

  /**
   * Solve the Maximum Likelihood for mean which maximizes
   * L(mean) = P(S| mean) = P(x1, x2, x3, .... | mean)
   * We can assume that each sample was Independently picked from an Identical Distribution
   * of G(mean, std_dev)
   * If std_dev is not provided we assume it to be 1.0
   * @return
   */
  def approximateMean(stdDev: BigDecimal = 1.0): Array[BigDecimal]={
    Array()
  }

}
