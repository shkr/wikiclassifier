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
   * We will use this to classify section names by the content length for disease articles
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
  def approximateMean: Array[BigDecimal]={

  }

  /**
   * Solve the Maximum Likelihood for mean which maximizes
   * L(stdDev) = P(S| stdDev) = P(x1, x2, x3, .... | stdDev)
   * We can assume that each sample was Independently picked from an Identical Distribution
   * The biased estimate of solving L'(stdDev) = d P(S| stdDev) / dstdDev = 0
   * is (1 - 1/n)sumOf(x_i - u)^T(x_i -u) for i=1 ...n
   * The unbiased version is
   * 1/(n-1)sumOf(x_i - u)^T(x_i -u) for i=1 ...n
   * @return
   */
  def approximateStdDev: Array[BigDecimal]={

  }
}
