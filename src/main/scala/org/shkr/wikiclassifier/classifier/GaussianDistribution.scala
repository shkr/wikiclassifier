package org.shkr.wikiclassifier.classifier

import scala.collection.mutable.ListBuffer

class GaussianDistribution(val covariance: BigDecimal = 1.0) {

  /** 
   * We have univariate labeled samples x_1: c_1, x_2:c_2 ..... x_N: c_K: where
   * there are 1...K categories.
   * We assume each sample comes from an independent Gaussian Distribution
   * Our goal is to estimate the parameter theta = (mean, std_dev) for each
   * class 1..K and provide then use it to
   * classify the category for an unknown sample (x')
   * 
   * We will use this to classify section names by the content length for disease articles
   */
  private var samples: scala.collection.mutable.ListBuffer[BigDecimal] = ListBuffer[BigDecimal]()

  /*
   * In the Bayesian Estimator we assume only mean is unknown
   * Covariance is known 1.0 or a given constant
   * We then assume that the uncertainity on the mean of the P.D. from which samples are fetched
   * is best modeled as a Gaussian Distribution with (priorMean, priorCovariance)
   */
  private var priorMean: BigDecimal = 0.0
  private var priorCovariance: BigDecimal = 1.0

  private def sqr(x: BigDecimal): BigDecimal={
    x*x
  }

  /** Add Sample**/
  def addSample(x: BigDecimal): Unit={
    samples += x
  }

  /**
   * Solve the Maximum Likelihood for mean which maximizes
   * L(mean) = P(S| mean) = P(x1, x2, x3, .... | mean)
   * We can assume that each sample was Independently picked from an Identical Distribution
   * of G(mean, covariance)
   * Here, mean is unknown.
   * We estimate the mean by Maximum Likelihood
   * @return meanEstimate
   */
  def maximizeLikelihoodParameters(newSamples: List[BigDecimal]): (BigDecimal, BigDecimal)={
    //Add Samples
    samples ++= newSamples

    //The Maximum Likelihood estimate fo u = Sum(x for each in D)/Cardinality of D
    var size: Int = 0
    var sum: BigDecimal = 0.0
    samples.foreach(x => {sum+=x; size+=1})

    val mean: BigDecimal = sum/size

    //Maximum Likelihood estimate of sample covariance
    val covariance = samples.map(x => (x-mean)*(x-mean)).sum/(size - 1)

    (mean, covariance)
  }

  /**
   * theta is a sufficient statistic for R.V. X; That implies H(x|theta) = 0
   *
   * p(theta| x) = p(x| theta)p(theta)* alpha(samples)
   *
   * Assume
   * p(theta) has a prior N(u_0, sigma_0)
   * theta is only mean
   * We assume we know the covariance of the samples to be 1.0 or a another constant
   * This function returns the posteriorMean and posteriorCovariance on the PD of the mean of the
   * normal Distribution of a sample
   * @return p(mean| D)
   */
  def bayesianLearningEstimate(newSamples: List[BigDecimal]): (BigDecimal, BigDecimal)={

    //The Sample Mean Estimate
    var size: Int = 0
    var sum: BigDecimal = 0.0
    newSamples.foreach(x => {sum+=x; size+=1})

    val sampleMean: BigDecimal = sum/size

    val posteriorMean = ((size*sqr(priorCovariance))/( size*sqr(priorCovariance) + sqr(covariance)))*sampleMean +
                                             (sqr(covariance)/(size*sqr(priorCovariance) + sqr(covariance)))*priorMean

    val posteriorCovariance = (sqr(covariance)*sqr(priorCovariance))/( size*sqr(priorCovariance) + sqr(covariance))

    priorMean = posteriorMean
    priorCovariance = posteriorCovariance
    samples ++= newSamples

    (posteriorMean, posteriorCovariance)
  }
}
