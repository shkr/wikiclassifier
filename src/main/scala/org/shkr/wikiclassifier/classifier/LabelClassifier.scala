package org.shkr.wikiclassifier.classifier

import scala.collection.mutable

class LabelClassifier(sizeCovariance: BigDecimal) {

  private val srcMap = new mutable.HashMap[String, GaussianDistribution]()

  def addSection(sectionName: String, text: List[String]): Unit={
    srcMap.update(sectionName, {
      val sectionSizeDistribution = srcMap.getOrElse(sectionName, new GaussianDistribution(sizeCovariance))
      sectionSizeDistribution.addSample(List(text.size))
      sectionSizeDistribution
    })
  }

  def classifyML(text: List[String]): String={
    srcMap.map(labelWithGaussianDistribution => {
      val mean: BigDecimal = labelWithGaussianDistribution._2.maximizeLikelihoodParameters(List.empty[BigDecimal])

      (labelWithGaussianDistribution._1, mean)
    }).maxBy(_._2)._1
  }

  def classifyBayesian(text: List[String]): String={
    srcMap.map(labelWithGaussianDistribution => {
        0.0
    })
  }
}
