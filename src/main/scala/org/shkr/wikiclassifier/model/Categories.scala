package org.shkr.wikiclassifier.model

sealed trait Category

case object Disease extends Category

case object NotDisease extends Category

case object Undefined extends Category

