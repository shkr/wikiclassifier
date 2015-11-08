package org.shkr.wikiclassifier.util.sampler

object Remove {

  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new java.util.NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (pre, Nil)        => throw new java.util.NoSuchElementException
  }
}
