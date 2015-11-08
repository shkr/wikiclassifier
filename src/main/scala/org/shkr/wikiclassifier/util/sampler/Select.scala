package org.shkr.wikiclassifier.util.sampler

object Select {
  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR(n: Int, ls: List[A], r: java.util.Random): List[A] =
      if (n <= 0) Nil
      else {
        val (rest, e) = Remove.removeAt(r.nextInt(ls.length), ls)
        e :: randomSelectR(n - 1, rest, r)
      }
    randomSelectR(n, ls, new java.util.Random)
  }
}
