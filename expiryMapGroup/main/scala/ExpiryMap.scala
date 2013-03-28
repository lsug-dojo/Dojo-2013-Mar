import collection.immutable.Queue

//Solution at http://higher-state.blogspot.co.uk/2013/02/expiration-map.html
case class ExpiryMap[A,B](map:Map[A,(B,Long)], queue:Queue[(A, Long)], duration:Long) {

  def +(entry: (A,B))(implicit time:Long) =  {
    map + (entry._1 -> (entry._2, time + duration))

  }


  def get(s: A)(implicit time:Long): Option[B] =
    map.get(s) collect { case (value, t2) if t2 > time => value}
}

object ExpiryMap {
  def apply[A,B](duration:Long, entries:(A, B)*)(implicit time:Long) =
    new ExpiryMap(entries.map(p => (p._1, (p._2, time + duration))).toMap, Queue(entries.map(p => (p._1, time + duration)):_*), duration)
}
