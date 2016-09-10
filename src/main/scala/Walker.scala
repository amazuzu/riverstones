import scala.collection.immutable.::


object Walker extends App {

  import Iterator._

  def walkStart(v: Int, path: List[Char]) = walk(v, '#' :: path /* add stone since left bank is like s stone*/)

  private def walk(v: Int, path: List[Char]): Iterator[List[_]] =
    range(-1, 2) filter (_ + v > 0) flatMap { delta =>
      (if (v + delta >= path.length) single(Nil) //jump through river
      else path drop (v + delta) match {
        case '#' :: Nil => single(List(0)) //last stone
        case list@('#' :: _) => walk(v + delta, list) //we on stone
        case _ => empty
      }) map (delta :: _)
    }


  println(walkStart(8, "#--#---#--#----#--#".toList).toList)


}
