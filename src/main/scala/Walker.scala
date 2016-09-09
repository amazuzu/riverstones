import scala.collection.immutable.::


object Walker extends App {

  import Iterator._

  def walkStart(v: Int, path: List[Char]) = walk(v, '#' :: path /* add stone since left bank is like s stone*/)

  private def walk(v: Int, path: List[Char]): Iterator[List[Int]] = {
    def fork(velocity: Int, fpath: List[Char]): Iterator[List[Int]] =
      range(-1, 2).filter(_ + velocity > 0).flatMap { delta =>
        (
          if (velocity + delta >= fpath.length) single(Nil) //jump through river
          else fpath.drop(velocity + delta) match {
            case '#' :: Nil => single(List(0)) //last stone
            case list@('#' :: _) => walk(velocity + delta, list) //we on stone
            case _ => empty
          }
          ).map(delta :: _)
      }

    fork(v, path)
  }


  println(walkStart(8, "#--#---#--#----#--#".toList).toList)


}
