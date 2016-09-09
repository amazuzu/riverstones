import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import org.scalatest.prop.TableDrivenPropertyChecks._

class WalkerSpec extends FlatSpec {

  val CaseNum = -1

  val cases = Table(
    ("n", "v", "river ", " result")

    //vanilla case
    , (0, 3, "--#--#--#".toList, List(0, 0, 0, 0))
    , (1, 3, "---#--#-#".toList, List(1, -1, -1, 0))
    , (2, 3, "---#--#---#".toList, List(1, -1, 1, 0))

    //    //enough speed to jump
    , (3, 0, List('-', '-', '#'), Nil)
    , (4, 1, List('-', '-', '#'), Nil)
    , (5, 2, List('-', '-', '#'), List(1, 0))
    , (6, 3, List('-', '-', '-'), List(1))
    , (7, 9, List('-', '-', '-'), List(1))

    // corner cases
    , (8, 0, List(), List(1))
    , (9, 1, List(), List())
    , (10, 2, List(), List())
    , (11, 9, List(), List())
    , (12, 3, List('-', '-', '-', '#'), List(1, 0))
    // one values cases
    , (13, 0, List('#'), List(1, 0))
    , (14, 1, List('#'), List(0, 0))
    , (15, 2, List('#'), List(0))
    , (16, 9, List('#'), List(0))
    //emptyness
    , (17, 0, List('-'), Nil)
    , (18, 1, List('-'), List(1))
    , (19, 2, List('-'), List(0))
    , (20, 0, List('-', '-'), Nil)
    , (21, 1, List('-', '-'), Nil)
    , (22, 2, List('-', '-'), List(1))
    , (23, 3, List('-', '-'), List(0))
    , (24, 3, List('-', '-'), List(0))

    //cases from head
    , (25, 0, List('#', '-'), List(1, 1))
    , (26, 1, List('#', '-'), List(0, 1))
    , (27, 2, List('#', '-'), List(1))
    , (28, 3, List('#', '-'), Nil)
    , (29, 0, List('-', '#'), Nil)
    , (30, 1, List('-', '#'), List(1, 0))
    , (31, 2, List('-', '#'), List(0, 0))
    , (32, 3, List('-', '#'), List(0))
    , (33, 0, List('#', '-', '#'), Nil)
    , (34, 1, List('#', '-', '#'), Nil)
    , (35, 2, List('#', '-', '#'), List(1, 0))
    , (36, 2, List('#', '-', '#'), List(-1, 1, 0)) //wrong case 21
    , (37, 3, List('#', '-', '#'), List(1))
    , (38, 3, List('#', '-', '#'), List(0, 0))
    , (39, 0, List('-', '#', '-'), Nil)
    , (40, 1, List('-', '#', '-'), List(1, 0))
    , (41, 2, List('-', '#', '-'), List(0, 0))
    , (42, 3, List('-', '#', '-'), List(1))
    , (43, 4, List('-', '#', '-'), List(0))
    , (44, 0, List('#', '#', '#'), List(1, 0, 0, 0))
    , (45, 1, List('#', '#', '#'), List(0, 0, 0, 0))
    , (46, 2, List('#', '#', '#'), List(0, 0))
    , (47, 2, List('#', '#', '#'), List(0, -1, 0))
    , (48, 0, List('#', '#', '-'), List(1, 0, 1))
    , (49, 1, List('#', '#', '-'), List(0, 0, 1))
  )

  forAll(cases) { (n, v, river, result) =>
    whenever(CaseNum == -1 || CaseNum != -1 && CaseNum == n) {
      if (result == Nil)
        Walker.walkStart(v, river).toSet should be eq (List())
      else
        Walker.walkStart(v, river).toSet should contain(result)
    }
  }
}
