package uncle

import org.scalatest.{Matchers, WordSpecLike}
import play.api.libs.json.Json
import uncle.model._

import scala.collection.immutable.SortedMap
import scala.util.Success

class UncleBSpec extends WordSpecLike with Matchers {

  case class Spam(s: String, l: List[Int], b: Boolean)

  object Spam {
    implicit val format = Json.format[Spam]
  }

  "🍚  UncleBSpec" should {
    "successfully decode" in {
      UncleB.decode("4:spam") shouldEqual Success(bs"spam")
    }
    "successfully encode" in {
      UncleB.encode(bs"spam") shouldEqual "4:spam"
    }
    "successfully cast" in {
      UncleB.toBen(Spam("spam", List(-3), b = true)) shouldEqual BDictionary(SortedMap(bs"s" -> bs"spam", bs"l" -> BList(List(BLong(-3))), bs"b" -> bs"true"))
    }
  }

}
