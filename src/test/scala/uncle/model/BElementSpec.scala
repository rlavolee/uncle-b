package uncle
package model

import org.scalatest.{Matchers, WordSpecLike}
import play.api.libs.json.{JsArray, JsNumber, JsObject, JsString}

import scala.collection.immutable.SortedMap

class BElementSpec extends WordSpecLike with Matchers {

  "🍚  BElement" should {
    "properly encode BString spam" in {
      bs"spam".encode shouldEqual "4:spam"
    }
    "properly encode BLong -3" in {
      BLong(-3L).encode shouldEqual "i-3e"
    }
    "properly encode empty Blist" in {
      BList.empty.encode shouldEqual "le"
    }
    "properly encode Blist" in {
      BList(List(bs"spam", BLong(4L), BList(List(bs"plop", BLong(-2L))),
        BDictionary(SortedMap(bs"cow" -> bs"moo", bs"spam" -> bs"eggs"))))
        .encode shouldEqual "l4:spami4el4:plopi-2eed3:cow3:moo4:spam4:eggsee"
    }
    "properly encode empty BDictionary" in {
      BDictionary.empty.encode shouldEqual "de"
    }
    "properly encode BDictionary" in {
      BDictionary(SortedMap(bs"cow" -> bs"moo", bs"spam" -> bs"eggs"))
        .encode shouldEqual "d3:cow3:moo4:spam4:eggse"
    }
    "properly json encode BString" in {
      bs"plop".toJson shouldEqual JsString("plop")
    }
    "properly json encode BLong" in {
      BLong(3L).toJson shouldEqual JsNumber(3)
    }
    "properly json encode BList" in {
      BList(List(BLong(3L), bs"plop")).toJson shouldEqual JsArray(Seq(JsNumber(3), JsString("plop")))
    }
    "properly json encode BDictionary" in {
      val dict = BDictionary(SortedMap(bs"firstKey" -> BList(List(BLong(3L), bs"plop")), bs"secondKey" -> bs"plop")).toJson
      dict shouldEqual JsObject(Seq("firstKey" -> JsArray(Seq(JsNumber(3), JsString("plop"))), "secondKey" -> JsString("plop")))
    }
  }

}
