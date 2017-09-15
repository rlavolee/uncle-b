package uncle

import org.scalatest.{Matchers, WordSpecLike}
import uncle.model._

import scala.collection.immutable.SortedMap

class BencodeParserSpec extends WordSpecLike with Matchers {

  "🍚  BencodeParser" should {
    "successfully parse empty as ByteString" in {
      BencodeParser("0:").LoneElement.run().get shouldEqual bs""
    }
    "successfully parse 4:spam as ByteString" in {
      BencodeParser("4:spam").LoneElement.run().get shouldEqual bs"spam"
    }
    "failed to parse invalid input string" in {
      BencodeParser("5:spam").LoneElement.run().isFailure shouldBe true
    }
    "successfully parse i0e as BLong" in {
      BencodeParser("i0e").LoneElement.run().get shouldEqual BLong(0L)
    }
    "successfully parse i3e as BLong" in {
      BencodeParser("i3e").LoneElement.run().get shouldEqual BLong(3L)
    }
    "successfully parse i-3e as BLong" in {
      BencodeParser("i-3e").LoneElement.run().get shouldEqual BLong(-3L)
    }
    "failed to parse invalid input integer" in {
      BencodeParser("i03e").LoneElement.run().isFailure shouldBe true
    }
    "successfully parse l4:spam4:eggse as BList" in {
      BencodeParser("l4:spam4:eggse").LoneElement.run().get shouldEqual BList(List(bs"spam", bs"eggs"))
    }
    "successfully parse l4:spami4ee as BList" in {
      BencodeParser("l4:spami4ee").LoneElement.run().get shouldEqual BList(List(bs"spam", BLong(4L)))
    }
    "successfully parse empty list as BList" in {
      BencodeParser("le").LoneElement.run().get shouldEqual BList.empty
    }
    "successfully parse l4:spami4el4:plopi-2eed3:cow3:moo4:spam4:eggsee as BList" in {
      BencodeParser("l4:spami4el4:plopi-2eed3:cow3:moo4:spam4:eggsee").LoneElement.run().get shouldEqual
        BList(List(bs"spam", BLong(4L), BList(List(bs"plop", BLong(-2L))),
          BDictionary(SortedMap(bs"cow" -> bs"moo", bs"spam" -> bs"eggs"))))
    }
    "succesfully parse de as BDictionary" in {
      BencodeParser("de").LoneElement.run().get shouldEqual BDictionary.empty
    }

  }

}
