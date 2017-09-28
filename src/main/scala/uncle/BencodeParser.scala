package uncle

import org.parboiled2._
import uncle.model._

import scala.collection.immutable.SortedMap

private[uncle] case class BencodeParser(input: ParserInput) extends Parser {

  def LoneElement = rule { Element ~ EOI }

  def Element: Rule1[BElement] = rule { ByteString | Long | Listing | Dictionary }

  private def ByteString: Rule1[BString] = rule {
    Count ~ ':' ~> ((count: Int) => capture(count.times(!EOI ~ CharPredicate.All))) ~> ((v: String) =>BString(v))
  }

  private def Count: Rule1[Int] = rule { capture(CharPredicate.Digit.+) ~> ((_: String).toInt)}

  private def Long: Rule1[BLong] = rule { 'i' ~  capture(ValidLong)  ~ 'e' ~> ((n: String) => BLong(n.toLong))}

  private def ValidLong: Rule0 = rule {
    '0' |
    '-'.? ~ CharPredicate.Digit19 ~ CharPredicate.Digit.*
  }

  private def Listing: Rule1[BList] = rule {
    'l' ~ zeroOrMore(Element) ~> ((es: Seq[BElement]) => BList(es.toList)) ~ 'e'
  }

  private def Dictionary: Rule1[BDictionary] = rule {
    'd' ~ zeroOrMore(DictionaryEntry) ~> ((d: Seq[(BString, BElement)]) => {
      val keys = d.map(_._1.value)
      if(keys == keys.sorted) push(BDictionary(SortedMap[BString, BElement]() ++ d))
      else {
        fail("Dictionary keys must be ordered") ~ push(BDictionary(SortedMap[BString, BElement]()))
      }
    }) ~ 'e'
  }

  private def DictionaryEntry = rule {
    ByteString ~ Element ~> ((bs: BString, e: BElement) => bs -> e)
  }
}