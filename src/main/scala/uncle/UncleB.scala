package uncle

import org.parboiled2.ParserInput
import play.api.libs.json.{Json, Writes}
import uncle.model.BElement

import scala.util.Try

object UncleB {
  def decode(i: ParserInput): Try[BElement] = BencodeParser(i).LoneElement.run()
  def encode(b: BElement): String = b.encode
  def toBen[T: Writes](t: T): BElement = Json.toJson(t).as[BElement]
}
