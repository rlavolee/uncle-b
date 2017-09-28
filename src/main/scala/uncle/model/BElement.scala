package uncle.model

import play.api.libs.json._

import scala.collection.immutable.SortedMap

sealed trait BElement {
  def encode: String
  def toJson: JsValue
  def as[T: Reads]: T = toJson.as[T]
}

object BElement {
  implicit val reads: Reads[BElement] = Reads[BElement](BString.fromJson) orElse 
    Reads[BElement](BLong.fromJson) orElse 
    Reads[BElement](BList.fromJson) orElse 
    Reads[BElement](BDictionary.fromJson) orElse
    Reads[BElement](v => JsSuccess(BString(v.toString)))
}

case class BString(value: String) extends BElement {
  override def encode: String = s"${value.length}:$value"

  override def toJson: JsValue = JsString(value)
}

object BString {
  implicit val ordering: Ordering[BString] = implicitly[Ordering[String]].on[BString](_.value)

  def fromJson(s: JsValue): JsResult[BElement] = s.validate[String].map(BString(_))
}

case class BLong(n: Long) extends BElement {
  override def encode: String = s"i${n}e"

  override def toJson: JsValue = JsNumber(n)
}

object BLong {
  def fromJson(s: JsValue): JsResult[BElement] =
    s.validate[Long].map(BLong(_))
}

case class BList(l: List[BElement]) extends BElement {
  override def encode: String = s"l${l.map(_.encode).mkString}e"

  override def toJson: JsValue = JsArray(l.map(_.toJson))
}

object BList {
  val empty: BList = BList(List.empty[BElement])

  def fromJson(s: JsValue): JsResult[BElement] = {
    s.validate[List[BElement]].map(BList(_))
  }
}

case class BDictionary(m: SortedMap[BString, BElement]) extends BElement {
  override def encode: String = s"d${m.map(v => v._1.encode + v._2.encode).mkString}e"

  override def toJson: JsValue = JsObject(m.map(v => v._1.value -> v._2.toJson))
}

object BDictionary {

  implicit val sortedMapReader: Reads[SortedMap[BString, BElement]] =
    implicitly[Reads[Map[String, BElement]]].map(m => SortedMap[BString, BElement]() ++ m.map(v => BString(v._1) -> v._2.as[BElement]))

  val empty: BDictionary = BDictionary(SortedMap.empty)

  def fromJson(s: JsValue): JsResult[BElement] =
    s.validate[SortedMap[BString, BElement]].map(BDictionary(_))
}
