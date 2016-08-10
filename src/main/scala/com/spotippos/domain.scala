package com.spotippos

import spray.json._
import com.spottipos._
import spray.httpx.SprayJsonSupport

trait Property {
  def x: Int
  def y: Int
  def title: String
  def price: Int
  def description: String
  def beds: Int
  def baths: Int
  def squareMeters: Int

  require(!title.isEmpty, "title must be not empty")
  require(!description.isEmpty, "description must be not empty")
  require(0 <= x && x <= 1400, "x must be between 0 and 1400")
  require(0 <= y && y <= 1000, "y must be between 0 and 1000")
  require(1 <= beds && beds <= 5, "beds must be between 1 and 5")
  require(1 <= baths && baths <= 4, "baths must be between 1 and 4")
  require(20 <= squareMeters && squareMeters <= 240, "squareMeters must be between 20 and 240")
  require(0 < price, "price must be greater than 0")
}

case class PropertyQuery(ax: Int, ay: Int, bx: Int, by: Int) {
  require(0 <= ax && ax <= 1400, "ax must be between 0 and 1400")
  require(0 <= ay && ay <= 1000, "ay must be between 0 and 1000")
  require(0 <= bx && bx <= 1400, "bx must be between 0 and 1400")
  require(0 <= by && by <= 1000, "by must be between 0 and 1000")
  require(ay >= by, "ay must be greather or equal by")
  require(ax <= bx, "ax must be less or equal bx")
}

case class PropertySearchResponse(foundProperties: Int, properties: Array[PropertyEntry])
case class PropertyRequest(x: Int, y: Int, title: String, price: Int, description: String, beds: Int, baths: Int, squareMeters: Int) extends Property

case class PropertyRequestSample(x: Int, y: Int, title: String, price: Int, description: String, beds: Int, baths: Int, squareMeters: Int, id: Int) extends Property

case class PropertyEntry(x: Int,
                         y: Int,
                         title: String,
                         price: Int,
                         description: String,
                         beds: Int,
                         baths: Int,
                         squareMeters: Int,
                         id: Int,
                         provinces: Seq[Province]) extends Property

case class Province(name: String, upperLeftX: Int, upperLeftY: Int, bottomRightX: Int, bottomRightY: Int)
object Gode extends Province("Gode", 0, 1000, 600, 500)
object Ruja extends Province("Ruja", 400, 1000, 1100, 500)
object Jaby extends Province("Jaby", 1100, 1000, 1400, 500)
object Scavy extends Province("Scavy", 0, 500, 600, 0)
object Groola extends Province("Groola", 600, 500, 800, 0)
object Nova extends Province("Nova", 800, 500, 1400, 0)
case class SpotippoPiece(province: List[Province], x: Int, y: Int, property: Option[PropertyEntry])

case class RegistryPropertyColideException(message: String) extends Exception(message)

case class RegistryPropertyOutOfBoundsException(message: String) extends Exception(message)

object PropertyJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {

  implicit object ProvinceFormat extends RootJsonFormat[Province] {
    def write(c: Province) = JsString(c.name)
    def read(value: JsValue) = {
      throw new DeserializationException("Unsupported type")
    }
  }

  implicit val PropertyRequestRootFormats: RootJsonFormat[PropertyRequest] = rootFormat(jsonFormat8(PropertyRequest))
  implicit val PropertyRequestSampleFormats: RootJsonFormat[PropertyRequestSample] = rootFormat(jsonFormat9(PropertyRequestSample))
  implicit val PropertyRootFormats: RootJsonFormat[PropertyEntry] = rootFormat(jsonFormat10(PropertyEntry))
  implicit val PropertySearchResponseRootFormats: RootJsonFormat[PropertySearchResponse] = rootFormat(jsonFormat2(PropertySearchResponse))

}

