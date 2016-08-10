package com.spotippos

import spray.json.DefaultJsonProtocol
import spray.json.DefaultJsonProtocol._
import spray.json._
import com.spottipos._
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import java.io.InputStream
import com.typesafe.scalalogging.StrictLogging

/**
 * Defines an ApotipposWorld
 */
trait ApotipposWorld extends StrictLogging {

  val provinces = List[Province](Gode, Ruja, Jaby, Scavy, Groola, Nova)
  lazy val registry: PropertiesRegistry = PropertiesRegistry

  def register(property: PropertyRequest): Try[PropertyEntry] = Try {
    registry.register(property)
  }

  def toLandStringMap = {
    val land = registry.land
    val bitMap = land.map { _.map { s => { if (s.property.isDefined) s.property.get.id else "-" } } }
    bitMap.deep.mkString("\n").replace("Array", "")
  }

  def getProperty(id: String) = {
    Try { registry.getProperty(id.toInt) } match {
      case Failure(f) => {
        logger.info(s"Invalid entry: ${id}")
        None
      }
      case Success(s) => {
        s
      }
    }
  }

  def searchProperties(query: PropertyQuery) = {
    registry.getProperties(query)
  }
}

/**
 * An ApotipposWorld with a property register that propagates the assigned area according to each property square meters defined.
 */
object ApotipposWorld extends ApotipposWorld

/**
 * An ApotipposWorld with a property register that just assign a property given the (x, y) coordinates.
 */
object SimpleApotipposWorld extends ApotipposWorld {
  override lazy val registry: PropertiesRegistry = PropertiesSimpleRegistry
}

/**
 * Defines how should act The Regulator Entity on ApotipposWorld responsible for assign a property
 */
trait PropertiesRegistry extends StrictLogging {

  lazy val land: Array[Array[SpotippoPiece]] = Array.tabulate(1401, 1001)((x, y) => {
    SpotippoPiece(matchProvinces(x, y), x, y, None)
  })

  private val properties = scala.collection.mutable.Map[Int, PropertyEntry]()

  def getProperty(id: Int) = {
    properties.get(id)
  }

  def getProperties(query: PropertyQuery) = {

    val ax = query.ax
    val bx = query.bx + 1

    val by = query.by
    val ay = query.ay + 1

    val pieces = land.slice(query.ax, query.bx + 1) // slice x
      .map { y => y.slice(query.by, query.ay + 1) } // for each x, then slice y
      .flatten // flat in a unique list
      .flatMap { x => x.property } // reduce by only existing properties

    PropertySearchResponse(pieces.size, pieces)
  }

  def register(property: Property): PropertyEntry = {
    register(property, properties.size)
  }

  def register(property: Property, id: Int): PropertyEntry = synchronized {
    val offset = property.squareMeters / 2 - 1

    // 1. create new PropertyEntry 
    val newProperty = PropertyEntry(property.x,
      property.y,
      property.title,
      property.price,
      property.description,
      property.beds,
      property.baths,
      property.squareMeters, id, matchProvinces(property.x, property.y, offset))

    register(offset, property, id, Option(newProperty))

    properties(id) = newProperty
    newProperty
  }

  def register(offset: Int, property: Property, id: Int, newProperty: Option[PropertyEntry])

  def matchProvinces(x: Int, y: Int): List[Province] = for {
    province <- ApotipposWorld.provinces
    if (x >= province.upperLeftX && y <= province.upperLeftY
      && x <= province.bottomRightX && y >= province.bottomRightY)
  } yield {
    province
  }

  def matchProvinces(x: Int, y: Int, offset: Int): Seq[Province] = {

    val bla = for {
      offsetX <- 0 to offset
      offsetY <- 0 to offset
    } yield {
      matchProvinces(x + offset, y + offset)
    }

    bla.flatten.distinct
  }

  def loadSample() {
    import com.spotippos.PropertyJsonSupport.PropertyRequestSampleFormats

    val in: InputStream = getClass.getResourceAsStream("/properties.json")

    val jsonString = this.using(io.Source.fromInputStream(in)) { source =>
      source.mkString
    }

    val properties = jsonString.parseJson.convertTo[List[PropertyRequestSample]]

    for (property <- properties) {
      Try { register(property, property.id) } match {
        case Failure(f) => {
          logger.info(s"${f} Error on inserting property: ${property.toJson}")
        }
        case Success(s) => {
          logger.info(s"Inserted id ${property.id}")
        }
      }
    }

  }

  private def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }
}

/**
 * A Regulator Entity on ApotipposWorld responsible for assign a property, it propagates the assigned area according to each property square meters defined.
 */
object PropertiesRegistry extends PropertiesRegistry {
  override def register(offset: Int, property: Property, id: Int, newProperty: Option[PropertyEntry]) = {

    // initialize a buffer for revert operations
    val propertyBuffer = scala.collection.mutable.ListBuffer[SpotippoPiece]()

    def undo = {
      for (revertPiece <- propertyBuffer) {
        land(revertPiece.x)(revertPiece.y) = revertPiece.copy(property = None)
      }
      propertyBuffer.clear()
    }

    for {
      offsetX <- 0 to offset
      offsetY <- 0 to offset
    } yield {
      if (property.x + offsetX > 1400 || property.y + offsetY > 1000 || property.x < 0 || property.y < 0) {
        undo
        throw RegistryPropertyOutOfBoundsException("Invalid x, y cordinates or square meters size, property out of bounds of Apotippos Land")
      }

      val piece = land(property.x + offsetX)(property.y + offsetY)

      if (piece.property.isDefined) {
        undo
        throw RegistryPropertyColideException("Invalid (x, y) cordinates or square meters size, area compromised by existing property")
      } else {
        // assign the property to a SpotippoPiece
        val newPiece = piece.copy(property = newProperty)
        land(property.x + offsetX)(property.y + offsetY) = newPiece
        propertyBuffer += newPiece
      }
    }

    propertyBuffer.clear()
  }
}

/**
 * A Regulator Entity on ApotipposWorld responsible for assign a property, just assign a property given the (x, y) coordinates.
 */
object PropertiesSimpleRegistry extends PropertiesRegistry {

  override def register(offset: Int, property: Property, id: Int, newProperty: Option[PropertyEntry]) = {

    if (property.x > 1400 || property.y > 1000 || property.x < 0 || property.y < 0) {
      throw RegistryPropertyOutOfBoundsException("Invalid x, y cordinates or square meters size, property out of bounds of Apotippos Land")
    }

    val piece = land(property.x)(property.y)

    if (piece.property.isDefined) {
      throw RegistryPropertyColideException("Invalid (x, y) cordinates or square meters size, area compromised by existing property")
    } else {
      // assign the property to a SpotippoPiece
      val newPiece = piece.copy(property = newProperty)
      land(property.x)(property.y) = newPiece
    }

  }
}

