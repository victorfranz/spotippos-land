package com.spottipos

import scala.concurrent.duration.DurationInt
import org.junit.runner.RunWith
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import com.spotippos.ApiService
import akka.actor.ActorRefFactory
import akka.actor.ActorSystem
import akka.testkit.TestDuration
import spray.http.StatusCodes._
import spray.routing.HttpService
import spray.testkit.Specs2RouteTest
import org.specs2.runner.JUnitRunner
import spray.http.HttpEntity
import spray.http.MediaTypes._
import com.spotippos._

@RunWith(classOf[JUnitRunner])
class ApotipposWorldRouterSpec extends Specification with Specs2RouteTest with Mockito with HttpService {
  sequential
  def actorRefFactory = system
  val spec = this
  val service = createRoute()

  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(30.second dilated)

  "ApiRouter" should {
    "return build info" >> {
      Get("/") ~> sealRoute(service) ~> check {
        val response = responseAs[String]
        response should contain("builtAtMillis", """"name":"spotippos-land"""", "scalaVersion", """"version":"0.1"""", "builtAtString")
        status === OK

      }
    }

    "Reject invalid properties" >> {
      val validProperty = """{
                      |  "x": 222,
                      |  "squareMeters": 210,
                      |  "y": 444,
                      |  "description": "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
                      |  "price": 1250000,
                      |  "baths": 3,
                      |  "title": "Imóvel código 1, com 5 quartos e 4 banheiros",
                      |  "beds": 4
                      |}""".stripMargin

      Post("/properties", HttpEntity(`application/json`, "")) ~> sealRoute(service) ~> check {
        responseAs[String] === "Request entity expected but not supplied"
        status === BadRequest
      }

      postInvalidRequest("{}", "Object is missing required member 'x'")

      postInvalidRequest(validProperty.replace("Imóvel código 1, com 5 quartos e 4 banheiros", ""), "requirement failed: title must be not empty")

      postInvalidRequest(validProperty.replace("222", "-1"), "requirement failed: x must be between 0 and 1400")

      postInvalidRequest(validProperty.replace("222", "1401"), "requirement failed: x must be between 0 and 1400")

      postInvalidRequest(validProperty.replace("444", "-1"), "requirement failed: y must be between 0 and 1000")

      postInvalidRequest(validProperty.replace("444", "1001"), "requirement failed: y must be between 0 and 1000")

      postInvalidRequest(validProperty.replace("beds\": 4", "beds\": 0"), "requirement failed: beds must be between 1 and 5")

      postInvalidRequest(validProperty.replace("beds\": 4", "beds\": 6"), "requirement failed: beds must be between 1 and 5")

      postInvalidRequest(validProperty.replace("3", "0"), "requirement failed: baths must be between 1 and 4")

      postInvalidRequest(validProperty.replace("3", "5"), "requirement failed: baths must be between 1 and 4")

      postInvalidRequest(validProperty.replace("210", "19"), "requirement failed: squareMeters must be between 20 and 240")

      postInvalidRequest(validProperty.replace("210", "241"), "requirement failed: squareMeters must be between 20 and 240")

      postInvalidRequest(validProperty.replace("1250000", "0"), "requirement failed: price must be greater than 0")

      postInvalidRequest(validProperty.replace("1250000", "-1250000"), "requirement failed: price must be greater than 0")
    }

    "Create a new propertie" >> {
      val property = """{
                    |  "x": 222,
                    |  "squareMeters": 210,
                    |  "y": 444,
                    |  "description": "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
                    |  "price": 1250000,
                    |  "baths": 3,
                    |  "title": "Imóvel código 1, com 5 quartos e 4 banheiros",
                    |  "beds": 4
                    |}""".stripMargin

      Post("/properties", HttpEntity(`application/json`, property)) ~> sealRoute(service) ~> check {
        responseAs[String] === """{
                              |  "x": 222,
                              |  "squareMeters": 210,
                              |  "provinces": ["Gode"],
                              |  "y": 444,
                              |  "description": "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
                              |  "price": 1250000,
                              |  "id": 0,
                              |  "baths": 3,
                              |  "title": "Im�vel c�digo 1, com 5 quartos e 4 banheiros",
                              |  "beds": 4
                              |}""".stripMargin
        status === Created
      }
    }

    "Reject post property on an area that already has a property" >> {

      PropertiesRegistry.loadSample()

      val property = """{
                       | "title": "Imóvel código 1380, com 5 quartos e 4 banheiros.",
                       | "price": 1351000,
                       | "description": "Aute pariatur cillum id occaecat elit cupidatat. Aute cillum exercitation est ad ad elit quis.",
                       | "x": 1260,
                       | "y": 600,
                       | "beds": 5,
                       | "baths": 4,
                       | "squareMeters": 131
                       |}""".stripMargin

      Post("/properties", HttpEntity(`application/json`, property)) ~> sealRoute(service) ~> check {
        responseAs[String] === "Invalid (x, y) cordinates or square meters size, area compromised by existing property"
        status === BadRequest
      }
    }

    "Reject post property out of bounds" >> {

      Post("/properties", HttpEntity(`application/json`, """{
                       | "title": "Imóvel código 1380, com 5 quartos e 4 banheiros.",
                       | "price": 1351000,
                       | "description": "Aute pariatur cillum id occaecat elit cupidatat. Aute cillum exercitation est ad ad elit quis.",
                       | "x": 1400,
                       | "y": 1000,
                       | "beds": 5,
                       | "baths": 4,
                       | "squareMeters": 20
                       |}""".stripMargin)) ~> sealRoute(service) ~> check {
        responseAs[String] === "Invalid x, y cordinates or square meters size, property out of bounds of Apotippos Land"
        status === BadRequest
      }
    }
  }

  def postInvalidRequest(entity: String, expected: String) = {
    Post("/properties", HttpEntity(`application/json`, entity)) ~> sealRoute(service) ~> check {
      responseAs[String] === s"""The request content was malformed:
                                  |${expected}""".stripMargin
      status === BadRequest
    }
  }

  def createRoute() = {
    new ApiService() {
      override implicit def actorRefFactory: ActorRefFactory = spec.actorRefFactory
      
      override val appotiposWorld = ApotipposWorld
    }.myRoute
  }
}