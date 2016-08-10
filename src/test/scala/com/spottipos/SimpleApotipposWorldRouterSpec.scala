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
class SimpleApotipposWorldRouterSpec extends Specification with Specs2RouteTest with Mockito with HttpService {
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
                      |  "title": "Imovel codigo 1, com 5 quartos e 4 banheiros",
                      |  "beds": 4
                      |}""".stripMargin

      Post("/properties", HttpEntity(`application/json`, "")) ~> sealRoute(service) ~> check {
        responseAs[String] === "Request entity expected but not supplied"
        status === BadRequest
      }

      postInvalidRequest("{}", "Object is missing required member 'x'")

      postInvalidRequest(validProperty.replace("Imovel codigo 1, com 5 quartos e 4 banheiros", ""), "requirement failed: title must be not empty")

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

    "Create and provide access to existing properties" >> {
      val property = """{
                    |  "x": 222,
                    |  "squareMeters": 210,
                    |  "y": 444,
                    |  "description": "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
                    |  "price": 1250000,
                    |  "baths": 3,
                    |  "title": "Imovel codigo 1, com 5 quartos e 4 banheiros",
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
                              |  "title": "Imovel codigo 1, com 5 quartos e 4 banheiros",
                              |  "beds": 4
                              |}""".stripMargin
        status === Created
      }

      "Reject post property on an area that already has a property" >> {

        PropertiesSimpleRegistry.loadSample()

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
          responseAs[String] === "Invalid (x, y) cordinates or square meters size, area compromised by existing property"
          status === BadRequest
        }
      }

      "Get a property by id" >> {
        Get("/properties/0") ~> sealRoute(service) ~> check {
          responseAs[String] === """{
                              |  "x": 222,
                              |  "squareMeters": 210,
                              |  "provinces": ["Gode"],
                              |  "y": 444,
                              |  "description": "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
                              |  "price": 1250000,
                              |  "id": 0,
                              |  "baths": 3,
                              |  "title": "Imovel codigo 1, com 5 quartos e 4 banheiros",
                              |  "beds": 4
                              |}""".stripMargin
          status === OK
        }

        Get("/properties/9000") ~> sealRoute(service) ~> check {
          status === NotFound
        }

        Get("/properties/asdfadsf") ~> sealRoute(service) ~> check {
          status === NotFound
        }
      }

      "Reject invalid search query" >> {
        Get("/properties?") ~> sealRoute(service) ~> check {
          responseAs[String] === """Request is missing required query parameter 'ax'"""
          status === NotFound
        }

        Get("/properties?ax=asfdasdfa") ~> sealRoute(service) ~> check {
          responseAs[String] === """The query parameter 'ax' was malformed:
                                           |'asfdasdfa' is not a valid 32-bit integer value""".stripMargin
          status === BadRequest
        }

        Get("/properties?ax=-1") ~> sealRoute(service) ~> check {
          responseAs[String] === """Request is missing required query parameter 'ay'"""
          status === NotFound
        }

        Get("/properties?ax=1") ~> sealRoute(service) ~> check {
          responseAs[String] === """Request is missing required query parameter 'ay'"""
          status === NotFound
        }

        Get("/properties?ax=-1&ay=0&bx=0&by=0") ~> sealRoute(service) ~> check {
          responseAs[String] === """requirement failed: ax must be between 0 and 1400"""
          status === BadRequest
        }

        Get("/properties?ax=1401&ay=0&bx=0&by=0") ~> sealRoute(service) ~> check {
          responseAs[String] === """requirement failed: ax must be between 0 and 1400"""
          status === BadRequest
        }

        Get("/properties?ax=0&ay=-1&bx=0&by=0") ~> sealRoute(service) ~> check {
          responseAs[String] === """requirement failed: ay must be between 0 and 1000"""
          status === BadRequest
        }

        Get("/properties?ax=0&ay=1001&bx=0&by=0") ~> sealRoute(service) ~> check {
          responseAs[String] === """requirement failed: ay must be between 0 and 1000"""
          status === BadRequest
        }

        Get("/properties?ax=0&ay=0&bx=0&by=-1") ~> sealRoute(service) ~> check {
          responseAs[String] === """requirement failed: by must be between 0 and 1000"""
          status === BadRequest
        }

        Get("/properties?ax=0&ay=1001&bx=0&by=1001") ~> sealRoute(service) ~> check {
          responseAs[String] === """requirement failed: ay must be between 0 and 1000"""
          status === BadRequest
        }

        Get("/properties?ax=0&ay=0&bx=-1&by=0") ~> sealRoute(service) ~> check {
          responseAs[String] === """requirement failed: bx must be between 0 and 1400"""
          status === BadRequest
        }

        Get("/properties?ax=0&ay=0&bx=1401&by=0") ~> sealRoute(service) ~> check {
          responseAs[String] === """requirement failed: bx must be between 0 and 1400"""
          status === BadRequest
        }

        Get("/properties?ax=0&ay=0&bx=0&by=100") ~> sealRoute(service) ~> check {
          responseAs[String] === """requirement failed: ay must be greather or equal by"""
          status === BadRequest
        }

        Get("/properties?ax=100&ay=100&bx=0&by=100") ~> sealRoute(service) ~> check {
          responseAs[String] === """requirement failed: ax must be less or equal bx"""
          status === BadRequest
        }

      }

      "Search for properties" >> {

        Get("/properties?ax=400&ay=400&bx=400&by=400") ~> sealRoute(service) ~> check {
          responseAs[String] === """{
                                  |  "foundProperties": 0,
                                  |  "properties": []
                                  |}""".stripMargin
          status === OK
        }

        Get("/properties?ax=222&ay=444&bx=222&by=444") ~> sealRoute(service) ~> check {
          responseAs[String] === """{
                                  |  "foundProperties": 1,
                                  |  "properties": [{
                                  |    "x": 222,
                                  |    "squareMeters": 210,
                                  |    "provinces": ["Gode"],
                                  |    "y": 444,
                                  |    "description": "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
                                  |    "price": 1250000,
                                  |    "id": 0,
                                  |    "baths": 3,
                                  |    "title": "Imovel codigo 1, com 5 quartos e 4 banheiros",
                                  |    "beds": 4
                                  |  }]
                                  |}""".stripMargin
          status === OK
        }

        Get("/properties?ax=114&ay=822&bx=114&by=822") ~> sealRoute(service) ~> check {
          responseAs[String] === """{
                                |  "foundProperties": 1,
                                |  "properties": [{
                                |    "x": 114,
                                |    "squareMeters": 52,
                                |    "provinces": ["Gode"],
                                |    "y": 822,
                                |    "description": "Laboris duis ut mollit fugiat excepteur cupidatat veniam anim laborum sunt pariatur culpa minim. Aliquip id magna do voluptate voluptate tempor exercitation ut laboris fugiat consequat ipsum mollit dolor.",
                                |    "price": 535000,
                                |    "id": 7513,
                                |    "baths": 1,
                                |    "title": "Imóvel código 7513, com 2 quartos e 1 banheiros.",
                                |    "beds": 2
                                |  }]
                                |}""".stripMargin
          status === OK
        }

        Get("/properties?ax=400&ay=900&bx=420&by=850") ~> sealRoute(service) ~> check {
          responseAs[String] === """{
                                |  "foundProperties": 5,
                                |  "properties": [{
                                |    "x": 407,
                                |    "squareMeters": 47,
                                |    "provinces": ["Gode", "Ruja"],
                                |    "y": 871,
                                |    "description": "Esse non excepteur proident labore ad ad et elit ex non dolor consequat. Elit minim tempor irure veniam.",
                                |    "price": 472000,
                                |    "id": 2573,
                                |    "baths": 1,
                                |    "title": "Imóvel código 2573, com 2 quartos e 1 banheiros.",
                                |    "beds": 2
                                |  }, {
                                |    "x": 409,
                                |    "squareMeters": 184,
                                |    "provinces": ["Gode", "Ruja"],
                                |    "y": 858,
                                |    "description": "Esse proident veniam aliqua nostrud culpa do proident eiusmod irure sit qui irure. Sunt sit officia voluptate eu dolore labore mollit incididunt cillum incididunt consequat proident adipisicing.",
                                |    "price": 1847000,
                                |    "id": 5067,
                                |    "baths": 4,
                                |    "title": "Imóvel código 5067, com 5 quartos e 4 banheiros.",
                                |    "beds": 5
                                |  }, {
                                |    "x": 409,
                                |    "squareMeters": 156,
                                |    "provinces": ["Gode", "Ruja"],
                                |    "y": 885,
                                |    "description": "Velit non dolore eu magna pariatur reprehenderit aliquip enim ad veniam do. Ad ut elit dolor ea adipisicing incididunt cupidatat et reprehenderit sint.",
                                |    "price": 1601000,
                                |    "id": 2581,
                                |    "baths": 4,
                                |    "title": "Imóvel código 2581, com 5 quartos e 4 banheiros.",
                                |    "beds": 5
                                |  }, {
                                |    "x": 413,
                                |    "squareMeters": 93,
                                |    "provinces": ["Gode", "Ruja"],
                                |    "y": 891,
                                |    "description": "Ea fugiat mollit nostrud eiusmod sint laboris officia. Ut nisi velit tempor exercitation ut.",
                                |    "price": 936000,
                                |    "id": 4005,
                                |    "baths": 3,
                                |    "title": "Imóvel código 4005, com 4 quartos e 3 banheiros.",
                                |    "beds": 4
                                |  }, {
                                |    "x": 418,
                                |    "squareMeters": 53,
                                |    "provinces": ["Gode", "Ruja"],
                                |    "y": 873,
                                |    "description": "Sunt Lorem cillum qui cupidatat minim irure veniam deserunt est adipisicing fugiat labore ullamco. Tempor non irure proident ipsum aliquip pariatur proident magna cupidatat id est Lorem.",
                                |    "price": 575000,
                                |    "id": 5377,
                                |    "baths": 1,
                                |    "title": "Imóvel código 5377, com 2 quartos e 1 banheiros.",
                                |    "beds": 2
                                |  }]
                                |}""".stripMargin
          status === OK
        }
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
    }.myRoute
  }
}