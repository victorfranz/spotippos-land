package com.spotippos

import org.json4s.DefaultJsonFormats
import akka.actor.Actor
import akka.actor.ActorLogging
import spray.json._
import spray.routing.Directives
import spray.routing.HttpService
import com.spottipos._
import spray.http.HttpHeaders._
import spray.http.StatusCodes
import spray.http.HttpEntity
import spray.http.HttpHeader
import spray.http.HttpHeaders
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import spray.http.MediaTypes
import spray.httpx.SprayJsonSupport
import spray.httpx.SprayJsonSupport._

class ApiRouter() extends Actor with ApiService with ActorLogging {

  def actorRefFactory = context

  def receive = runRoute(myRoute)
}

trait ApiService extends HttpService with Directives with DefaultJsonFormats {

  import com.spotippos.PropertyJsonSupport._

  val appotiposWorld: ApotipposWorld = SimpleApotipposWorld

  val myRoute = {
    implicit val executionContext = actorRefFactory.dispatcher

    pathEndOrSingleSlash {
      get {
        respondWithMediaType(MediaTypes.`application/json`) {
          complete(BuildInfo.toJson)
        }
      }
    } ~ pathPrefix("properties") {
      pathSuffix(Segment) { id =>
        pathEndOrSingleSlash {
          get {
            complete(appotiposWorld.getProperty(id))
          }
        }
      } ~
        pathEndOrSingleSlash {
          post {
            respondWithMediaType(MediaTypes.`application/json`) {
              entity(as[PropertyRequest]) { property =>
                appotiposWorld.register(property) match {
                  case Failure(f) => {
                    complete(StatusCodes.BadRequest, f.getMessage)
                  }
                  case Success(s) => {
                    complete(StatusCodes.Created, Seq[HttpHeader](Location(s"/properties/${s.id}")), s)
                  }
                }
              }
            }
          } ~
            get {
              parameters("ax".as[Int], "ay".as[Int], "bx".as[Int], "by".as[Int]).as(PropertyQuery) { (query) =>
                complete(appotiposWorld.searchProperties(query))
              }
            }
        }
    }
  }
}

