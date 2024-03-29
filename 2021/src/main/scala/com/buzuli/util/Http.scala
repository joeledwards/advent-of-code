package com.buzuli.util

import java.time.Instant
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpHeader.ParsingResult.{Error, Ok}
import sttp.client._
import akka.http.scaladsl.{Http => AkkaHttp}
import akka.http.scaladsl.model._
import play.api.libs.json._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

sealed trait Validity[I, O]
case class Valid[I, O](value: O) extends Validity[I, O]
case class Invalid[I, O](message: String, input: I) extends Validity[I, O]

sealed trait HttpBody {
  def contentType: ContentType
}
case class HttpBodyText(body: String) extends HttpBody {
  val contentType = ContentTypes.`text/plain(UTF-8)`
}
case class HttpBodyBytes(body: Array[Byte]) extends HttpBody {
  val contentType = ContentTypes.`application/octet-stream`
}
case class HttpBodyJson(body: JsValue) extends HttpBody {
  val contentType = ContentTypes.`application/json`
}

sealed trait HttpResult {
  def response: Option[HttpResponse] = None
  def body: Option[HttpBody] = None
  def duration: Option[Duration] = None
}
case class HttpResultInvalidMethod(input: String) extends HttpResult
case class HttpResultInvalidUrl(url: String) extends HttpResult
case class HttpResultInvalidHeader(name: String, value: String) extends HttpResult
case class HttpResultInvalidBody() extends HttpResult
case class HttpResultRawResponse(
    rawResponse: HttpResponse,
    data: Option[HttpBody] = None,
    elapsed: Option[Duration] = None
) extends HttpResult {
  override def response: Option[HttpResponse] = Some(rawResponse)
  override def body: Option[HttpBody] = data
  override def duration: Option[Duration] = elapsed
}

object Http {
  implicit val httpBackend = HttpURLConnectionBackend()
  implicit val system = ActorSystem("advent-2021")
  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  object method {
    def apply(method: String): Validity[String, HttpMethod] = validateMethod(method)
  }

  object header {
    def apply(name: String, value: String): Validity[(String, String), HttpHeader] = {
      validateHeader((name, value))
    }
  }

  object body {
    def text(body: String): HttpBody = HttpBodyText(body)
    def bytes(body: Array[Byte]): HttpBody = HttpBodyBytes(body)
    def json(body: JsValue): HttpBody = HttpBodyJson(body)
  }

  def http = AkkaHttp()

  def makeRequest(
    request: HttpRequest,
    timeout: Option[Duration] = Some(Duration(15, TimeUnit.SECONDS))
  ): Future[HttpResponse] = {
    // TODO: Figure out how to enforce client connection timeouts.
    //       From what I have read, Akka HTTP doesn't like the idea of timeouts.
    http.singleRequest(request)
  }
  def rq(
    method: String,
    url: String,
    headers: List[(String, String)] = Nil,
    body: Option[HttpBody] = None,
    collect: Boolean = false,
    timeout: Option[Duration] = None
  ): Future[HttpResult] = {
    val start: Instant = Instant.now
    (
      validateMethod(method),
      validateHeaders(headers)
    ) match {
      case (Invalid(_, _), _) => Future.successful(
        HttpResultInvalidMethod(method)
      )
      case (_, Invalid(_, inv :: _ :: Nil)) => Future.successful(
        HttpResultInvalidHeader(inv._1, inv._2)
      )
      case (Valid(mth), Valid(hdrs)) => makeRequest(
        HttpRequest(
          method = mth,
          uri = url,
          headers = hdrs,
          entity = body match {
            case None => HttpEntity.Empty
            case Some(HttpBodyBytes(bytes)) => HttpEntity(bytes)
            case Some(HttpBodyText(text)) => HttpEntity(text)
            case Some(HttpBodyJson(json)) => HttpEntity(ContentTypes.`application/json`, json.toString.getBytes)
          }
        ),
        timeout = timeout
      ) flatMap { response =>
        collect match {
          case false => {
            response.entity.discardBytes()
            Future.successful(HttpResultRawResponse(response, elapsed = Some(Time.diff(start, Instant.now))))
          }
          case true => {
            response.entity.toStrict(Duration(5, TimeUnit.SECONDS)).collect { case r =>
              r.getData().asByteBuffer.array()
            } map { bytes =>
              val data: HttpBody = response.entity.getContentType() match {
                case ContentTypes.`application/json` => HttpBodyJson(Json.parse(bytes))
                case ContentTypes.`text/plain(UTF-8)` => HttpBodyText(bytes.toString)
                case _ => HttpBodyBytes(bytes)
              }
              HttpResultRawResponse(response, Some(data))
            }
          }
        }
      }
    }
  }

  def get(
    url: String,
    headers: List[(String, String)] = Nil,
    timeout: Option[Duration] = None
  ): Future[HttpResult] = rq(
    "GET",
    url,
    headers = headers,
    timeout = timeout
  )

  def post(
    url: String,
    headers: List[(String, String)] = Nil,
    body: Option[HttpBody] = None,
    timeout: Option[Duration] = None
  ): Future[HttpResult] = rq(
    "POST",
    url,
    headers = headers,
    body = body,
    timeout = timeout
  )

  private def validateMethod(method: String): Validity[String, HttpMethod] = method.toUpperCase match {
    case "CONNECT" => Valid(HttpMethods.CONNECT)
    case "DELETE" => Valid(HttpMethods.DELETE)
    case "GET" => Valid(HttpMethods.GET)
    case "HEAD" => Valid(HttpMethods.HEAD)
    case "OPTIONS" => Valid(HttpMethods.OPTIONS)
    case "PATCH" => Valid(HttpMethods.PATCH)
    case "PUT" => Valid(HttpMethods.PUT)
    case "POST" => Valid(HttpMethods.POST)
    case "TRACE" => Valid(HttpMethods.TRACE)
    case um => Invalid(s"""Unsupported HTTP method "$um"""", method)
  }

  private def validateHeaders(
    headers: List[(String, String)]
  ): Validity[List[(String, String)], List[HttpHeader]] = {
    val (invalids, valids) = headers map {
      validateHeader(_)
    } partition {
      case Valid(_) => true
      case Invalid(_, _) => false
    }

    invalids.length match {
      case 0 => Valid(valids map { case Valid(header) => header })
      case n => Invalid(s"Found ${n} invalid headers.", headers)
    }
  }

  private def validateHeader(
    header: (String, String)
  ): Validity[(String, String), HttpHeader] = HttpHeader.parse(header._1, header._2) match {
    case Ok(header, errors) => Valid(header)
    case Error(error) => Invalid(error.detail, header)
  }

  def shutdown(): Unit = system.terminate()
}

object TryHttp {
  def tryHttp(): Unit = {
    val result = Await.result(Http.get("http://rocket:1337/"), Duration(5, TimeUnit.SECONDS))

    result match {
      case HttpResultRawResponse(response, _, _) => {
        val json = Json.obj(
          "status" -> response.status.intValue,
          "headers" -> Json.arr(response.headers.map(h =>
            h.name -> h.value
          )
          )
        )
        println(Json.prettyPrint(json))
      }
      case _ => println(s"Invalid request!")
    }

    Http.shutdown()
  }
}
