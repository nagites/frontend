package frontpress

import com.amazonaws.services.s3.AmazonS3Client
import com.gu.contentapi.client.GuardianContentClient
import com.gu.contentapi.client.model._
import com.gu.facia.api.contentapi.ContentApi.AdjustSearchQuery
import com.gu.facia.api.models.Collection
import com.gu.facia.api.models.Collection
import com.gu.facia.api.models.CuratedContent
import com.gu.facia.api.utils._
import com.gu.facia.api.{FAPI, Response}
import com.gu.facia.api.models._
import com.gu.facia.client.{AmazonSdkS3Client, ApiClient}
import com.gu.facia.client.models.{Trail, TrailMetaData}
import common.ExecutionContexts
import conf.Configuration
import contentapi.QueryDefaults
import play.api.libs.json._

import scala.util.Try

private case class TestContentApiClient(override val apiKey: String, override val targetUrl: String) extends GuardianContentClient(apiKey)

trait IntegrationTestConfig extends ExecutionContexts {
  val apiKey: String = scala.sys.env.getOrElse("CONTENT_API_KEY", "")
  val targetUrl: Option[String] = scala.sys.env.get("FACIA_CLIENT_TARGET_URL")

  implicit val capiClient: GuardianContentClient =
    targetUrl.fold(ifEmpty = new GuardianContentClient(apiKey)){ targetUrl =>
      new TestContentApiClient(
        apiKey,
        targetUrl)}

  private val amazonS3Client = new AmazonS3Client()
  implicit val apiClient: ApiClient = ApiClient("aws-frontend-store", "FRANCIS", AmazonSdkS3Client(amazonS3Client))
}

object FapiFrontPress extends IntegrationTestConfig with QueryDefaults {
  implicit val frontImageFormat = Json.format[FrontImage]
  implicit object frontPriorityFormat extends Format[FrontPriority] {
    def reads(json: JsValue) = (json \ "type").transform[JsString](Reads.JsStringReads) match {
      case JsSuccess(JsString("EditorialPriority"), _) => JsSuccess(EditorialPriority)
      case JsSuccess(JsString("CommercialPriority"), _) => JsSuccess(CommercialPriority)
      case JsSuccess(JsString("TrainingPriority"), _) => JsSuccess(TrainingPriority)
      case e@JsError(_) => e
      case _ => JsError()
    }

    def writes(frontPriority: FrontPriority) = frontPriority match {
      case EditorialPriority => JsObject(Seq("type" -> JsString("EditorialPriority")))
      case CommercialPriority => JsObject(Seq("type" -> JsString("CommercialPriority")))
      case TrainingPriority => JsObject(Seq("type" -> JsString("TrainingPriority")))
    }
  }
  implicit val frontFormat = Json.format[Front]

  implicit val trailMetaDataFormat = Json.format[TrailMetaData]
  implicit val trailFormat = Json.format[Trail]
  implicit val groupFormat = Json.format[Group]
  implicit val newCollectionFormat = Json.format[Collection]

  implicit val podcastFormat = Json.format[Podcast]
  implicit val referenceFormat = Json.format[Reference]
  implicit val tagFormat = Json.format[Tag]
  implicit val assetFormat = Json.format[Asset]
  implicit val elementFormat = Json.format[Element]
  implicit val contentFormat = Json.format[Content]

  implicit object itemKickerFormat extends Format[ItemKicker] {
    def reads(json: JsValue) = (json \ "type").transform[JsString](Reads.JsStringReads) match {
      case JsSuccess(JsString("BreakingNewsKicker"), _) => JsSuccess(BreakingNewsKicker)
      case JsSuccess(JsString("LiveKicker"), _) => JsSuccess(LiveKicker)
      case JsSuccess(JsString("AnalysisKicker"), _) => JsSuccess(AnalysisKicker)
      case JsSuccess(JsString("ReviewKicker"), _) => JsSuccess(ReviewKicker)
      case JsSuccess(JsString("CartoonKicker"), _) => JsSuccess(CartoonKicker)
      //TODO: Podcast
      case _ => JsError()
    }

    def writes(itemKicker: ItemKicker) = itemKicker match {
      case BreakingNewsKicker => JsObject(Seq("type" -> JsString("BreakingNewsKicker")))
      case LiveKicker => JsObject(Seq("type" -> JsString("LiveKicker")))
      case AnalysisKicker => JsObject(Seq("type" -> JsString("AnalysisKicker")))
      case ReviewKicker => JsObject(Seq("type" -> JsString("ReviewKicker")))
      case CartoonKicker => JsObject(Seq("type" -> JsString("CartoonKicker")))
      case _ => JsString("Who Cares")
    }
  }

  implicit val imageCutoutFormat = Json.format[ImageCutout]
  implicit val imageFormat = Json.format[Image]

  implicit val latestSnapFormat = Json.format[LatestSnap]
  implicit val linkSnapFormat = Json.format[LinkSnap]

  implicit object faciaContentFormat extends Format[FaciaContent] {
    def reads(json: JsValue) = (json \ "type").transform[JsString](Reads.JsStringReads) match {
      case JsSuccess(JsString("LinkSnap"), _) => JsSuccess(json.as[LinkSnap])
      case JsSuccess(JsString("LatestSnap"), _) => JsSuccess(json.as[LatestSnap])
      case JsSuccess(JsString("CuratedContent"), _) => JsSuccess(json.as[CuratedContent])
      case JsSuccess(JsString("SupportingCuratedContent"), _) => JsSuccess(json.as[SupportingCuratedContent])
      case _ => JsError()
    }

    def writes(faciaContent: FaciaContent) = faciaContent match {
      case linkSnap: LinkSnap => Json.toJson(linkSnap)(linkSnapFormat)
      case latestSnap: LatestSnap => Json.toJson(latestSnap)(latestSnapFormat)
      case content: CuratedContent => Json.toJson(content)(curatedContentFormat)
      case supportingContent: SupportingCuratedContent => Json.toJson(supportingContent)(supportingCuratedContentFormat)
      case _ => JsNull
    }
  }

  implicit val curatedContentFormat = Json.format[CuratedContent]
  implicit val supportingCuratedContentFormat = Json.format[SupportingCuratedContent]



//  implicit object faciaContentFormat extends Format[FaciaContent] {
//    def reads(json: JsValue) = ((json \ "type").asOpt[String], json.transform[JsObject](Reads.JsObjectReads)) match {
//      case (Some("LinkSnap"), JsSuccess(jsObj, _)) =>
//        Try(JsSuccess(
//          LinkSnap(
//            (jsObj \ "id").as[String],
//            (jsObj \ "snapUri").asOpt[String],
//            (jsObj \ "snapCss").asOpt[String])))
//        .getOrElse(JsError())
//      case (Some("LatestSnap"), JsSuccess(jsObj, _)) =>
//        Try(JsSuccess(
//          LatestSnap(
//            (jsObj \ "id").as[String],
//            (jsObj \ "snapUri").asOpt[String],
//            (jsObj \ "snapCss").asOpt[String],
//            (jsObj \ "latestContent").asOpt[Content])))
//        .getOrElse(JsError())
//      case (Some("LinkSnap"), JsSuccess(jsObj, _)) =>
//        Try(JsSuccess(
//          CuratedContent(
//            (jsObj \ "content").as[Content],
//            (jsObj \ "supportingContent").as[List[FaciaContent]],
//            (jsObj \ "headline").as[String],
//            (jsObj \ "href").asOpt[String],
//            (jsObj \ "trailText").asOpt[String],
//            (jsObj \ "group").as[String],
//            (jsObj \ "image").asOpt[Image],
//            (jsObj \ "isBreaking").as[Boolean],
//            (jsObj \ "isBoosted").as[Boolean],
//            (jsObj \ "imageHide").as[Boolean],
//            (jsObj \ "imageReplace").as[Boolean],
//            (jsObj \ "showMainVideo").as[Boolean],
//            (jsObj \ "showKickerTag").as[Boolean],
//            (jsObj \ "byLine").asOpt[String],
//            (jsObj \ "showByLine").as[Boolean],
//            (jsObj \ "kicker").asOpt[ItemKicker],
//            (jsObj \ "imageCutOut").asOpt[ImageCutout],
//            (jsObj \ "showBoostedHeadline").as[Boolean],
//            (jsObj \ "showQuotedHeadline").as[Boolean])))
//          .getOrElse(JsError())
//      case (Some("LinkSnap"), JsSuccess(jsObj, _)) =>
//        Try(JsSuccess(
//          SupportingCuratedContent(
//            (jsObj \ "content").as[Content],
//            (jsObj \ "headline").as[String],
//            (jsObj \ "href").asOpt[String],
//            (jsObj \ "trailText").asOpt[String],
//            (jsObj \ "group").as[String],
//            (jsObj \ "image").asOpt[Image],
//            (jsObj \ "isBreaking").as[Boolean],
//            (jsObj \ "isBoosted").as[Boolean],
//            (jsObj \ "imageHide").as[Boolean],
//            (jsObj \ "imageReplace").as[Boolean],
//            (jsObj \ "showMainVideo").as[Boolean],
//            (jsObj \ "showKickerTag").as[Boolean],
//            (jsObj \ "byLine").asOpt[String],
//            (jsObj \ "showByLine").as[Boolean],
//            (jsObj \ "kicker").asOpt[ItemKicker],
//            (jsObj \ "imageCutOut").asOpt[ImageCutout],
//            (jsObj \ "showBoostedHeadline").as[Boolean],
//            (jsObj \ "showQuotedHeadline").as[Boolean])))
//          .getOrElse(JsError())
//      case _ => JsError()
//    }
//
//    def writes(faciaContent: FaciaContent) = faciaContent match {
//      case snap: LatestSnap =>
//        JsObject(
//          Seq(
//            ("type", JsString("LatestSnap"),
//            ("id", JsString(snap.id)),
//            ("snapUri", JsString(snap.snapUri)),
//            ("snapCss", JsString(snap.snapCss)))))
//      case snap: LinkSnap => JsObject(Seq("type" -> JsString("CommercialPriority")))
//      case curatedContent: CuratedContent => JsObject(Seq("type" -> JsString("TrainingPriority")))
//      case supportingCuratedContent: SupportingCuratedContent => JsObject(Seq("type" -> JsString("TrainingPriority")))
//    }
//  }

  val showFields = "body,trailText,headline,shortUrl,liveBloggingNow,thumbnail,commentable,commentCloseDate,shouldHideAdverts,lastModified,byline,standfirst,starRating,showInRelatedContent,internalContentCode"
  val apiQuery: AdjustSearchQuery = (searchQuery: SearchQuery) =>
    searchQuery
      .showFields(showFields)
      .showElements("all")
      .pageSize(Configuration.faciatool.frontPressItemSearchBatchSize)
      .showTags("all")
      .showReferences(references)

  def generateFrontJsonFromFapiClient(): Response[JsValue] =
    for {
      frontsSet <- FAPI.getFronts()
    } yield Json.toJson(frontsSet.toList)

  def generateCollectionJsonFromFapiClient(collectionId: String): Response[JsValue] =
    for {
      collection <- FAPI.getCollection(collectionId)
      backfill <- getBackfill(collection)
      curatedCollection <- FAPI.collectionContentWithoutSnaps(collection, apiQuery)
    } yield Json.obj("curated" -> Json.toJson(collection), "backfill" -> Json.toJson(backfill))

  private def getBackfill(collection: Collection): Response[List[FaciaContent]] =
    collection
      .apiQuery
      .map { query =>
      FAPI.backfill(query, collection, apiQuery)}
      .getOrElse{Response.Right(Nil)}
}
