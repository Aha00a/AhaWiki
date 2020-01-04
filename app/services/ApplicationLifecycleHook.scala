package services

import java.io.File
import java.util.Date

import actors.ActorAhaWiki.{Calculate, CalculateCosineSimilarity, CalculateLink}
import akka.actor.{ActorRef, ActorSystem}
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import javax.inject._
import logics.SchemaOrg
import models.{AhaWikiQuery, Link, Page}
import play.api.Logger
import play.api.db.Database
import play.api.inject.ApplicationLifecycle

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Codec

@Singleton
class ApplicationLifecycleHook @Inject()(implicit
                                         applicationLifecycle: ApplicationLifecycle,
                                         actorSystem: ActorSystem,
                                         executionContext: ExecutionContext,
                                         database: Database,
                                         @Named("db-actor") actorAhaWiki: ActorRef
                                        ) {
  applicationLifecycle.addStopHook { () =>
    Logger.info("OnApplicationStop")
    Future.successful(())
  }

  Logger.info("OnApplicationStart")
  actorSystem.scheduler.scheduleOnce(2 second, () => { database.withConnection { implicit connection =>
    Logger.info("OnApplicationStarting")

    val ahaWikiQuery: AhaWikiQuery = AhaWikiQuery()
    if (0 == ahaWikiQuery.Page.selectCount()) {
      def getArrayPageFromFile: Array[Page] = {
        new File("app/assets/Page").listFiles().map(file => {
          val name = file.getName
          implicit val codec: Codec = Codec.UTF8
          val body = Using(scala.io.Source.fromFile(file))(_.mkString)
          Page(name, 1, new Date(), "AhaWiki", "127.0.0.1", body, "initial")
        })
      }

      getArrayPageFromFile.foreach(p => {
        ahaWikiQuery.pageInsert(p.name, p.revision, p.dateTime, p.author, p.remoteAddress, p.content, p.comment)
        actorAhaWiki ! Calculate(p.name)
      })
    }

    val count = ahaWikiQuery.Link.selectCountWhereAlias("subClassOf")
    
    val seqLink: Seq[Link] = SchemaOrg.seqClass.flatMap(c => c.subClassOf.map(s => Link(SchemaOrg.withNameSpace(c.id), SchemaOrg.withNameSpace(s), "subClassOf")))
    if(count < seqLink.length) {
      ahaWikiQuery.Link.insert(seqLink)
    }

    Logger.info("OnApplicationStarted")
  }})

  //noinspection LanguageFeature
  actorSystem.scheduler.schedule(30 seconds, 5 minutes, () => { database.withConnection { implicit connection =>
    val ahaWikiQuery: AhaWikiQuery = AhaWikiQuery()
    ahaWikiQuery.pageSelectNameWhereNoCosineSimilarity() match {
      case Some(s) => actorAhaWiki ! CalculateCosineSimilarity(s)
      case None => Logger.info("None")
    }
  }})

  //noinspection LanguageFeature
  actorSystem.scheduler.schedule(15 seconds, 30 seconds, () => { database.withConnection { implicit connection =>
    val ahaWikiQuery: AhaWikiQuery = AhaWikiQuery()
    val seq: Seq[String] = ahaWikiQuery.pageSelectNameWhereNoLinkSrc()
    for((v, i) <- seq.zipWithIndex) {
      actorAhaWiki ! CalculateLink(v, i, seq.length)
    }
  }})
}