package controllers

import anorm.SQL
import anorm.SqlParser.scalar
import play.api.Logging
import play.api.db.Database
import play.api.mvc._

import java.io.File
import javax.inject.Inject

class Health @Inject()(implicit val
                       controllerComponents: ControllerComponents,
                       database: Database,
                      ) extends BaseController with Logging {

  def hc: Action[AnyContent] = Action {
    database.withConnection { implicit connection =>
      SQL("SELECT 1").as(scalar[Int].single)
    }

    val fileAbsolute = new File(".").getAbsoluteFile
    val total = fileAbsolute.getTotalSpace / 1024.0 / 1024
    val free = fileAbsolute.getFreeSpace / 1024.0 / 1024
    val percent = free / total * 100
    val logMessage: String = f"${free}%,.0f MiB / ${total}%,.0f MiB = $percent%.2f%% free"
    if (percent < 5) {
      logger.error(s"Health check failed: low disk space: $logMessage")
      InsufficientStorage("LOW_DISK_SPACE")
    } else {
      Ok("OK")
    }
  }
}
