package logics

import org.scalatest.freespec.AnyFreeSpec

class PermissionLogicSpec extends AnyFreeSpec {

  import com.aha00a.commons.Implicits._
  import models.tables.Permission

  def createPermissionLogicWithLog(title: String, seqPermission: Seq[Permission]): PermissionLogic = {
    val permissionLogic = new PermissionLogic(seqPermission)
    System.out.println(permissionLogic.toLogString(title))
    permissionLogic
  }

  val targetFrontPage = "FrontPage"
  val targetPrivate = "Private"

  val actorEmpty = ""
  val actorSomeone = "aha00a+someone@gmail.com"
  val actorAha00a = "aha00a@gmail.com"

  val seqAction: Seq[Int] = Seq(
    Permission.read,
    Permission.edit,
    Permission.create,
    Permission.upload,
    Permission.delete,
  )

  def to01(s: Seq[Boolean]): String = s.map(_.to01).mkString

  "permitted" - {
    "Public" in {
      val permissionLogic = createPermissionLogicWithLog("Public", Seq(
        Permission("", actorAha00a, Permission.admin),
        Permission("", "", Permission.edit),
      ))

      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a))) === "11000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a))) === "11000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a))) === "11111")
    }

    "Open" in {
      val permissionLogic = createPermissionLogicWithLog("Open", Seq(
        Permission("", actorAha00a, Permission.admin),
        Permission("", "@gmail.com", Permission.edit),
        Permission("", "", Permission.read),
      ))

      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a))) === "10000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a))) === "11000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a))) === "11111")
    }

    "Closed" in {
      val permissionLogic = createPermissionLogicWithLog("Closed", Seq(
        Permission("", actorAha00a, Permission.admin),
        Permission("", "@gmail.com", Permission.edit),
        Permission("", "", Permission.none),
      ))

      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a))) === "00000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a))) === "11000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a))) === "11111")
    }

    "Protected" in {
      val permissionLogic = createPermissionLogicWithLog("Protected", Seq(
        Permission("", actorAha00a, Permission.admin),
        Permission("", actorSomeone, Permission.edit),
        Permission("", "", Permission.none),
      ))

      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a))) === "00000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a))) === "11000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a))) === "11111")
    }

    "Private" in {
      val permissionLogic = createPermissionLogicWithLog("Private", Seq(
        Permission("", "aha00a@gmail.com", Permission.admin),
        Permission("", "", Permission.none),
      ))

      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a))) === "00000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a))) === "00000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a))) === "11111")
    }


    "aha00a" in {
      val permissionLogic = createPermissionLogicWithLog("aha00a", Seq(
        Permission(targetPrivate, "", Permission.read),
        Permission(targetPrivate + "?", actorAha00a, Permission.admin),
        Permission(targetPrivate + "?", "", Permission.none),
        Permission("", actorAha00a, Permission.admin),
        Permission("", "@gmail.com", Permission.edit),
        Permission("", "", Permission.read),
      ))


      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a))) === "10000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a))) === "11000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a))) === "11111")

      assert(to01(seqAction.map(a => permissionLogic.permitted(targetPrivate, actorEmpty, a))) === "10000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetPrivate, actorSomeone, a))) === "10000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetPrivate, actorAha00a, a))) === "11111")

      assert(to01(seqAction.map(a => permissionLogic.permitted(targetPrivate + "Page", actorEmpty, a))) === "00000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetPrivate + "Page", actorSomeone, a))) === "00000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetPrivate + "Page", actorAha00a, a))) === "11111")
    }

  }

}
