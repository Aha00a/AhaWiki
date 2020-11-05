package logics

import org.scalatest.freespec.AnyFreeSpec

class PermissionLogicSpec extends AnyFreeSpec {

  import models.tables.Permission

  case class TargetActorAction(target: String, actor: String, action: Int)

  def createPermissionLogicWithLog(title: String, seqTargetActorAction: Seq[TargetActorAction]): PermissionLogic = {
    val permissionLogic = createPermissionLogic(seqTargetActorAction)
    System.out.println(permissionLogic.toLogString(title))
    permissionLogic
  }

  def createPermissionLogic(seqTargetActorAction: Seq[TargetActorAction]): PermissionLogic = {
    new PermissionLogic(seqTargetActorAction.map(t => Permission(t.target, t.actor, t.action)))
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

  def to01(b: Boolean): String = if(b) "1" else "0"
  def to01(s: Seq[Boolean]): String = s.map(to01).mkString

  "permitted" - {
    "Public" in {
      val permissionLogic = createPermissionLogicWithLog("Public", Seq(
        TargetActorAction("", actorAha00a, Permission.admin),
        TargetActorAction("", "", Permission.edit),
      ))

      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a))) === "11000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a))) === "11000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a))) === "11111")
    }

    "Open" in {
      val permissionLogic = createPermissionLogicWithLog("Open", Seq(
        TargetActorAction("", actorAha00a, Permission.admin),
        TargetActorAction("", "@gmail.com", Permission.edit),
        TargetActorAction("", "", Permission.read),
      ))

      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a))) === "10000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a))) === "11000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a))) === "11111")
    }

    "Closed" in {
      val permissionLogic = createPermissionLogicWithLog("Closed", Seq(
        TargetActorAction("", actorAha00a, Permission.admin),
        TargetActorAction("", "@gmail.com", Permission.edit),
        TargetActorAction("", "", Permission.none),
      ))

      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a))) === "00000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a))) === "11000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a))) === "11111")
    }

    "Protected" in {
      val permissionLogic = createPermissionLogicWithLog("Protected", Seq(
        TargetActorAction("", actorAha00a, Permission.admin),
        TargetActorAction("", actorSomeone, Permission.edit),
        TargetActorAction("", "", Permission.none),
      ))

      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a))) === "00000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a))) === "11000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a))) === "11111")
    }

    "Private" in {
      val permissionLogic = createPermissionLogicWithLog("Private", Seq(
        TargetActorAction("", "aha00a@gmail.com", Permission.admin),
        TargetActorAction("", "", Permission.none),
      ))

      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a))) === "00000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a))) === "00000")
      assert(to01(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a))) === "11111")
    }


    "aha00a" in {
      val permissionLogic = createPermissionLogicWithLog("aha00a", Seq(
        TargetActorAction(targetPrivate, "", Permission.read),
        TargetActorAction(targetPrivate + "?", actorAha00a, Permission.admin),
        TargetActorAction(targetPrivate + "?", "", Permission.none),
        TargetActorAction("", actorAha00a, Permission.admin),
        TargetActorAction("", "@gmail.com", Permission.edit),
        TargetActorAction("", "", Permission.read),
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
