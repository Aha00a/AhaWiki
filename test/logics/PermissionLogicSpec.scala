package logics

import org.scalatest.freespec.AnyFreeSpec

class PermissionLogicSpec extends AnyFreeSpec {

  import models.tables.Permission

  case class TargetActorAction(target: String, actor: String, action: Int)

  def createPermissionLogicWithLog(seqTargetActorAction: Seq[TargetActorAction]): PermissionLogic = {
    val permissionLogic = createPermissionLogic(seqTargetActorAction)
    System.out.println(permissionLogic.toLogString)
    permissionLogic
  }

  def createPermissionLogic(seqTargetActorAction: Seq[TargetActorAction]): PermissionLogic = {
    new PermissionLogic(seqTargetActorAction.reverse.zipWithIndex.map {
      case (t, i) => Permission(i + 1, t.target, t.actor, t.action)
    }.reverse)
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

  "permitted" - {
    "Public" in {
      val permissionLogic = createPermissionLogicWithLog(Seq(
        TargetActorAction("", actorAha00a, Permission.admin),
        TargetActorAction("", "", Permission.edit),
      ))

      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a)).map(to01).mkString === "11001")
      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a)) === "11000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a)) === "11111".map(_ == '1'))
    }

    "Open" in {
      val permissionLogic = createPermissionLogicWithLog(Seq(
        TargetActorAction("", actorAha00a, Permission.admin),
        TargetActorAction("", "@gmail.com", Permission.edit),
        TargetActorAction("", "", Permission.read),
      ))

      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a)) === "10000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a)) === "11000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a)) === "11111".map(_ == '1'))
    }

    "Closed" in {
      val permissionLogic = createPermissionLogicWithLog(Seq(
        TargetActorAction("", actorAha00a, Permission.admin),
        TargetActorAction("", "@gmail.com", Permission.edit),
        TargetActorAction("", "", Permission.none),
      ))

      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a)) === "11000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a)) === "11111".map(_ == '1'))
    }

    "Protected" in {
      val permissionLogic = createPermissionLogicWithLog(Seq(
        TargetActorAction("", actorAha00a, Permission.admin),
        TargetActorAction("", actorAha00a, Permission.edit),
        TargetActorAction("", actorSomeone, Permission.edit),
        TargetActorAction("", "", Permission.none),
      ))

      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a)) === "11000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a)) === "11111".map(_ == '1'))
    }

    "Private" in {
      val permissionLogic = createPermissionLogicWithLog(Seq(
        TargetActorAction("", "aha00a@gmail.com", Permission.admin),
        TargetActorAction("", "", Permission.none),
      ))

      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a)) === "11111".map(_ == '1'))
    }


    "aha00a" in {
      val permissionLogic = createPermissionLogicWithLog(Seq(
        TargetActorAction(targetPrivate, "", Permission.read),
        TargetActorAction(targetPrivate + "?", actorAha00a, Permission.admin),
        TargetActorAction(targetPrivate + "?", "", Permission.none),
        TargetActorAction("", actorAha00a, Permission.admin),
        TargetActorAction("", "@gmail.com", Permission.edit),
        TargetActorAction("", "", Permission.read),
      ))


      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorEmpty, a)) === "10000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorSomeone, a)) === "11000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetFrontPage, actorAha00a, a)) === "11111".map(_ == '1'))

      assert(seqAction.map(a => permissionLogic.permitted(targetPrivate, actorEmpty, a)) === "10000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetPrivate, actorSomeone, a)) === "10000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetPrivate, actorAha00a, a)) === "11111".map(_ == '1'))

      assert(seqAction.map(a => permissionLogic.permitted(targetPrivate + "Page", actorEmpty, a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetPrivate + "Page", actorSomeone, a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetPrivate + "Page", actorAha00a, a)) === "11111".map(_ == '1'))
    }

  }

}
