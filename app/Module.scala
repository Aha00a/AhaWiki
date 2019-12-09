import actors.ActorAhaWiki
import com.google.inject.AbstractModule
import play.api.libs.concurrent.AkkaGuiceSupport
import services.ApplicationLifecycleHook

// A Module is needed to register bindings
class Module extends AbstractModule with AkkaGuiceSupport {
  override def configure(): Unit = {
     bind(classOf[ApplicationLifecycleHook]).asEagerSingleton()
     bindActor[ActorAhaWiki]("db-actor")
  }
}