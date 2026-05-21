import actors.ActorAccessLog
import actors.ActorGeocode
import actors.ActorPageCalculator
import com.google.inject.AbstractModule
import play.api.libs.concurrent.PekkoGuiceSupport
import services.ApplicationLifecycleHook

// A Module is needed to register bindings
class Module extends AbstractModule with PekkoGuiceSupport {
  override def configure(): Unit = {
     bind(classOf[ApplicationLifecycleHook]).asEagerSingleton()
     bindActor[ActorPageCalculator]("actor-page-calculator")
     bindActor[ActorGeocode]("actor-geocode")
     bindActor[ActorAccessLog]("access-log-actor")
  }
}
