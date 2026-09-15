import actors.ActorAccessLog
import actors.ActorGeocode
import actors.ActorPageCalculator
import com.google.inject.AbstractModule
import logics.CrossInstanceBus
import play.api.libs.concurrent.PekkoGuiceSupport
import services.ApplicationLifecycleHook

// A Module is needed to register bindings
class Module extends AbstractModule with PekkoGuiceSupport {
  override def configure(): Unit = {
     bind(classOf[ApplicationLifecycleHook]).asEagerSingleton()
     // Eager so the Redis pub/sub subscriber is up at boot, before this instance's first save,
     // and so it hears page.updated from the other instance from the start.
     bind(classOf[CrossInstanceBus]).asEagerSingleton()
     bindActor[ActorPageCalculator]("actor-page-calculator")
     bindActor[ActorGeocode]("actor-geocode")
     bindActor[ActorAccessLog]("access-log-actor")
  }
}
