import com.google.inject.AbstractModule
import com.google.inject.name.Names
import services.ApplicationLifecycleHook

// A Module is needed to register bindings
class Module extends AbstractModule {
  override def configure() = {
     bind(classOf[ApplicationLifecycleHook]).asEagerSingleton()
  }
}