package mesosphere.marathon

import mesosphere.chaos.AppConfiguration
import mesosphere.chaos.http.HttpConf
import mesosphere.marathon.core.plugin.PluginManagerConfiguration
import mesosphere.marathon.event.EventConfiguration
import mesosphere.marathon.event.http.HttpEventConfiguration
import mesosphere.marathon.metrics.MetricsReporterConf
import org.rogach.scallop.ScallopConf

class AllConf(args: Seq[String] = Nil)
    extends ScallopConf(args)
    with MetricsReporterConf
    with HttpConf
    with MarathonConf
    with AppConfiguration
    with EventConfiguration
    with HttpEventConfiguration
    with DebugConf
    with PluginManagerConfiguration

object AllConf {
  //Set if main is initialized. Allow test to override this value.
  @volatile private[marathon] var SuppliedOptionNames: Set[String] = Set.empty
  def suppliedOptionNames: Set[String] = SuppliedOptionNames
}
