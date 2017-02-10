package mesosphere.marathon.state

import org.apache.mesos.{Protos => mesos}

case class Parameter(key: String, value: String) {

  def toProto(): mesos.Parameter =
    mesos.Parameter.newBuilder.setKey(key).setValue(value).build
}

object Parameter {
  def apply(proto: mesos.Parameter): Parameter =
    Parameter(
        proto.getKey,
        proto.getValue,
    )
}
