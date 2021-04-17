package rawmaterials.world

sealed trait Sink
case object Hub extends Sink
case class ProducerBuilder (material: Material) extends Sink
case class ProducerFeed (material: Material) extends Sink
trait MilitarySink extends Sink
case object Defence extends MilitarySink
case class Siege (lord: Lord) extends MilitarySink