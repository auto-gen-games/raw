package rawmaterials.world

sealed trait Occurrence
case class ProducerBuilt   (position: Position, material: Material) extends Occurrence
case class SectorConquered (position: Position, lord: Lord)         extends Occurrence
