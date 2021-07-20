package rawmaterials.game

import indigo.shared.events.GlobalEvent
import rawmaterials.world._

case class OccurrenceEvent (occurrence: Occurrence, world: World) extends GlobalEvent {
  val message: String = occurrence match {
    case ProducerBuilt (position, material) =>
      s"Producer of ${world.terrain.materialName (material)} built at ${world.terrain.locationName (position)} to level ${world.producer (position, material)}${world.owner.get (position).map (" by " + _.name).getOrElse ("")}"
    case SectorConquered (position, lord) =>
      s"Sector ${world.terrain.locationName (position)} conquered by ${lord.name}"
  }
}
