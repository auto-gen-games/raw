package rawmaterials.game

import indigo.shared.events.MouseEvent
import rawmaterials.world.Position

case class ViewModel (topLeft: Position, rowOffset: Int, columnOffset: Int, scrollingFrom: Option[MouseEvent],
                      logMessage: String)

object ViewModel {
  def apply (): ViewModel = ViewModel ((0, 0), 0, 0, None, "")
}
