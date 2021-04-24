package rawmaterials.game

import indigo.GameViewport
import indigo.shared.events.MouseEvent
import rawmaterials.world.Position

case class ViewModel (viewport: GameViewport, topLeft: Position, rowOffset: Int, columnOffset: Int,
                      scrollingFrom: Option[MouseEvent], logMessage: String)

object ViewModel {
  def apply (viewport: GameViewport): ViewModel = ViewModel (viewport, (0, 0), 0, 0, None, "")
}
