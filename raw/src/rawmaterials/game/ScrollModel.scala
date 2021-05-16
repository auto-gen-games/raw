package rawmaterials.game

import indigo.Outcome
import indigo.shared.events.MouseEvent
import indigo.shared.events.MouseEvent.Move
import rawmaterials.Utilities.moved
import rawmaterials.world.Position

case class ScrollModel (topLeft: Position, rowOffset: Int, columnOffset: Int,
                        scrollingFrom: Option[MouseEvent])

object ScrollModel {
  val initial: ScrollModel = ScrollModel ((0, 0), 0, 0, None)

  def scroll (model: ScrollModel, event: Move, rows: Int, columns: Int): ScrollModel = {
    model.scrollingFrom match {
      case None => model
      case Some (mouseEvent) =>
        val dx = mouseEvent.x - event.x
        val dy = mouseEvent.y - event.y
        var newRowOffset = model.rowOffset + dy
        var newColumnOffset = model.columnOffset + dx
        var newPosition = model.topLeft
        while (newRowOffset < 0) {
          newRowOffset = 64 + newRowOffset
          newPosition = moved (newPosition, drow = -1, dcolumn = 0, rows, columns)
        }
        while (newRowOffset >= 64) {
          newRowOffset = newRowOffset - 64
          newPosition = moved (newPosition, drow = 1, dcolumn = 0, rows, columns)
        }
        while (newColumnOffset < 0) {
          newColumnOffset = 64 + newColumnOffset
          newPosition = moved (newPosition, drow = 0, dcolumn = -1, rows, columns)
        }
        while (newColumnOffset >= 64) {
          newColumnOffset = newColumnOffset - 64
          newPosition = moved (newPosition, drow = 0, dcolumn = 1, rows, columns)
        }
        model.copy (topLeft = newPosition, rowOffset = newRowOffset, columnOffset = newColumnOffset, scrollingFrom = Some (event))
    }
  }

}
