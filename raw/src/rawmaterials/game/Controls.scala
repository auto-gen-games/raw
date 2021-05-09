package rawmaterials.game

import indigo.shared.input.Mouse
import indigo.{GameViewport, Graphic, Group, Overlay, RGBA, Text}
import rawmaterials.Utilities.moved
import rawmaterials.game.GameAssets.{decreaseButton, fontKey, increaseButton, log, menuBackground, optionIcons, optionPos, optionsBackground, producers}
import rawmaterials.game.PlayScene.drawBar
import rawmaterials.world.{Amount, Defence, Hub, Position, ProducerBuilder, ProducerFeed, Siege, Sink, Task}

object Controls {
  sealed trait ValueCell
  case class NumberCell   (value: Long) extends ValueCell
  case class AllocateCell (value: Long) extends ValueCell
  case class MaterialCell (amount: Long, material: String) extends ValueCell

  def infoLines (position: Position, model: GameModel): List[(String, ValueCell)] = {
    import model._
    import world.terrain
    def allocated (constraints: Task => Boolean): AllocateCell =
      AllocateCell (world.allocationsAt (position, material).filter (ta => constraints (ta._1)).map (_._2).sum)
    def allocatedTo (sink: Sink): AllocateCell =
      allocated (_.sink == sink)
    if (!model.militaryView)
      model.allocateView match {
        case DepositsView => List (
          ("Strength", NumberCell (terrain.materialStrength (material))),
          ("Deposit", NumberCell (terrain.deposit (position, material).toLong)))
        case ProducersView =>
          val (upgradeMaterial, upgradeAmount) = world.producerUpgradeCostAt (material, position)
          List (
            ("Producers", NumberCell (world.producer (position, material))),
            ("Upgrade",   MaterialCell (upgradeAmount, terrain.materialName (upgradeMaterial))),
            ("Stored",    NumberCell (world.reserve (position, Hub, material))))
        case BalanceView =>
          val made = world.producer (position, material) * model.world.terrain.deposit (position, material).toLong
          val out  = world.allocationsAt (position, material).map (_._2).sum
          val in   = world.allocatedImports (position, material).map (_._2).sum
          List (
            ("Produced",  NumberCell (made)),
            ("Allocated", NumberCell (out)),
            ("Incoming",  NumberCell (in)),
            ("Balance",   NumberCell (made + in - out)))
        case BuildView =>
          world.terrain.producedUsing (material)
            .map (produced => (s"Feed ${world.terrain.materialName (produced)} mine",
              allocatedTo (ProducerFeed (produced)))).toList ++
            world.producerBuildsPossibleFrom (position, material)
              .map (produced => (s"Build ${world.terrain.materialName (produced)} mine",
                allocatedTo (ProducerBuilder (produced))))
        case TransportView =>
          val exports = world.allocatedExports (position, material)
          def exportTotal (drow: Int, dcolumn: Int): Amount =
            exports
              .filter (_._1.target == moved (position, drow, dcolumn, world))
              .map (_._2).sum
          List (
            ("Transport north", AllocateCell (exportTotal (-1, 0))),
            ("Transport east",  AllocateCell (exportTotal ( 0, 1))),
            ("Transport south", AllocateCell (exportTotal ( 1, 0))),
            ("Transport west",  AllocateCell (exportTotal ( 0, -1)))
          )
        case SiegeView =>
          val lord = world.owner (position)
          List (
            ("Feed defence", allocatedTo (Defence)),
            ("Siege north",  allocated (task => task.sink == Siege (lord) && task.target == moved (position, -1, 0, world))),
            ("Siege east",   allocated (task => task.sink == Siege (lord) && task.target == moved (position, 0, 1, world))),
            ("Siege south",  allocated (task => task.sink == Siege (lord) && task.target == moved (position, 1, 0, world))),
            ("Siege west",   allocated (task => task.sink == Siege (lord) && task.target == moved (position, 0, -1, world)))
          )
        case _ => List (("Undefined", NumberCell (0L)))
      }
    else
      ("Defence", NumberCell (world.reserve (position, Defence, material))) ::
        world.siegesAt (position).map (la => (s"Siege by ${la._1.name}", NumberCell (la._2)))
  }

  def tintIfHover (graphic: Graphic, x: Int, y: Int, mouse: Mouse): Graphic =
    if (mouse.position.x >= x && mouse.position.x < x + 16 && mouse.position.y >= y && mouse.position.y < y + 16)
      graphic.withTint (RGBA.Magenta)
    else graphic

  def titleBar (mouse: Mouse): Group =
    Group (
      Group (optionIcons.indices.map { index =>
        val x = optionPos (index)
        tintIfHover (optionsBackground.moveTo (x, 0).withTint (RGBA (0.3, 0.3, 0.3)).withAlpha (0.9), x, 0, mouse)
      }.toList),
      Group (optionIcons.zipWithIndex.map { case (image, index) => image.moveTo (optionPos (index), 0) })
    )


  def showInfo (lines: List[(String, ValueCell)], mouse: Mouse): Group =
    Group (
      lines.zipWithIndex.flatMap {
        case ((heading, value), index) =>
          List (Text (heading, 200, index * 20 + 18, 1, fontKey).withOverlay (Overlay.Color (RGBA.White)),
            value match {
              case NumberCell (value) =>
                Text (value.toString, 375, index * 20 + 18, 1, fontKey).withOverlay (Overlay.Color (RGBA.White))
              case AllocateCell (value) =>
                Group (
                  tintIfHover (decreaseButton.moveTo (375, index * 20 + 20), 375, index * 20 + 20, mouse),
                  Text (value.toString, 395, index * 20 + 18, 1, fontKey).withOverlay (Overlay.Color (RGBA.Yellow)),
                  tintIfHover (increaseButton.moveTo (415, index * 20 + 18), 415, index * 20 + 20, mouse)
                )
              case MaterialCell (amount, material) =>
                Text (amount + " " + material, 375, index * 20 + 18, 1, fontKey).withOverlay (Overlay.Color (RGBA.White))
            })
      }
    )

  def infoBar (position: Position, model: GameModel, viewport: GameViewport, mouse: Mouse): Group = {
    val lines = infoLines (position, model)
    Group (
      drawBar (menuBackground, 32, (0, 18), 200, 1, RGBA (0.3, 0.3, 0.3)),
      drawBar (log, 20, (200, 18), viewport.width - 200, lines.size, RGBA (0.3, 0.3, 0.3)),
      (if (!model.militaryView)
        Group (producers (model.material).moveTo (0, 18),
          Text (model.world.terrain.materialName (model.material), 36, 22, 1, fontKey).withOverlay (Overlay.Color (RGBA.White)))
      else
        Text ("military", 36, 22, 1, fontKey).withOverlay (Overlay.Color (RGBA.White))),
      showInfo (lines, mouse)
    )
  }

  def controls (position: Position, model: GameModel, viewport: GameViewport, mouse: Mouse): Group =
    Group (titleBar (mouse), infoBar (position, model, viewport, mouse))

}
