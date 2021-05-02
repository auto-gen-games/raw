package rawmaterials.game

import indigo._
import indigo.scenes.{Lens, Scene, SceneName}
import indigo.shared.events.MouseEvent.{Click, MouseDown, MouseUp, Move}
import indigo.shared.input.Mouse
import rawmaterials.Settings._
import rawmaterials.Utilities.{moved, within}
import rawmaterials.game.GameAssets.{base, cell, defence, fontKey, log, menuBackground, noProduction, optionGroup, optionIcons, optionPos, optionsBackground, producers, siege}
import rawmaterials.world.{Material, Position, Terrain, World}

object PlayScene extends Scene[ReferenceData, GameModel, ViewModel] {
  type SceneModel                               = GameModel
  type SceneViewModel                           = ViewModel
  val name: SceneName                           = SceneName ("PlayScene")
  val modelLens: Lens[GameModel, GameModel]     = Lens.keepLatest
  val viewModelLens: Lens[ViewModel, ViewModel] = Lens.keepLatest
  val eventFilters: EventFilters                = EventFilters.AllowAll
  val subSystems: Set[SubSystem]                = Set.empty

  def updateModel (context: FrameContext[ReferenceData], model: GameModel): GlobalEvent => Outcome[GameModel] = {
    case FrameTick =>
      val (newModel, occurrences) = GameModel.update (model, context.gameTime)
      Outcome (newModel).addGlobalEvents (occurrences)
    case Click (x, y) =>
      if (model.zone.nonEmpty)
        optionOver (x, y) match {
          case Some (index) =>
            if (index == 0)
              Outcome (model.copy (material = within (model.material + 1, model.world.terrain.materials.size)))
            else Outcome (model)
          case None => Outcome (model)
        }
      else Outcome (model)
    case _ => Outcome (model)
  }

  def updateViewModel (context: FrameContext[ReferenceData], model: GameModel, viewModel: ViewModel): GlobalEvent => Outcome[ViewModel] = {
    case event@MouseDown (_, _) => Outcome (viewModel.copy (scrollingFrom = Some (event)))
    case MouseUp (_, _) => Outcome (viewModel.copy (scrollingFrom = None))
    case event@Move (newX, newY) =>
      viewModel.scrollingFrom match {
        case None => Outcome (viewModel)
        case Some (mouseEvent) =>
          val dx = mouseEvent.x - newX
          val dy = mouseEvent.y - newY
          var newRowOffset = viewModel.rowOffset + dy
          var newColumnOffset = viewModel.columnOffset + dx
          var newPosition = viewModel.topLeft
          while (newRowOffset < 0) {
            newRowOffset = 64 + newRowOffset
            newPosition = moved (newPosition, drow = -1, dcolumn = 0, model.world.terrain.rows, model.world.terrain.columns)
          }
          while (newRowOffset >= 64) {
            newRowOffset = newRowOffset - 64
            newPosition = moved (newPosition, drow = 1, dcolumn = 0, model.world.terrain.rows, model.world.terrain.columns)
          }
          while (newColumnOffset < 0) {
            newColumnOffset = 64 + newColumnOffset
            newPosition = moved (newPosition, drow = 0, dcolumn = -1, model.world.terrain.rows, model.world.terrain.columns)
          }
          while (newColumnOffset >= 64) {
            newColumnOffset = newColumnOffset - 64
            newPosition = moved (newPosition, drow = 0, dcolumn = 1, model.world.terrain.rows, model.world.terrain.columns)
          }
          Outcome (viewModel.copy (topLeft = newPosition, rowOffset = newRowOffset, columnOffset = newColumnOffset, scrollingFrom = Some (event)))
      }
    case occurrence @ OccurrenceEvent (_, _) =>
      println (occurrence.message)
      Outcome (viewModel.copy (logMessage = occurrence.message))
    case _ => Outcome (viewModel)
  }

  def optionOver (x: Int, y: Int): Option[Int] =
    if (y < 0 || y >= 16) None
    else
      optionIcons.indices.find { index =>
        val optionX = optionPos (index)
        x >= optionX && x < optionX + 16
      }

  def rowsVisible    (viewport: GameViewport): Int = (viewport.height - 20) / 64
  def columnsVisible (viewport: GameViewport): Int = viewport.width / 64

  def background (viewport: GameViewport): Group =
    Group ((for (row <- 0 to rowsVisible (viewport) + 1; column <- 0 to columnsVisible (viewport) + 1) yield
      cell.moveTo (column * 64, row * 64)).toList)

  def materialAlpha (material: Material, materials: Int): Double =
    ((maxDefenceAlpha - minDefenceAlpha) / (materials - 1)) * material + minDefenceAlpha

  def bases (model: GameModel, viewModel: ViewModel): Group =
    Group ((
      for (row <- 0 to rowsVisible (viewModel.viewport); column <- 0 to columnsVisible (viewModel.viewport)) yield {
        val position = moved ((row, column), viewModel.topLeft._1, viewModel.topLeft._2, model.world.terrain.rows, model.world.terrain.columns)
        val x = column * 64 - viewModel.columnOffset
        val y = row * 64 - viewModel.rowOffset
        model.world.owner.get (position).map { lord =>
          val produced = model.world.producersAt (position)
          val predominant = if (produced.map (_._2).sum == 0) noProduction else producers (produced.maxBy (_._2)._1)
          val maxSiege = model.world.siegesAt (position).filter (_._2 > 0L).sortBy (_._2).reverse.headOption.map (_._1)
          val defences = model.world.defenceMaterialsAt (position).filter (_._2 > 0L).map (_._1).sorted.reverse.headOption
          Group (List (
            Some (base.moveTo (x, y)),
            defences.map (material => defence.moveTo (x, y).withAlpha (materialAlpha (material, model.world.terrain.materials.size))),
            maxSiege.map (attacker => siege.moveTo (x, y).withTint (attacker.flag)),
            Some (predominant.moveTo (x + 16, y + 16).withTint (lord.flag))
          ).flatten)
        }
      }).toList.flatten)

  def logbarY (viewport: indigo.GameViewport): Int =
    viewport.height - 20

  def drawBar (graphic: Graphic, graphicWidth: Int, topLeft: (Int, Int), barWidth: Int, rows: Int, tint: RGBA): Group =
    Group ((for (x <- 0 to Math.ceil (barWidth / graphicWidth.toDouble).toInt; y <- 0 until rows) yield
      graphic.moveTo (topLeft._1 + x * graphicWidth, topLeft._2 + y * graphicWidth).withTint (tint).withAlpha (0.5)).toList)

  def logbar (viewport: GameViewport): Group =
    drawBar (log, 20, (0, logbarY (viewport)), viewport.width, 1, RGBA.White)

  def tintIfHover (graphic: Graphic, x: Int, mouse: Mouse): Graphic =
    if (mouse.position.x >= x && mouse.position.x < x + 16 && mouse.position.y >= 0 && mouse.position.y < 16)
      graphic.withTint (RGBA.Magenta)
    else graphic

  def titleBar (mouse: Mouse): Group =
    Group (
      Group (optionIcons.indices.map { index =>
        val x = optionPos (index)
        tintIfHover (optionsBackground.moveTo (x, 0).withTint (RGBA (0.3, 0.3, 0.3)).withAlpha (0.9), x, mouse)
      }.toList),
      Group (optionIcons.zipWithIndex.map { case (image, index) => image.moveTo (optionPos (index), 0) })
    )

  def infoLines (position: Position, material: Material, view: AllocateView, world: World): List[(String, Long)] =
    view match {
      case DepositsView => List (
        ("Strength", world.terrain.materialStrength (material)),
        ("Deposit", world.terrain.deposit (position, material).toLong))
      case _ => List (("Undefined", 0L))
    }

  def showInfo (lines: List[(String, Long)]): Group =
    Group (
      lines.zipWithIndex.flatMap {
        case ((heading, value), index) =>
          List (Text (heading, 200, index * 20 + 18, 1, fontKey).withOverlay (Overlay.Color (RGBA.White)),
            Text (value.toString, 400, index * 20 + 18, 1, fontKey).withOverlay (Overlay.Color (RGBA.White)))
      }
    )

  def infoBar (position: Position, material: Material, view: AllocateView, world: World, viewport: GameViewport): Group = {
    val lines = infoLines (position, material, view, world)
    Group (
      drawBar (menuBackground, 32, (0, 18), 200, 1, RGBA (0.3, 0.3, 0.3)),
      drawBar (log, 20, (200, 18), viewport.width - 200, lines.size, RGBA (0.3, 0.3, 0.3)),
      producers (material).moveTo (0, 18),
      Text (world.terrain.materialName (material), 36, 22, 1, fontKey).withOverlay (Overlay.Color (RGBA.White)),
      showInfo (lines)
    )
  }

  def controls (position: Position, material: Material, view: AllocateView, world: World, viewport: GameViewport, mouse: Mouse): Group =
    Group (titleBar (mouse), infoBar (position, material, view, world, viewport))

  def present (context: FrameContext[ReferenceData], model: GameModel, viewModel: ViewModel): Outcome[SceneUpdateFragment] = {
    Outcome {
      val worldScene =
        SceneUpdateFragment.empty
          .addGameLayerNodes (background (viewModel.viewport).moveBy (-viewModel.columnOffset, -viewModel.rowOffset))
          .addGameLayerNodes (bases (model, viewModel))
          .addUiLayerNodes (logbar (viewModel.viewport))
          .addUiLayerNodes (Text (viewModel.logMessage, 1, logbarY (viewModel.viewport) + 1, 1, fontKey))
      model.zone match {
        case Some (position) =>
          worldScene
            .addUiLayerNodes (controls (position, model.material, model.allocateView, model.world, viewModel.viewport, context.mouse))
        case None => worldScene
      }
    }
  }
}
