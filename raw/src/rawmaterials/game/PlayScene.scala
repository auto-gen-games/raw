package rawmaterials.game

import indigo._
import indigo.scenes.{Lens, Scene, SceneName}
import indigo.shared.events.MouseEvent.{MouseDown, MouseUp, Move}
import rawmaterials.Settings._
import rawmaterials.Utilities.moved
import rawmaterials.game.GameAssets.{base, cell, defence, fontKey, log, noProduction, producers, siege}
import rawmaterials.world.Material

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

  def logbar (viewport: GameViewport): Group =
    Group ((for (x <- 0 to Math.ceil (viewport.width / 20.0).toInt) yield
      log.moveTo (x * 20, logbarY (viewport))).toList)

  def present (context: FrameContext[ReferenceData], model: GameModel, viewModel: ViewModel): Outcome[SceneUpdateFragment] = {
    Outcome (
      SceneUpdateFragment.empty
        .addGameLayerNodes (background (viewModel.viewport).moveBy (-viewModel.columnOffset, -viewModel.rowOffset))
        .addGameLayerNodes (bases (model, viewModel))
        .addUiLayerNodes (logbar (viewModel.viewport))
        .addUiLayerNodes (Text (viewModel.logMessage, 1, logbarY (viewModel.viewport) + 1, 1, fontKey))
    )
  }
}
