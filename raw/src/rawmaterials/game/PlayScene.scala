package rawmaterials.game

import indigo._
import indigo.scenes.{Lens, Scene, SceneName}
import indigo.shared.events.MouseEvent.{Click, MouseDown, MouseUp, Move}
import rawmaterials.Settings._
import rawmaterials.Utilities.{moved, within}
import rawmaterials.game.Controls.{adjustOver, controls, optionOver}
import rawmaterials.game.GameAssets.{base, cell, decreaseButton, defence, fontKey, log, noProduction, producers, siege}
import rawmaterials.game.GameModel.{openControlView, scrollBy, scrollFrom, setAllocateView, setMaterialView, setMilitaryView, stopScrolling, updateView}
import rawmaterials.world.{Material, Position, Task}
import indigo.shared.materials.Material.Bitmap

object PlayScene extends Scene[ReferenceData, GameModel, GameViewport] {
  type SceneModel                                     = GameModel
  type SceneViewModel                                 = GameViewport
  val name: SceneName                                 = SceneName ("PlayScene")
  val modelLens: Lens[GameModel, GameModel]           = Lens.keepLatest
  val viewModelLens: Lens[GameViewport, GameViewport] = Lens.keepLatest
  val eventFilters: EventFilters                      = EventFilters.AllowAll
  val subSystems: Set[SubSystem]                      = Set.empty

  def updateModel (context: FrameContext[ReferenceData], model: GameModel): GlobalEvent => Outcome[GameModel] = {
    case FrameTick =>
      val (newModel, occurrences) = GameModel.update (model, context.gameTime)
      Outcome (newModel).addGlobalEvents (occurrences)
    case event@MouseDown (_, _) => Outcome (scrollFrom (model, event))
    case event@Move (_, _) => Outcome (scrollBy (model, event))
    case MouseUp (_, _) => Outcome (stopScrolling (model))
    case Click (x, y) =>
      model.controlView match {
        case Some (view) =>
          optionOver (x, y) match {
            case Some (0) =>
              Outcome (setMaterialView (model, within (view.material - 1, model.world.terrain.materials.size)))
            case Some (1) =>
              Outcome (setMaterialView (model, within (view.material + 1, model.world.terrain.materials.size)))
            case Some (2) => Outcome (setMilitaryView (model))
            case Some (3) => Outcome (setAllocateView (model, DepositsView))
            case Some (4) => Outcome (setAllocateView (model, ProducersView))
            case Some (5) => Outcome (setAllocateView (model, BalanceView))
            case Some (6) => Outcome (setAllocateView (model, BuildView))
            case Some (7) => Outcome (setAllocateView (model, TransportView))
            case Some (8) => Outcome (setAllocateView (model, SiegeView))
            case _ =>
              Outcome (adjustOver (x, y, view.infoLines).map { case (line, isIncrease) =>
                line.update match {
                  case Some ((target, sink)) =>
                    val change = if (isIncrease) 1 else -1
                    if (line.value + change >= 0)
                      updateView (model.copy (world = model.world.setAllocation (
                        Task (view.zone, view.material, target, sink), line.value + change)))
                    else model
                  case None => model
                }
              }.getOrElse (selectZone (x, y, model)))
          }
        case None => Outcome (selectZone (x, y, model))
      }
    case _ => Outcome (model)
  }

  def selectZone (x: Int, y: Int, model: GameModel): GameModel = {
    val row    = within (((y - model.scrollModel.columnOffset) / 64) + model.scrollModel.topLeft._1 + 1, model.world.terrain.rows)
    val column = within (((x - model.scrollModel.rowOffset) / 64) + model.scrollModel.topLeft._2 + 1, model.world.terrain.columns)
    openControlView (model, (row, column))
  }

  def updateViewModel (context: FrameContext[ReferenceData], model: GameModel, viewModel: GameViewport): GlobalEvent => Outcome[GameViewport] =
    _ => Outcome (viewModel)

  def rowsVisible    (viewport: GameViewport): Int = (viewport.height - 20) / 64
  def columnsVisible (viewport: GameViewport): Int = viewport.width / 64

  def background (selected: Option[Position], scrollModel: ScrollModel, rows: Int, columns: Int, viewport: GameViewport): Group =
    Group ((for (row <- 0 to rowsVisible (viewport) + 1; column <- 0 to columnsVisible (viewport) + 1) yield
      if (selected.contains ((within (scrollModel.topLeft._1 + row, rows), within (scrollModel.topLeft._2 + column, columns))))
        cell.moveTo (column * 64, row * 64).modifyMaterial (_.toImageEffects.withOverlay (Fill.Color (RGBA (1.0, 1.0, 1.0, 0.5))))
      else
        cell.moveTo (column * 64, row * 64)
      ).toList).moveBy (-scrollModel.columnOffset, -scrollModel.rowOffset)

  def materialAlpha (material: Material, materials: Int): Double =
    ((maxDefenceAlpha - minDefenceAlpha) / (materials - 1)) * material + minDefenceAlpha

  def bases (model: GameModel, scrollModel: ScrollModel, viewport: GameViewport): Group =
    Group ((
      for (row <- 0 to rowsVisible (viewport); column <- 0 to columnsVisible (viewport)) yield {
        val position = moved ((row, column), scrollModel.topLeft._1, scrollModel.topLeft._2, model.world.terrain.rows, model.world.terrain.columns)
        val x = column * 64 - scrollModel.columnOffset
        val y = row * 64 - scrollModel.rowOffset
        model.world.owner.get (position).map { lord =>
          val produced = model.world.producersAt (position)
          val predominant = if (produced.map (_._2).sum == 0) noProduction else producers (produced.maxBy (_._2)._1)
          val maxSiege = model.world.siegesAt (position).filter (_._2 > 0L).sortBy (_._2).reverse.headOption.map (_._1)
          val defences = model.world.defenceMaterialsAt (position).filter (_._2 > 0L).map (_._1).sorted.reverse.headOption
          Group (List (
            Some (base.moveTo (x, y)),
            defences.map (material => defence.moveTo (x, y).modifyMaterial (_.toImageEffects.withAlpha (materialAlpha (material, model.world.terrain.materials.size)))),
            maxSiege.map (attacker => siege.moveTo (x, y).modifyMaterial (_.toImageEffects.withTint (attacker.flag))),
            Some (predominant.moveTo (x + 16, y + 16).modifyMaterial (_.toImageEffects.withTint (lord.flag)))
          ).flatten)
        }
      }).toList.flatten)

  def logbarY (viewport: indigo.GameViewport): Int =
    viewport.height - 20

  def logbar (viewport: GameViewport): Shape.Box =
    Shape.Box (Rectangle (Point (0, logbarY (viewport)), Size (viewport.width, 20)), Fill.Color (RGBA.White), Stroke (1, RGBA.White))


  def present (context: FrameContext[ReferenceData], model: GameModel, viewport: GameViewport): Outcome[SceneUpdateFragment] = {
    Outcome {
      val worldScene =
        SceneUpdateFragment (
          background (model.controlView.map (_.zone), model.scrollModel,
            model.world.terrain.rows, model.world.terrain.columns, viewport),
          bases (model, model.scrollModel, viewport),
          logbar (viewport))
          //.addUiLayerNodes (Text (viewModel.logMessage, 1, logbarY (viewModel.viewport) + 1, 1, fontKey))
      model.controlView match {
        case Some (view) =>
          worldScene.addLayer (Layer (controls (view, model.player, model.world, viewport, context.mouse)))
        case None => worldScene
      }
    }
  }
}
