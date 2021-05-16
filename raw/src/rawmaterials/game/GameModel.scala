package rawmaterials.game

import indigo.{GameTime, Seconds}
import rawmaterials.ai.LordAI
import rawmaterials.game.Controls.{InfoLine, updateInfoLines}
import rawmaterials.world._

case class ControlView (zone: Position, material: Material, allocateView: AllocateView,
                        militaryView: Boolean, infoLines: List[InfoLine])

case class GameModel (world: World, player: Lord, controllers: List[LordAI], controlView: Option[ControlView],
                      tickPeriod: Option[Seconds], lastTick: Seconds)

object GameModel {
  val emptyControlView: ControlView = ControlView ((0, 0), 0, DepositsView, false, Nil)

  def apply (world: World, player: Lord, controllers: List[LordAI]): GameModel =
    //GameModel (world, player, controllers, None, None, Seconds.zero)
    openControlView (GameModel (world, player, controllers, None, Some (Seconds (1)), Seconds.zero), player.home)

  def updateView (model: GameModel): GameModel =
    model.copy (controlView = model.controlView.map (view => updateInfoLines (view, model.player, model.world)))

  def openControlView (model: GameModel, position: Position): GameModel =
    model.copy (controlView = Some (updateInfoLines (emptyControlView.copy (zone = position), model.player, model.world)))

  def closeControlView (model: GameModel): GameModel =
    model.copy (controlView = None)

  def setAllocateView (model: GameModel, allocateView: AllocateView): GameModel =
    model.controlView match {
      case Some (current) =>
        updateView (model.copy (controlView = Some (current.copy (allocateView = allocateView, militaryView = false))))
      case None =>
        model
    }

  def setMilitaryView (model: GameModel): GameModel =
    model.controlView match {
      case Some (current) =>
        updateView (model.copy (controlView = Some (current.copy (militaryView = true))))
      case None =>
        model
    }

  def setMaterialView (model: GameModel, material: Material): GameModel =
    model.controlView match {
      case Some (current) =>
        updateView (model.copy (controlView = Some (current.copy (material = material, militaryView = false))))
      case None =>
        model
    }

  def update (model: GameModel, gameTime: GameTime): (GameModel, List[OccurrenceEvent]) =
    model.tickPeriod match {
      case Some (period) =>
        if (gameTime.running > model.lastTick + period && readyToProceed (model))
          nextRound (model, gameTime.running)
        else
          (updateWithinRound (model, gameTime), Nil)
      case None => (updateWithinRound (model, gameTime), Nil)
    }

  def updateWithinRound (model: GameModel, gameTime: GameTime): GameModel = {
    val updatedControllers =
      model.controllers.foldLeft[List[LordAI]] (Nil) {
        case (newControllers, controller) =>
          val newController = controller.planInRound (model.world, gameTime)
          newController :: newControllers
      }
    model.copy (controllers = updatedControllers.reverse)
  }

  def readyToProceed (model: GameModel): Boolean =
    model.controllers.forall (_.readyToProceed (model.world))

  def nextRound (model: GameModel, time: Seconds): (GameModel, List[OccurrenceEvent]) = {
    val (plannedWorld, updatedControllers) =
      model.controllers.foldLeft[(World, List[LordAI])] ((model.world, Nil)) {
        case ((world, newControllers), controller) =>
          val (cworld, newController) = controller.completeRound (world)
          (cworld, newController :: newControllers)
      }
    val updatedWorld = plannedWorld.update
    (updateView (model.copy (world = updatedWorld, controllers = updatedControllers.reverse, lastTick = time)),
      updatedWorld.occurrences.map (OccurrenceEvent (_, updatedWorld)))
  }
}
