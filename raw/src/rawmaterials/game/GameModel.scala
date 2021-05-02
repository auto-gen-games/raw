package rawmaterials.game

import indigo.{FrameContext, GameTime, Seconds}
import rawmaterials.ai.LordAI
import rawmaterials.world._

case class GameModel (world: World, controllers: List[LordAI], zone: Option[Position], material: Material,
  tickPeriod: Option[Seconds], lastTick: Seconds)

object GameModel {
  def apply (world: World, controllers: List[LordAI]): GameModel =
    //GameModel (world, controllers, None, 0, None, Seconds.zero)
    GameModel (world, controllers, Some ((0, 0)), 0, Some (Seconds (0.1)), Seconds.zero)

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
    (model.copy (world = updatedWorld, controllers = updatedControllers.reverse, lastTick = time),
      updatedWorld.occurrences.map (OccurrenceEvent (_, updatedWorld)))
  }
}
