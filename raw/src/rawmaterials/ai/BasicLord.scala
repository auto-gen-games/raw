package rawmaterials.ai

import scala.annotation.tailrec
import scala.util.Random.shuffle
import indigo.{FrameContext, GameTime}
import rawmaterials.Utilities._
import rawmaterials.ai.BasicLord.{AllocationReasoning, ReasoningState, allocationPeriod, enactTransfers, planComputation}
import rawmaterials.game.{InterleavedComputation, MonitoredStep, ReferenceData}
import rawmaterials.world._


/*
 * Consumer as target plus amount needed
First task is to get list of producer feeds as consumers list, ordered so highest deposit listed first
One execution unit per producer
Determine targets for producer productions as nearest from consumer list
If consumer lists empty then randomly choose and add new consumer to list
Amount needed for defence and siege is fixed quantity
 */

/**
 * @param currentReasoning None means no reasoning planned for this round, Some (x) means a computation is planned for this round but could be completed
 */
case class BasicLord (lord: Lord, currentReasoning: Option[AllocationReasoning], timeToReallocation: Int) extends LordAI {
  def planInRound (world: World, gameTime: GameTime): LordAI =
    currentReasoning match {
      case None =>
        if (timeToReallocation == 0)
          copy (currentReasoning = Some (planComputation (lord, world)))
        else
          this
      case Some (reasoning) =>
        copy (currentReasoning = Some (reasoning.performMore (gameTime)))
    }

  def readyToProceed (world: World): Boolean =
    timeToReallocation > 0 || currentReasoning.exists (_.isCompleted)

  def completeRound (world: World): (World, LordAI) =
    (currentReasoning.map { plan => enactTransfers (lord, plan.currentResult, world) }.getOrElse (world),
      copy (currentReasoning = None, timeToReallocation = within (timeToReallocation - 1, allocationPeriod)))
}

object BasicLord {
  val allocationPeriod = 10

  type AllocationReasoning = InterleavedComputation[World, ReasoningState]

  def apply (identity: Lord): BasicLord =
    BasicLord (identity, None, 1)  // Need to give 1 round before reallocating to create the first producer

  case class Consumer (position: Position, sink: Sink, required: Amount)

  case class TransferRequest (material: Material, route: List[Position], sink: Sink, amount: Amount)

  case class ReasoningState (activeConsumers: Map[Material, List[Consumer]],
                             additionalConsumers: Map[Material, List[Consumer]],
                             transferRequests: List[TransferRequest]) {
    def addConsumer (material: Material, consumer: Consumer): ReasoningState =
      copy (activeConsumers = activeConsumers + (material -> (consumer :: activeConsumers (material))))

    def removeConsumer (material: Material, consumer: Consumer): ReasoningState =
      copy (activeConsumers = activeConsumers + (material -> activeConsumers (material).filter (_ != consumer)))

    def reduceRequired (material: Material, consumer: Consumer, reduction: Amount): ReasoningState = {
      val reduced = consumer.required - reduction
      if (reduced > 0L)
        removeConsumer (material, consumer).addConsumer (material, consumer.copy (required = reduced))
      else
        removeConsumer (material, consumer)
    }

    def activateConsumer (material: Material): Option[ReasoningState] =
      additionalConsumers (material).headOption.map { consumer =>
        copy (
          additionalConsumers = additionalConsumers + (material -> additionalConsumers (material).tail),
          activeConsumers     = activeConsumers     + (material -> (consumer :: activeConsumers (material)))
        )}

    def addTransfer (material: Material, route: List[Position], sink: Sink, amount: Amount): ReasoningState =
      copy (transferRequests = TransferRequest (material, route, sink, amount) :: transferRequests)
  }

  def initialReasoningState (lord: Lord, world: World): ReasoningState = {
    val producers = world.ownedProducers (lord)
    val activeConsumers: Map[Material, List[Consumer]] =
      world.terrain.materials.map { material =>
        (material, world.terrain.producedUsing (material).flatMap { produced =>
          producers.get (produced).map {
            pls => pls.map (pl => Consumer (pl._1, ProducerFeed (produced), pl._2))
          }
        }.getOrElse (Nil))
      }.toMap
    val otherTasks = shuffle (world.constructionPossibleBy (lord) ++ world.siegeTasksPossibleBy (lord))
    val additionalConsumers: Map[Material, List[Consumer]] =
      world.terrain.materials.map { material =>
        val consumers = (material, otherTasks.filter (_.material == material).map { task =>
          task.sink match {
            case ProducerBuilder (produced) =>
              Consumer (task.target, task.sink,
                world.producerUpgradeCostAt (produced, task.target)._2 - world.reserve (task.target, task.sink, material))
            case _ =>
              Consumer (task.target, task.sink, Long.MaxValue)
          }
        })
        if (consumers._2.isEmpty) System.err.println (s"No additional consumers for material $material")
        consumers
      }.toMap
    ReasoningState (activeConsumers, additionalConsumers, Nil)
  }

  case class ProducerAllocation (position: Position, material: Material, amount: Amount)
    extends MonitoredStep[World, ReasoningState] {

    val size = 1

    def perform (worldAtPlanStart: World, state: ReasoningState): ReasoningState =
      if (amount <= 0L) state
      else if (state.activeConsumers (material).nonEmpty) {
        val consumer =
          state.activeConsumers (material).minBy {
            consumer => distance (position, consumer.position,
              worldAtPlanStart.terrain.rows, worldAtPlanStart.terrain.columns)
          }
        val transferred = amount.min (consumer.required)
        worldAtPlanStart.route (position, consumer.position) match {
          case Some (route) =>
            copy (amount = amount - transferred)
              .perform (worldAtPlanStart,
                state
                  .reduceRequired (material, consumer, transferred)
                  .addTransfer (material, route, consumer.sink, transferred))
          case None =>
            perform (worldAtPlanStart, state.removeConsumer (material, consumer)).addConsumer (material, consumer)
        }
      } else state.activateConsumer (material) match {
        case Some (stateWithActivation) => perform (worldAtPlanStart, stateWithActivation)
        case None =>
          System.err.println ("No consumer found to pass materials to - should never happen")
          state
      }
  }

  def planComputation (lord: Lord, world: World): AllocationReasoning =
    InterleavedComputation[World, ReasoningState] (
      initialReasoningState (lord, world), world,
      world.ownedProducers (lord).toList.flatMap { mps =>
        mps._2.map { pa => ProducerAllocation (pa._1, mps._1, pa._2) }
      }
    )

  def enactTransfers (lord: Lord, state: ReasoningState, world: World): World =
    state.transferRequests.foldLeft (world.removeAllocationsBy (lord)) {
      case (tworld, transfer) =>
        tworld.addTransferPath (transfer.material, transfer.route, transfer.sink, transfer.amount)
    }
}
