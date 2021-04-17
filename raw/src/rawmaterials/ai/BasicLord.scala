package rawmaterials.ai

import scala.annotation.tailrec
import scala.util.Random.shuffle
import indigo.{FrameContext, GameTime}
import rawmaterials.Utilities._
import rawmaterials.ai.BasicLord.{AllocationReasoning, allocationPeriod, planComputation}
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
  def readyToProceed (world: World): Boolean =
    timeToReallocation > 0 || currentReasoning.exists (_.isCompleted)

  def newRoundStarted (world: World): LordAI =
    copy (currentReasoning = None, timeToReallocation = within (timeToReallocation - 1, allocationPeriod))

  def update (world: World, gameTime: GameTime): (World, LordAI) =
      currentReasoning match {
        case None =>
          if (timeToReallocation == 0) {
            val unallocated = world.removeAllocationsBy (lord)
            (unallocated, copy (currentReasoning = Some (planComputation (lord, unallocated))))
          }
          else
            (world, this)
        case Some (reasoning) =>
          val next = reasoning.performMore (gameTime)
          (next.currentResult.world, copy (currentReasoning = Some (next)))
      }
}

object BasicLord {
  val allocationPeriod = 10

  type AllocationReasoning = InterleavedComputation[Unit, ReasoningState]

  def apply (identity: Lord): BasicLord =
    BasicLord (identity, None, 0)

  case class Consumer (position: Position, sink: Sink, required: Amount)

  case class ReasoningState (activeConsumers: Map[Material, List[Consumer]],
                             additionalConsumers: Map[Material, List[Consumer]], world: World) {
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

    def withWorld (world: World): ReasoningState =
      copy (world = world)
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
    ReasoningState (activeConsumers, additionalConsumers, world)
  }

  case class ProducerAllocation (position: Position, material: Material, amount: Amount)
    extends MonitoredStep[Unit, ReasoningState] {

    val size = 1

    def execute (state: ReasoningState): ReasoningState =
      if (amount <= 0L) state
      else if (state.activeConsumers (material).nonEmpty) {
        val consumer =
          state.activeConsumers (material).minBy {
            consumer => distance (position, consumer.position, state.world.terrain.rows, state.world.terrain.columns)
          }
        val transferred = amount.min (consumer.required)
        state.world.route (position, consumer.position) match {
          case Some (route) =>
            copy (amount = amount - transferred)
              .execute (state
                .reduceRequired (material, consumer, transferred)
                .withWorld (state.world.addTransferPath (material, route, consumer.sink, transferred)))
          case None =>
            execute (state.removeConsumer (material, consumer)).addConsumer (material, consumer)
        }
      } else state.activateConsumer (material) match {
        case Some (stateWithActivation) => execute (stateWithActivation)
        case None =>
          System.err.println ("No consumer found to pass materials to - should never happen")
          state
      }


    def perform (ignored: Unit, state: ReasoningState): ReasoningState =
      execute (state)
  }

  def planComputation (lord: Lord, world: World): AllocationReasoning =
    InterleavedComputation[Unit, ReasoningState] (
      initialReasoningState (lord, world), (),
      world.ownedProducers (lord).toList.flatMap { mps =>
        mps._2.map { pa => ProducerAllocation (pa._1, mps._1, pa._2) }
      }
    )



  /** Allocate produced materials to feed producers, then allocate excess of each material to a single random sink. */
  def updateAllocations (lord: Lord, world: World): World = {
    val (worldAfterFeed, excessProducers) = supplyAllFeeds (lord, world)
    val chosenSink: Map[Material, (Position, Sink)] =
      nonFeedSinksPossible (lord, worldAfterFeed).toList
        .flatMap (materialSinks => choose (materialSinks._2).map (chosen => (materialSinks._1, chosen))).toMap
    worldAfterFeed.terrain.materials.foldLeft (worldAfterFeed) {
      case (mworld, material) =>
        implementAllFeeds (material, excessProducers (material), chosenSink (material), mworld)
    }
  }

  /** For each material, create supply paths to give all the producer feeds with enough input material from the
   * producers of that input material, returning the world with the new allocations and the excess remaining
   * produced of each material. */
  def supplyAllFeeds (lord: Lord, world: World): (World, Map[Material, List[(Position, Level)]]) =
    world.terrain.materials.foldLeft ((world, world.ownedProducers (lord))) {
      case ((mworld, producers), material) =>
        mworld.terrain.producedUsing (material) match {
          case Some (produced) =>
            val (newWorld, excessProducers) =
              supplyFeedMaterial (material, mworld.consumers (material, Some (lord)), produced, producers (material), mworld)
            (newWorld, producers + (material -> excessProducers))
          case None => (mworld, producers)
        }
    }

  /** For the given input material, supply all the given producer feeds producing the produced material
   * with the given amounts up to the amount of the input material produced at each of the given positions,
   * returning the world with the new allocations and the unused input material from each source position. */
  @tailrec
  def supplyFeedMaterial (material: Material, consumers: List[(Position, Level)], produced: Material, producers: List[(Position, Level)], world: World): (World, List[(Position, Level)]) =
    consumers match {
      case Nil => (world, producers)
      case consumer :: otherConsumers if consumer._2 == 0 =>  // If the next consumer needs no more input material
        supplyFeedMaterial (material, otherConsumers, produced, producers, world)
      case consumer :: otherConsumers =>
        val accessible = world.accessibleFrom (consumer._1)
        producers.find (producer => accessible.contains (producer._1) && producer._2 > 0)
          .flatMap (producer => world.route (producer._1, consumer._1)) match {
          case Some (route) =>
            val newWorld = world.addTransferPath (material, route, ProducerFeed (produced), 1L)
            val newProducers = producers.map {
              case (position, level) => (position, if (position == route.head) level - 1 else level)
            }
            val newConsumers = (consumer._1, consumer._2 - 1) :: otherConsumers
            supplyFeedMaterial (material, newConsumers, produced, newProducers, newWorld)
          case None => supplyFeedMaterial (material, otherConsumers, produced, producers, world)
        }
    }

  /** For each material, get the list of sinks excluding production feeds that the given lord could send them to. */
  def nonFeedSinksPossible (lord: Lord, world: World): Map[Material, List[(Position, Sink)]] =
    (world.constructionPossibleBy (lord) ++ world.siegeTasksPossibleBy (lord))
      .groupBy (_.material)
      .toList
      .map (mt => (mt._1, mt._2.map (task => (task.target, task.sink))))
      .toMap

  /** Add a feed path for each given source and amount of the given material to the target sink. */
  def implementAllFeeds (material: Material, sources: List[(Position, Level)], sink: (Position, Sink), world: World): World =
    sources.foldLeft (world) {
      case (sworld, (position, amount)) =>
        sworld.route (position, sink._1) match {
          case Some (route) => sworld.addTransferPath (material, route, sink._2, amount)
          case None => sworld
        }
    }
}
