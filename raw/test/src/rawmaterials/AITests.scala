package rawmaterials

import indigo.shared.time.GameTime.FPS
import indigo.{GameTime, Seconds}
import rawmaterials.TestWorld._
import rawmaterials.Utilities.moved
import rawmaterials.ai.BasicLord
import rawmaterials.ai.BasicLord._
import rawmaterials.game.GameModel
import rawmaterials.game.GameModel.{nextRound, readyToProceed, updateWithinRound}
import rawmaterials.world._
import utest._

object AITests extends TestSuite {
  val tests: Tests = Tests {
    def seconds (time: Int): GameTime         = GameTime (Seconds (time), Seconds (1), FPS (60))
    def ailord  (model: GameModel): BasicLord = model.controllers.head.asInstanceOf[BasicLord]

    "ReasoningState" - {
      val consumer = Consumer ((0, 2), Hub, 10L)
      val reduced  = Consumer ((0, 2), Hub, 4L)
      val emptyConsumers =
        world.terrain.materials.map (material => (material, Nil)).toMap[Material, List[Consumer]]
      val singleConsumer =
        world.terrain.materials.map (material => (material, Nil)).toMap[Material, List[Consumer]] + (1 -> List (consumer))
      val state0 = ReasoningState (emptyConsumers, emptyConsumers, world)
      val state1 = state0.addConsumer (1, consumer)
      val state2 = state1.removeConsumer (1, consumer)
      val state3 = state1.reduceRequired (1, consumer, 6L)
      val state4 = ReasoningState (emptyConsumers, singleConsumer, world)
      val state5 = state4.activateConsumer (1)

      "addConsumer"      - { assert ( state1.activeConsumers (1).contains (consumer)) }
      "removeConsumer"   - { assert (!state2.activeConsumers (1).contains (consumer)) }
      "reduceRequired"   - { assert ( state3.activeConsumers (1).contains (reduced))  }
      "activateConsumer" - { assert ( state5.exists (_.activeConsumers (1).contains (consumer))) }
    }

    "initialReasoningState" - {
      val world1 = world.increaseProduction (enemy.home, 1, 2)
      val state  = initialReasoningState (enemy, world1)
      val left   = moved (enemy.home, -1, 0, rows, columns)

      "feed to 1" -
        { assert (state.activeConsumers (0).exists (c => c.position == enemy.home && c.sink == ProducerFeed (1))) }
      "only home" -
        { assert (!state.activeConsumers (0).exists (c => c.position != enemy.home)) }
      /*"defence" -
        { assert (state.additionalConsumers (0).exists (c => c.position == enemy.home && c.sink == Defence)) }
      "builder 1" -
        { assert (state.additionalConsumers (0).exists (c => c.position == enemy.home && c.sink == ProducerBuilder (1))) }*/
      "siege" -
        { assert (state.additionalConsumers (0).exists (c => c.position == left && c.sink == Siege (enemy))) }
      "all active" -
        { assert (terrain.materials.toSet.subsetOf (state.activeConsumers.keys.toSet)) }
      "all additional" -
        { assert (terrain.materials.toSet.subsetOf (state.additionalConsumers.keys.toSet)) }
    }

    "planCompute" - {
      val world1 = world
        .increaseProduction (enemy.home, 0, 2)
        .increaseProduction (enemy.home, 1, 1)
      val world2 = world.update
      val plan0 = planComputation (enemy, world)
      val plan1 = planComputation (enemy, world1)
      val plan2 = planComputation (enemy, world2)

      "Original plan" - { assert (plan0.sizeRemaining == 0) }
      "Other plan"    - { assert (plan1.sizeRemaining > 0) }
      "Next plan"     - { assert (plan2.sizeRemaining > 0) }
    }

    "execute step" - {
      val world1 = world
        .increaseProduction (enemy.home, 0, 2)
        .increaseProduction (enemy.home, 1, 1)
      val state = initialReasoningState (enemy, world1)
      val step0 = ProducerAllocation (enemy.home, 0, 2)
      val step1 = ProducerAllocation (enemy.home, 1, 1)

      "step 0" - { step0.execute (state); assert (true) }
      "step 1" - { step1.execute (state); assert (true) }
    }

    "complete execution" - {
      val world1 = world
        .increaseProduction (enemy.home, 0, 2)
        .increaseProduction (enemy.home, 1, 1)
      val compute = planComputation (enemy, world1)
      val world2  = compute.performAll.currentResult.world
      val feed    = Task (enemy.home, 0, enemy.home, ProducerFeed (1))

      "producer 0" -
        { assert (compute.steps.exists (s => s.asInstanceOf[ProducerAllocation].position == enemy.home && s.asInstanceOf[ProducerAllocation].material == 0)) }
      "producer 1" -
        { assert (compute.steps.exists (s => s.asInstanceOf[ProducerAllocation].position == enemy.home && s.asInstanceOf[ProducerAllocation].material == 1)) }
      "feed" -
        { assert (world2.allocations.toList.contains ((feed, 1L))) }
      "use excess" -
        { assert (world2.allocations.toList.exists (ta => ta._1.source == enemy.home && ta._1.material == 0 && ta._2 == 1L)) }
      "use unfed" -
        { assert (world2.allocations.toList.exists (ta => ta._1.source == enemy.home && ta._1.material == 1 && ta._2 == 1L)) }
    }

    "model update" - {
      val world1      = world.update
      val model0      = GameModel (world1, lordAIs, Some (Seconds (1)), Seconds.zero)
      val model1      = updateWithinRound (model0, seconds (1))
      val model2      = updateWithinRound (model1, seconds (2))
      val size1       = model1.controllers.head.asInstanceOf[BasicLord].currentReasoning.map (_.sizeRemaining).getOrElse (0)
      val size2       = model2.controllers.head.asInstanceOf[BasicLord].currentReasoning.map (_.sizeRemaining).getOrElse (0)
      val (model3, _) = nextRound (model2, seconds (3).running)
      val model4      = (1 until allocationPeriod).foldLeft (model3) {
        case (model, round) => nextRound (model, seconds (3 + round).running)._1
      }
      val model5      = updateWithinRound (model4, seconds (allocationPeriod + 3))
      val model6      = updateWithinRound (model5, seconds (allocationPeriod + 4))

      "initially unready" - { assert (!readyToProceed (model0)) }
      "unplanned"         - { assert (ailord (model0).currentReasoning.isEmpty) }
      "planned"           - { assert (ailord (model1).currentReasoning.nonEmpty) }
      "unallocated"       - { assert (model1.world.tasksAllocatedBy (enemy).isEmpty) }
      "unexecuted"        - { assert (ailord (model1).currentReasoning.exists (!_.isCompleted)) }
      "executed"          - { assert (size2 < size1) }
      "sink chosen"       - { assert (model2.world.allocations.keys.exists (task => task.source == enemy.home && (task.sink == Defence || task.sink.isInstanceOf[Siege] || task.sink.isInstanceOf[ProducerBuilder]))) }
      "ready"             - { assert (readyToProceed (model2)) }
      "unplanned again"   - { assert (ailord (model3).currentReasoning.isEmpty) }
      "remaining"         - { assert (ailord (model3).timeToReallocation == allocationPeriod - 1) }
      "siege included"    - { assert (ailord (model5).currentReasoning.exists (_.currentResult.additionalConsumers (0).exists (_.sink.isInstanceOf[Siege]))) }
    }

    "log sinks" - {
      val world1 = world.update
      val model0 = GameModel (world1, lordAIs, Some (Seconds (1)), Seconds.zero)
      val result = (1 until allocationPeriod * 30).foldLeft (model0) {
        case (model, round) =>
          var next = nextRound (model, seconds (round + 1).running)._1
          while (!readyToProceed (next))
            next = updateWithinRound (next, seconds (round + 1))
          next
      }
      println (result.world.tasksAllocatedBy (enemy).map (task => s"$task: ${result.world.allocations.getOrElse (task, 0)}"))
      assert (true)
    }
  }
}
