package rawmaterials

import utest._
import TestWorld._
import Utilities._
import rawmaterials.world._

object WorldTests extends TestSuite {
  // Utility methods for tests

  def findZoneWithDeposit (material: Material, exclude: List[Position]): Position =
    terrain.positions.find {
      position => terrain.deposit (position, material) > 0 && !exclude.contains (position)
    }.get

  def ownPath (lord: Lord, start: Position, path: List[(Int, Int)], world: World): World =
    path.foldLeft ((start, world)) {
      case ((position, pworld), (dx, dy)) =>
        val next = moved (position, dx, dy, rows, columns)
        (next, pworld.changeOwner (next, lord))
    }._2.changeOwner (start, lord)

  val tests: Tests =
    utest.Tests {
      "terrain" - {
        "producedUsing" - {
          terrain.producedUsing ( 0) ==> Some (1)
          terrain.producedUsing ( 1) ==> Some (2)
          terrain.producedUsing (15) ==> None
        }

        "requiredInput" - {
          terrain.requiredInput (0) ==> None
          terrain.requiredInput (1) ==> Some (0)
          terrain.requiredInput (15) ==> Some (14)
        }

        "producerUpgradeCost" - {
          terrain.producerUpgradeCost (material = 0, level = 0)._2 ==> 1L
        }

        "all sectors non-zero deposits" - {
          assert (terrain.positions.forall (position => terrain.depositsAt (position).map (_._2).sum > 0))
        }
      }

      "world functions" - {
        "changeReserve" - {
          val world0 = world.changeReserve (player.home, Defence, 1, 10L)

          world0.reserve (player.home, Defence, 1) ==> 10L
        }

        "removeReserves" - {
          val world0 = world.changeReserve (player.home, Defence, 1, 10L)
          val world1 = world0.removeReserves (player.home, List (Defence))

          world1.reserve (player.home, Defence, 1) ==> 0L
        }

        "setReserve" - {
          val world1 = world.setReserve (player.home, ProducerFeed (1), 0, 20L)
          world1.reserve (player.home, ProducerFeed (1), 0) ==> 20L
          world1.reserves.get ((player.home, ProducerFeed (1), 0)) ==> Some (20L)
        }

        "militaryStrength" - {
          val sieged = moved (player.home, 0, -1, terrain.rows, terrain.columns)
          val world0 = world.changeOwner (sieged, enemy).changeReserve (sieged, Defence, 2, 2L)

          world0.militaryStrength (sieged, Defence) ==> 8
        }

        "produceFromInput" - {
          val world1 = world.produceFromInput (player.home, produced = 1, input = Some (0), consumed = 10L)
          world1.reserve (player.home, Hub, 1) ==> 10L
        }

        "produceMaterialAt" - {
          val world1 = world.setReserve (player.home, ProducerFeed (1), 0, 20L)
                            .increaseProduction (player.home, 1, 30)
          val world2 = world1.produceMaterialAt (player.home, 1)
          world2.reserve (player.home, Hub, 1) ==> 20L
        }

        "constructionTasksPossible" - {
          val other       = findZoneWithDeposit (2, List (enemy.home, player.home))
          val world0      = world
            .increaseProduction (enemy.home, 0, terrain.levelsPerPriceMaterial + 1)
            .increaseProduction (other, 2, 1)
          val tasksHome0  = world0.constructionTasksPossible (enemy.home, 0)
          val tasksHome1  = world0.constructionTasksPossible (enemy.home, 1)
          val tasksOther1 = world0.constructionTasksPossible (other, 1)
          val homeTask    = Task (enemy.home, 1, enemy.home, ProducerBuilder (0))
          val otherTask   = Task (other, 1, other, ProducerBuilder (2))
          val defenceTask = Task (enemy.home, 0, enemy.home, Defence)

          "home defence"     - { assert (tasksHome0.contains (defenceTask)) }
          "home production"  - { assert (tasksHome1.contains (homeTask)) }
          "other production" - { assert (tasksOther1.contains (otherTask)) }
        }

        "constructionsPossibleBy" - {
          val other     = findZoneWithDeposit (2, List (enemy.home, player.home))
          val world0    = world
            .increaseProduction (enemy.home, 0, terrain.levelsPerPriceMaterial + 1)
            .changeOwner (other, enemy)
            .increaseProduction (other, 2, 1)
          val homeTask  = Task (enemy.home, 1, enemy.home, ProducerBuilder (0))
          val otherTask = Task (other, 1, other, ProducerBuilder (2))
          val tasks     = world0.constructionPossibleBy (enemy)

          "home production" - { assert (tasks.contains (homeTask)) }
          "other production" - { assert (tasks.contains (otherTask)) }
        }

        "siegeTasksPossibleBy" - {
          val other   = moved (enemy.home, -1, 0, rows, columns)
          val distant = moved (other, -2, 0, rows, columns)
          val world0  = world.changeOwner (other, enemy)
          val tasks   = world0.siegeTasksPossibleBy (enemy)

          "number of sieges"      - { tasks.size ==> 6 * terrain.materials.size }
          "cannot attack self"    - { assert (!tasks.exists (_.target == enemy.home)) }
          "cannot attack distant" - { assert (!tasks.exists (_.target == distant)) }
        }

        "accessibleFrom" - {
          val owned      = moved (player.home, 2, 2, rows, columns)
          val unowned1   = moved (player.home, -1, 0, rows, columns)
          val unowned2   = moved (player.home, 0, -1, rows, columns)
          val world0     =
            ownPath (player, player.home, List ((0, 1), (1, 0), (0, 1), (1, 0), (0, 1)), world)
              .changeOwner (unowned1, enemy)
          val accessible = world0.accessibleFrom (player.home)

          "number accessible"  - { accessible.size ==> 6 }
          "owned accessible"   - { assert ( accessible.contains (owned)) }
          "enemy inaccessible" - { assert (!accessible.contains (unowned1)) }
          "empty inaccessible" - { assert (!accessible.contains (unowned2)) }
        }

        "ownedProducers" - {
          val other     = moved (player.home, 2, 2, rows, columns)
          val world0    =
            ownPath (player, player.home, List ((0, 1), (1, 0), (0, 1), (1, 0), (0, 1)), world)
              .increaseProduction (player.home, 3, 2)
              .increaseProduction (other,       0, 5)
          val producers = world0.ownedProducers (player)

          "total producers"  - { producers.values.map (_.map (_._2).sum).sum ==> 7 }
          "home production"  - { assert ( producers.get (3).exists (_.contains ((player.home, 2)))) }
          "other production" - { assert ( producers.get (0).exists (_.contains ((other, 5)))) }
          "non-production"   - { assert (!producers.get (0).exists (_.exists (_._1 == player.home))) }
        }

        "route" - {
          val owned      = moved (player.home, 2, 2, rows, columns)
          val unowned1   = moved (player.home, -2, 0, rows, columns)
          val unowned2   = moved (player.home, 0, -2, rows, columns)
          val world0     =
            ownPath (player, player.home, List ((0, 1), (1, 0), (0, 1), (1, 0), (0, 1)), world)
              .changeOwner (unowned1, enemy)
          val routeSelf  = world0.route (owned, owned)
          val routeOwned = world0.route (player.home, owned)
          val routeBack  = world0.route (owned, player.home)
          val routeEnemy = world0.route (player.home, unowned1)
          val routeEmpty = world0.route (player.home, unowned2)

          "self route" - { routeSelf  ==> Some (List (owned)) }
          "distance"   - { routeOwned.getOrElse (Nil).size ==> 5 }
          "reverse"    - { routeOwned ==> routeBack.map (_.reverse) }
          "blocked 1"  - { routeEnemy ==> None }
          "blocked 2"  - { routeEmpty ==> None }
        }

        "addTransferPath" - {
          val route  = List ((0, 1), (1, 0), (0, 1), (1, 0), (0, 1))
          val steps  = route.foldLeft (List (player.home)) {
            case (stepsSoFar, (dx, dy)) => stepsSoFar :+ moved (stepsSoFar.last, dx, dy, rows, columns)
          }
          val world0 = ownPath (player, player.home, route, world)
            .addTransferPath (0, route, Defence, 10)
          val tasks  = world0.tasksAllocatedBy (player)

          "transfers"  - {
            steps.sliding (2, 1).forall { positions =>
              tasks.contains (Task (positions (0), 0, positions (1), Hub)) }
          }
          "amount"     - { world0.allocations.get (Task (steps (0), 0, steps (1), Hub)).contains (10) }
          "final step" - { world0.allocations.get (Task (steps.last, 0, steps.last, Defence)).contains (10) }
        }

        "removeAllocationsBy" - {
          val world0 = world.removeAllocationsBy (enemy)

          assert (world0.tasksAllocatedBy (enemy).isEmpty)
        }

        "produce"   - { world.produce;    assert (true) }
        "enact"     - { world.enactTasks; assert (true) }
        "construct" - { world.construct;  assert (true) }

        "conquer"   - {
          val left   = moved (player.home, 0, -1, rows, columns)
          val world1 = world
            .update
            .removeAllocationsBy (player)
            .addToAllocation (Task (player.home, 0, left, Siege (player)), 1)
          val world2 = world1.conquer

          "no siege" - { assert (world2.reserve (left, Siege (player), 0) == 0) }
        }

        "update"    - { world.update;     assert (true) }
      }

      "world scenarios" - {
        "starting producer" - {
          val build  = Task (player.home, 0, player.home, ProducerBuilder (0))
          val world1 = world.update
          val world2 = world1.removeAllocation (build).update
          val world3 = world2.update

          "zero producers start" - { world.producer  (player.home, 0)      ==> 0 }
          "one producers next"   - { world1.producer (player.home, 0)      ==> 1 }
          "first production"     - { world2.reserve  (player.home, Hub, 0) ==> 1L }
          "second production"    - { world3.reserve  (player.home, Hub, 0) ==> 2L }
        }

        "feed producer" - {
          val build  = Task (player.home, 0, player.home, ProducerBuilder (0))
          val feed   = Task (player.home, 0, player.home, ProducerFeed (1))
          val world0 = world
            .increaseProduction (player.home, 0, 1)
            .increaseProduction (player.home, 1, 2)
            .removeAllocation (build)
            .setAllocation (feed, 1L).update
          val world1 = world0.update
          val world2 = world1.update

          "zero produced start" - { world1.reserve (player.home, Hub, 1) ==> 1L }
          "one produced next"   - { world2.reserve (player.home, Hub, 1) ==> 2L }
        }

        "acquire sector" - {
          val sieged   = moved (player.home, 0, -1, rows, columns)
          val unsieged = moved (player.home, 0, 1, rows, columns)
          val siege    = Task (player.home, 0, sieged, Siege (player))
          val world1   = world
            .setReserve (player.home, Hub, 0, 5L)
            .setAllocation (siege, 1L)
            .update

          "sector acquired"     - { world1.owner.get (sieged) ==> Some (player) }
          "unsieged unacquired" - { world1.owner.get (unsieged) ==> None }
          "no materials left"   - { world1.reserve (sieged, Defence, 0) ==> 0L }
        }

        "conquer enemy sector" - {
          val sieged = moved (player.home, 0, -1, rows, columns)
          val siege  = Task (player.home, 0, sieged, Siege (player))
          val world0 = world
            .changeOwner (sieged, enemy)
            .setReserve (sieged, Defence, 0, 2L)
            .setReserve (player.home, Hub, 0, 10L)
            .setAllocation (siege, 1L)
          val world1 = world0.update
          val world2 = world1.update
          val world3 = world2.update

          "state 0 owner"   - { world0.owner.get (sieged) ==> Some (enemy) }
          "state 0 defence" - { world0.militaryStrength (sieged, Defence) ==> 2 }
          "state 0 siege"   - { world0.militaryStrength (sieged, Siege (player)) ==> 0 }
          "state 1 owner"   - { world1.owner.get (sieged) ==> Some (enemy) }
          "state 1 siege"   - { world1.militaryStrength (sieged, Siege (player)) ==> 1 }
          "state 2 owner"   - { world2.owner.get (sieged) ==> Some (enemy) }
          "state 2 siege"   - { world2.militaryStrength (sieged, Siege (player)) ==> 2 }
          "state 3 owner"   - { world3.owner.get (sieged) ==> Some (player) }
          "state 3 siege"   - { world3.militaryStrength (sieged, Siege (player)) ==> 0 }
          "state 3 defence" - { world3.militaryStrength (sieged, Defence) ==> 1 }
        }
      }
    }
}
