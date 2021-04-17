package rawmaterials

import rawmaterials.Utilities.transfers
import rawmaterials.world.{Defence, Hub, Task}
import utest._

object UtilityTests {
  val tests: Tests =
    utest.Tests {

      "utility methods" - {
        val task1 = world.Task ((0, 0), 0, (0, 1), Hub)
        val task2 = Task ((0, 0), 0, (0, 0), Defence)
        val allocations = List ((task1, 3L), (task2, 2L))

        "transfer 1" - {
          val transferred = transfers (4L, allocations).toMap
          transferred.getOrElse (task1, 0L) ==> 3L
          transferred.getOrElse (task2, 0L) ==> 1L
        }

        "transfer 2" - {
          val transferred = transfers (8L, allocations).toMap
          transferred.getOrElse (task1, 0L) ==> 3L
          transferred.getOrElse (task2, 0L) ==> 2L
        }

        "transfer 3" - {
          val transferred = transfers (1L, allocations).toMap
          transferred.getOrElse (task1, 0L) ==> 1L
          transferred.getOrElse (task2, 0L) ==> 0L
        }
      }
    }
}
