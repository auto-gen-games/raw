package rawmaterials

import rawmaterials.game.GameAssets
import utest._

object GameTests extends TestSuite {
  val tests: Tests =
    utest.Tests {
      "assets" - {
        "graphics loading" - {
          GameAssets.cell.moveTo (10, 10).position.x ==> 10
        }
      }
    }
}
