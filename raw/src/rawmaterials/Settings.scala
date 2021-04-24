package rawmaterials

import indigo.shared.datatypes.RGBA
import rawmaterials.ai.{BasicLord, LordAI}
import rawmaterials.world.{Lord, Terrain, World}

object Settings {
  object DefaultWorld {
    val rows                   = 12
    val columns                = 12
    val numberOfMaterials      = 16
    val depositsPerZone        = 16
    val homeMaterialDeposits   = 1
    val levelOffsetPerMaterial = 4
    val levelsPerPriceMaterial = 8
    val player: Lord           = Lord ("Player", (0, 0), RGBA.Cyan)
    val enemy: Lord            = Lord ("Enemy",  (6, 6), RGBA.Yellow)
    val lords: List[Lord]      = List (player, enemy)
    //val lordAIs: List [LordAI] = List (BasicLord (enemy))
    val lordAIs: List [LordAI] = List (BasicLord (player), BasicLord (enemy))
    val terrain: Terrain       =
      Terrain (rows, columns, numberOfMaterials, lords, depositsPerZone, homeMaterialDeposits,
               levelOffsetPerMaterial, levelsPerPriceMaterial)
    val world: World           = World (terrain)
  }

  val minDefenceAlpha = 0.2
  val maxDefenceAlpha = 0.8

  val materialNames = List ("abundantium", "bountifine", "copiese", "dozeenum", "eaesium", "feasibidium",
    "generosium", "handium", "intricanium", "justonium", "kinc", "lackel", "meeger", "needium", "obscurium", "powerine")

  val initialViewportWidth  = 512
  val initialViewportHeight = 522
  val magnificationLevel    = 1
}
