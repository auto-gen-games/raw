package rawmaterials

import indigo.shared.datatypes.RGBA
import rawmaterials.ai.{BasicLord, LordAI}
import rawmaterials.world.{Lord, Terrain, World}

object TestWorld {
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
  val lordAIs: List [LordAI] = List (BasicLord (enemy))
  val terrain: Terrain       =
    Terrain (rows, columns, numberOfMaterials, lords, depositsPerZone, homeMaterialDeposits,
      levelOffsetPerMaterial, levelsPerPriceMaterial)
  val world: World           = World (terrain)
}
