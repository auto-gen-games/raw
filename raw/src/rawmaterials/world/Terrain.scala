package rawmaterials.world

import scala.util.Random.{nextInt => randomInt}
import rawmaterials.Settings.materialNames

/** The constant elements of the game world */
case class Terrain (rows: Int, columns: Int, lords: List[Lord], levelOffsetPerMaterial: Int,
  levelsPerPriceMaterial: Int, materials: List[Material], deposits: Map[(Position, Material), Int]) {

  /** All positions on the world map */
  val positions: List[Position] = (for (r <- 0 until rows; c <- 0 until columns) yield ((r, c))).toList

  /** All combinations of positions and materials in the world */
  val positionMaterials: List[(Position, Material)] = for (p <- positions; m <- materials) yield (p, m)

  val sinks: List[Sink] =
    List (Hub, Defence) ++ materials.map (ProducerFeed.apply) ++ materials.map (ProducerBuilder.apply) ++ lords.map (Siege.apply)

  def addRandomDepositsAt (position: Position, number: Int): Terrain =
    if (number > 0) {
      val material = randomInt (materials.size)
      copy (deposits = deposits + ((position, material) -> (deposits.getOrElse ((position, material), 0) + 1)))
        .addRandomDepositsAt (position, number - 1)
    } else this

  def addRandomDepositsEverywhere (numberAtEach: Int): Terrain =
    positions.foldLeft (this) {
      case (world, position) => world.addRandomDepositsAt (position, numberAtEach)
    }

  def deposit (position: Position, material: Material): Int =
    deposits.getOrElse ((position, material), 0)

  def depositsAt (position: Position): List[(Material, Int)] =
    materials.map (material => (material, deposit (position, material)))

  def locationName (position: Position): String =
    position.toString

  def materialName (material: Material): String =
    materialNames (material) //s"material $material"

  def materialStrength (material: Material): Long =
    Math.pow (2, material).toLong

  def producedUsing (material: Material): Option[Material] =
    if (material < materials.size - 1) Some (material + 1) else None

  def producerUpgradeCost (material: Material, level: Level): (Material, Amount) = {
    // The effective level is the level as if this were material 0
    val effective = material * levelOffsetPerMaterial + level
    // The material required for upgrade changes every levelsPerPriceMaterial levels
    val priceMaterial = (effective / levelsPerPriceMaterial) % materials.size
    // The amount is a power of 2, going back down with each change of material but starting higher each time
    val priceAmount = Math.pow (2, effective % levelsPerPriceMaterial + priceMaterial).toLong
    (priceMaterial, priceAmount)
  }

  def requiredInput (material: Material): Option[Material] =
    if (material > 0) Some (material - 1) else None

  def setUniformDeposits (position: Position, numberOfEach: Int): Terrain =
    copy (deposits = deposits ++ materials.map (material => ((position, material), numberOfEach)).toMap)
}

object Terrain {
  def apply (rows: Int, columns: Int, numberOfMaterials: Int, lords: List[Lord], depositsPerZone: Int,
    homeMaterialDeposits: Int, levelOffsetPerMaterial: Int, levelsPerPriceMaterial: Int): Terrain = {

    val empty = Terrain (rows, columns, lords, levelOffsetPerMaterial, levelsPerPriceMaterial,
      (0 until numberOfMaterials).toList, Map.empty).addRandomDepositsEverywhere (depositsPerZone)
    lords.foldLeft (empty) {
      case (terrain, lord) => terrain.setUniformDeposits (lord.home, homeMaterialDeposits)
    }
  }
}
