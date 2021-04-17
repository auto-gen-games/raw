package rawmaterials

import scala.util.Random.{nextInt => randomInt}
import world._

object Utilities {
  /** Given a maximum available amount, and how much to ideally allocate to each of a list of targets, return
   * an allocation to each target not exceeding the maximum. */
  def transfers[Target] (amount: Amount, allocations: List[(Target, Amount)]): List[(Target, Amount)] =
    allocations match {
      case (target, allocated) :: rest =>
        if (amount <= allocated) List ((target, amount))
        else (target, allocated) :: transfers (amount - allocated, rest)
      case Nil => Nil
    }

  /**
   * Ensure a value, which may be negative, is within 0 to range-1 by treating as circular
   */
  def within (value: Int, range: Int): Int =
    ((value % range) + range) % range

  def moved (position: Position, drow: Int, dcolumn: Int, rows: Int, columns: Int): Position =
    (within (position._1 + drow, rows), within (position._2 + dcolumn, columns))

  def distance (position1: Position, position2: Position, rows: Int, columns: Int): Int =
    (position1._1 - position2._1).abs.min (rows - (position1._1 - position2._1).abs) +
      (position1._2 - position2._2).abs.min (columns - (position1._2 - position2._2).abs)

  def choose[Item] (list: List[Item]): Option[Item] =
    if (list.isEmpty) None
    else Some (list (randomInt (list.size)))
}
