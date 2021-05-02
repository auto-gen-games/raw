package rawmaterials.world

import scala.annotation.tailrec
import rawmaterials.Utilities._

object World {
  def apply (terrain: Terrain): World =
    terrain.lords.foldLeft (World (terrain, Map.empty, Map.empty, Map.empty, Map.empty, Nil)) {
      case (world, lord) =>
        world
          .changeOwner (lord.home, lord)
          .changeReserve (lord.home, sink = Hub, material = 0, difference = 1L)
          .addToAllocation (rawmaterials.world.Task (lord.home, 0, lord.home, rawmaterials.world.ProducerBuilder (0)), increase = 1L)
    }
}

case class World (terrain: Terrain,
                  owner: Map[Position, Lord],
                  reserves: Map[(Position, Sink, Material), Amount],
                  producers: Map[(Position, Material), Level],
                  allocations: Map[Task, Amount],
                  occurrences: List[Occurrence]) {

  import terrain._

  //
  // READ METHODS
  // First, methods to read summary information from world; after this come world-changing actions
  //

  /** Returns a list of positions accessible from the given one via positions owned by the same lord,
   * ordered starting from the nearest. The list will include (as first element), the starting position itself. */
  def accessibleFrom (from: Position): List[Position] = {
    @tailrec
    def accessibleFrom (toTry: List[Position], gathered: List[Position]): List[Position] =
      toTry match {
        case Nil => gathered.reverse
        case current :: remaining =>
          val next = neighbours (current)
            .filter (owner.get (_) == owner.get (current))
            .filterNot (position => gathered.contains (position) || toTry.contains (position))
          accessibleFrom (remaining ::: next, current :: gathered)
      }
    accessibleFrom (List (from), Nil)
  }

  /** Return the tasks and amounts allocated to each for the given material held at the given position. */
  def allocationsAt (position: Position, material: Material): List[(Task, Amount)] =
    allocations.toList.filter (ta => ta._1.source == position && ta._1.material == material)

  /** Get the positions of the producers of the given lord (or all) which consume the given input material. */
  def consumers (input: Material, lord: Option[Lord]): List[(Position, Level)] =
    positions
      .filter (position => lord.isEmpty || owner.get (position) == lord)
      .flatMap (position => producedUsing (input).map (produced => (position, producer (position, produced))))
      .filter (_._2 > 0)

  /** The list of construction tasks possible to allocate materials to by the given lord anywhere
   * in it's territory */
  def constructionPossibleBy (lord: Lord): List[Task] =
    positionMaterials
      .filter (pm => owner.get (pm._1).contains (lord))
      .flatMap (pm => constructionTasksPossible (pm._1, pm._2))

  /** The list of construction tasks (producers and defence) that the given material can contribute to
   * at the given position. Producers of materials with no deposit are excluded. */
  def constructionTasksPossible (position: Position, material: Material): List[Task] =
    Task (position, material, position, Defence) ::
      materials
        .filter  (produced => deposit (position, produced) > 0)
        .filter  (produced => producerUpgradeCostAt (produced, position)._1 == material)
        .flatMap (produced => List.fill (deposits ((position, produced)))(produced))
        .map     (produced => Task (position, material, position, ProducerBuilder (produced)))

  def defenceMaterialsAt (position: Position): List[(Material, Amount)] =
    materials.map (material => (material, reserve (position, Defence, material)))

  def feedMaterialAt (position: Position, produced: Material): Amount =
    requiredInput (produced) match {
      case Some (input) => reserve (position, ProducerFeed (produced), input)
      case None         => Long.MaxValue
    }

  /** All production feed tasks currently possible in zones owned by the given lord. */
  def feedsPossibleBy (lord: Lord): List[Task] =
    positionMaterials
      .filter (pm => owner.get (pm._1).contains (lord))
      .flatMap (pm => feedTasksPossible (pm._1, pm._2))

  /** The producer feed task at the given position from the given input material, if any. */
  def feedTasksPossible (position: Position, input: Material): Option[Task] =
    producedUsing (input)
      .filter (produced => producer (position, produced) > 0)
      .map (produced => Task (position, input, position, ProducerFeed (produced)))

  /** The amount of the given material in the hub at each position */
  def globalStore (material: Material): Map[Position, Amount] =
    reserves.toList.flatMap {
      case (key, value) =>
        if (key._2 == Hub && key._3 == material) Some ((key._1, value))
        else None
    }.toMap

  /** The military strength of the given sink (Defence or a Siege) at the given position. */
  def militaryStrength (position: Position, sink: MilitarySink): Amount =
    materials.map {
      material => materialStrength (material) * reserve (position, sink, material)
    }.sum

  def neighbours (position: Position): List[Position] =
    moved (position, 1, 0, rows, columns) ::
      moved (position, -1, 0, rows, columns) ::
      moved (position, 0, 1, rows, columns) ::
      moved (position, 0, -1, rows, columns) :: Nil

  def owned (lord: Lord): List[Position] =
    positions.filter (owner.get (_).contains (lord))

  /** For the given lord, returns a map from each material to the number of producers at each owned zone,
   * excluding zones with zero producers. */
  def ownedProducers (lord: Lord): Map[Material, List[(Position, Level)]] =
    materials.map { material =>
      (material, owned (lord).map (position => (position, producer (position, material))).filter (_._2 > 0))
    }.toMap

  def producer (position: Position, material: Material): Level =
    producers.getOrElse ((position, material), 0)

  def producersAt (position: Position): List[(Material, Level)] =
    materials.map (material => (material, producer (position, material)))

  def producerUpgradeCostAt (material: Material, position: Position): (Material, Amount) =
    terrain.producerUpgradeCost (material, producer (position, material))

  def reserve (position: Position, sink: Sink, material: Material): Amount =
    reserves.getOrElse ((position, sink, material), 0L)

  /** The non-zero amount of each material in all sinks at the given position */
  def reservesAt (position: Position): List[(Sink, Material, Amount)] =
    (for (sink <- sinks; material <- materials) yield
      (sink, material, reserve (position, sink, material)))
      .filter (_._3 > 0)

  /** The amount of each material in the given sink of the given position */
  def reservesAtSink (position: Position, sink: Sink): List[(Material, Amount)] =
    materials.map (material => (material, reserve (position, sink, material)))

  /** Return a shortest route from one position to another where all are owned by the same lord, possibly excluding the
   * last position (to allow sieges).
   * If from and to are the same node, the will contain just that position once. */
  def route (from: Position, to: Position): Option[List[Position]] = {
    @tailrec
    def routeAttempt (toTryFrom: List[List[Position]]): Option[List[Position]] =
      toTryFrom match {
        case Nil => None
        case current :: remaining =>
          current match {
            case Nil => None
            case reached :: _ if reached == to => Some (current.reverse)
            case reached :: prior =>
              val next = neighbours (reached)
                .filter (neighbour => neighbour == to || owner.get (neighbour) == owner.get (reached))
                .filterNot (prior.contains)
                .map (_ :: current)
              routeAttempt (remaining ::: next)
          }
      }
    routeAttempt (List (List (from)))
  }

  def siegeableBy (lord: Lord): List[Position] =
    owned (lord).flatMap (neighbours).filterNot (owner.get (_).contains (lord)).distinct

  def siegesAt (position: Position): List[(Lord, Amount)] =
    lords.map (lord => (lord, militaryStrength (position, Siege (lord))))

  def siegeTasksPossibleBy (lord: Lord): List[Task] =
    positionMaterials
      .filter (pm => owner.get (pm._1).contains (lord))
      .flatMap (pm => siegeTasksPossibleAt (pm._1) (pm._2))

  /** Get the sieges possible from the given position, i.e. neighbours with a different lord,
   * returning a function from a material to contribute to the siege to the task to do so */
  def siegeTasksPossibleAt (from: Position): Material => List[Task] =
    material => owner.get (from).map { lord =>
      neighbours (from)
        .filterNot (owner.get (_).contains (lord))
        .map (target => Task (from, material, target, Siege (lord)))
    }.getOrElse (Nil)

  def tasksAllocatedBy (lord: Lord): List[Task] =
    allocations.keys.filter (task => owner.get (task.source).contains (lord)).toList

  def tasksPossibleBy (lord: Lord): List[Task] =
    constructionPossibleBy (lord) ++ siegeTasksPossibleBy (lord) ++ feedsPossibleBy (lord)

  /**
   * The actual amount to transfer of each material at each position, given the allocated amount and
   * amount available.
   */
  def transferrableAmounts: Map[(Position, Material), Map[Task, Amount]] =
    materials.flatMap { material =>
      globalStore (material).toList.map {
        case (position, amount) =>
          ((position, material), transfers (amount, allocationsAt (position, material)).toMap)
      }
    }.toMap


  //
  // WRITE METHODS
  // World-changing actions below here
  //

  def addToAllocation (task: Task, increase: Amount): World = {
    val newAmount = allocations.getOrElse (task, 0L) + increase
    if (newAmount > 0)
      setAllocation (task, newAmount)
    else
      removeAllocation (task)
  }

  /** Add the allocations to the given world for the given amount of material to be routed along the given
   * path of positions to the given final position sink. */
  def addTransferPath (material: Material, route: List[Position], sink: Sink, amount: Amount): World =
      route match {
        case Nil => this
        case position :: Nil =>
          val task = Task (position, material, position, sink)
          addToAllocation (task, amount)
        case position :: end :: Nil =>
          sink match {
            case Siege (_) =>
              addToAllocation (Task (position, material, end, sink), amount)
            case _ =>
              addToAllocation (Task (position, material, end, Hub), amount)
              addToAllocation (Task (end, material, end, sink), amount)
          }
        case start :: next :: rest =>
          addToAllocation (Task (start, material, next, Hub), amount).addTransferPath (material, next :: rest, sink, amount)
      }

  def changeOwner (position: Position, newLord: Lord): World =
    copy (owner = owner + (position -> newLord))

  def changeReserve (position: Position, sink: Sink, material: Material, difference: Amount): World =
    setReserve (position, sink, material,
      (reserve (position, sink, material) + difference).max (0L))

  def clearLog: World =
    copy (occurrences = Nil)

  def conquer: World =
    positions.foldLeft (this) {
      case (world, position) =>
        val (strongest, strength) =
          lords.filter (lord => !world.owner.get (position).contains (lord))
            .map (lord => (lord, world.militaryStrength (position, Siege (lord))))
            .maxBy (_._2)
        if (strength > world.militaryStrength (position, Defence)) {
          val newDefence = world.reservesAtSink (position, Siege (strongest)).map (m => (m._1, m._2 / 2))
          world.changeOwner (position, strongest)
            .removeAllocationsAt (position)
            .removeAllocationsTo (position)
            .removeReserves (position, lords.map (Siege.apply))
            .setReserves (position, Defence, newDefence)
            .log (SectorConquered (position, strongest))
        } else world
    }

  def construct: World =
    positionMaterials.foldLeft (this) {
      case (world, (position, material)) =>
        val (costMaterial, costAmount) = terrain.producerUpgradeCost (material, producer (position, material))
        if (reserve (position, rawmaterials.world.ProducerBuilder (material), costMaterial) >= costAmount)
          world.increaseProduction (position, material, 1)
            .changeReserve (position, rawmaterials.world.ProducerBuilder (material), costMaterial, -costAmount)
            .log (ProducerBuilt (position, material))
        else world
    }

  def enactInstantiatedTask (task: Task, amount: Amount): World =
    moveReserves (task.source, Hub, task.target, task.sink, task.material, amount)

  def enactTasks: World = {
    val amount: Map[(Position, Material), Map[Task, Amount]] = transferrableAmounts
    allocations.keys.foldLeft (this) {
      case (world, task) =>
        amount.get ((task.source, task.material)).flatMap {
          _.get (task).map (quantity => world.enactInstantiatedTask (task, quantity))
        }.getOrElse (world)
    }
  }

  def increaseProduction (position: Position, material: Material, levels: Int): World =
    copy (producers = producers + ((position, material) -> (producer (position, material) + levels)))

  def log (occurrence: Occurrence): World =
    copy (occurrences = occurrences :+ occurrence)

  def moveReserves (source: Position, outOf: Sink, target: Position, into: Sink, material: Material, amount: Amount): World =
    changeReserve (source, outOf, material, -amount).changeReserve (target, into, material, amount)

  def produce: World =
    positionMaterials.foldLeft (this) {
      case (world, (position, material)) => world.produceMaterialAt (position, material)
    }

  /** Consume the given amount of input material to produce the given material at the given position. */
  def produceFromInput (position: Position, produced: Material, input: Option[Material], consumed: Amount): World =
    if (consumed > 0)
      input.map (feedMaterial => changeReserve (position, ProducerFeed (produced), feedMaterial, -consumed))
        .getOrElse (this)
        .changeReserve (position, Hub, produced, consumed * terrain.deposits ((position, produced)))
    else this


  /** Produce the given material at the given position from all the given input material held in reserves */
  def produceMaterialAt (position: Position, material: Material): World =
    produceFromInput (position, material, requiredInput (material),
      feedMaterialAt (position, material).min (producer (position, material).toLong))

  def removeAllocation (task: Task): World =
    copy (allocations = allocations - task)

  def removeAllocationsAt (position: Position): World =
    copy (allocations = allocations.filterNot (_._1.source == position))

  def removeAllocationsTo (position: Position): World =
    copy (allocations = allocations.filterNot (_._1.target == position))

  def removeAllocationsBy (lord: Lord): World =
    copy (allocations = allocations.filterNot (ta => owner.get (ta._1.source).contains (lord)))

  def removeReserves (position: Position, sink: Sink): World =
    copy (reserves = reserves -- materials.map ((position, sink, _)))

  def removeReserves (position: Position, sinks: List[Sink]): World =
    sinks.foldLeft (this) { case (world, sink) => world.removeReserves (position, sink) }

  def setAllocation (task: Task, amount: Amount): World =
    copy (allocations = allocations + (task -> amount))

  def setReserve (position: Position, sink: Sink, material: Material, newAmount: Amount): World =
    copy (reserves = reserves + ((position, sink, material) -> newAmount))

  def setReserves (position: Position, sink: Sink, newAmounts: List[(Material, Amount)]): World =
    newAmounts.foldLeft (this) {
      case (world, (material, newAmount)) => world.setReserve (position, sink, material, newAmount)
    }

  def update: World =
    clearLog.produce.enactTasks.construct.conquer
}
