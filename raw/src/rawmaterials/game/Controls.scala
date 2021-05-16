package rawmaterials.game

import indigo.{GameViewport, Graphic, Group, Overlay, RGBA, Text}
import indigo.shared.input.Mouse
import rawmaterials.Settings.{adjustableValueX, decreaseX, fixedValueX, increaseX}
import rawmaterials.game.GameAssets._
import rawmaterials.game.PlayScene.drawBar
import rawmaterials.world._

object Controls {
  /** The state currently being displayed in the controls. Gives convenience common read methods. */
  case class InfoState (position: Position, material: Material, lord: Lord, world: World) {
    def materialName (material: Material): String = world.terrain.materialName (material)
  }

  /** A line of information to be displayed on the controls, where the value is either a fixed amount or
   * an adjustable allocation of material to a given position and sink. */
  case class InfoLine (label: String, value: Amount, material: Option[String], update: Option[(Position, Sink)])

  object InfoLine {
    def apply (label: String, value: Amount): InfoLine =
      InfoLine (label, value, None, None)

    def apply (label: String, value: (Material, Amount), state: InfoState): InfoLine =
      InfoLine (label, value._2, Some (state.materialName (value._1)), None)

    def apply (label: String, target: Position, sink: Sink, state: InfoState): InfoLine =
      InfoLine (label, allocatedTo (state, target, sink), None, Some ((target, sink)))
  }

  def allocatedTo (state: InfoState, target: Position, sink: Sink): Amount =
    state.world.allocationsAt (state.position, state.material)
      .filter (ta => ta._1.target == target && ta._1.sink == sink)
      .map (_._2).sum

  def neighbourName (from: Position, to: Position): String =
    if (to._1 < from._1) "north"
    else if (to._1 > from._1) "south"
    else if (to._2 < from._2) "west"
    else "east"

  type InfoLineGenerator = InfoState => List[InfoLine]

  def fixedLines (lines: (InfoState => InfoLine)*): InfoLineGenerator =
    state => lines.map (_ (state)).toList

  def variableLines[Element] (elements: InfoState => Iterable[Element], lines: (Element, InfoState) => InfoLine): InfoLineGenerator =
    state => elements (state).map (element => lines (element, state)).toList

  def computedLines[Computed] (compute: InfoState => Computed, lines: Computed => List[InfoLine]): InfoLineGenerator =
    state => lines (compute (state))

  def combinedLines (generator1: InfoLineGenerator, generator2: InfoLineGenerator): InfoLineGenerator =
    state => generator1 (state) ::: generator2 (state)

  val infoLines: Map[AllocateView, InfoLineGenerator] = Map (
    DepositsView -> fixedLines (
      s => InfoLine ("Strength", s.world.terrain.materialStrength (s.material)),
      s => InfoLine ("Deposit",  s.world.terrain.deposit (s.position, s.material))
    ),
    ProducersView -> fixedLines (
      s => InfoLine ("Producers", s.world.producer (s.position, s.material)),
      s => InfoLine ("Upgrade", s.world.producerUpgradeCostAt (s.material, s.position), s),
      s => InfoLine ("Stored",  s.world.reserve (s.position, Hub, s.material))
    ),
    BalanceView -> computedLines[(Amount, Amount, Amount)] ({ s =>
      (s.world.producer (s.position, s.material) * s.world.terrain.deposit (s.position, s.material).toLong,
        s.world.allocatedImports (s.position, s.material).map (_._2).sum,
        s.world.allocationsAt (s.position, s.material).map (_._2).sum)
    }, {
      case (produced, incoming, allocated) => List (
        InfoLine ("Produced", produced),
        InfoLine ("Incoming", incoming),
        InfoLine ("Allocated", allocated),
        InfoLine ("Balance", produced + incoming - allocated)
      )
    }),
    BuildView ->
      combinedLines (
        variableLines[Material] (s => s.world.terrain.producedUsing (s.material), {
          case (produced, state) =>
            InfoLine (s"Feed ${state.materialName (produced)}", state.position, ProducerFeed (produced), state)
        }),
        variableLines[Material] (s => s.world.producerBuildsPossibleFrom (s.position, s.material), {
          case (produced, state) =>
            InfoLine (s"Build ${state.materialName (produced)} mine", state.position, ProducerBuilder (produced), state)
        })
      ),
    TransportView ->
      variableLines[Position] (s => s.world.transportableNeighboursAt (s.position), {
        case (neighbour, state) =>
          InfoLine (s"Transport to ${neighbourName (state.position, neighbour)}", neighbour, Hub, state)
      }),
    SiegeView ->
      combinedLines (
        fixedLines (s => InfoLine ("Feed defence", s.position, Defence, s)),
        variableLines[Position] (s => s.world.siegeableNeighboursAt (s.position), {
          case (neighbour, state) =>
            InfoLine (s"Siege ${neighbourName (state.position, neighbour)}", neighbour, Siege (state.lord), state)
        })
      ),
  )

  val militaryLines: InfoLineGenerator =
    state =>
      InfoLine ("Defence", state.world.militaryStrength (state.position, Defence)) ::
        state.world.siegesAt (state.position).map (la => InfoLine (s"Siege by ${la._1.name}", la._2))

  def updateInfoLines (view: ControlView, player: Lord, world: World): ControlView = {
    val state = InfoState (view.zone, view.material, player, world)
    view.copy (infoLines = if (view.militaryView) militaryLines (state) else infoLines (view.allocateView)(state))
  }

  def tintIfHover (graphic: Graphic, x: Int, y: Int, mouse: Mouse): Graphic =
    if (mouse.position.x >= x && mouse.position.x < x + 16 && mouse.position.y >= y && mouse.position.y < y + 16)
      graphic.withTint (RGBA.Magenta)
    else graphic

  def titleBar (mouse: Mouse): Group =
    Group (
      Group (optionIcons.indices.map { index =>
        val x = optionPos (index)
        tintIfHover (optionsBackground.moveTo (x, 0).withTint (RGBA (0.3, 0.3, 0.3)).withAlpha (0.9), x, 0, mouse)
      }.toList),
      Group (optionIcons.zipWithIndex.map { case (image, index) => image.moveTo (optionPos (index), 0) })
    )

  def optionOver (x: Int, y: Int): Option[Int] =
    if (y < 0 || y >= 16) None
    else
      optionIcons.indices.find { index =>
        val optionX = optionPos (index)
        x >= optionX && x < optionX + 16
      }

  def showInfo (lines: List[InfoLine], mouse: Mouse): Group =
    Group (
      lines.zipWithIndex.flatMap { line =>
        List (Text (line._1.label, 200, line._2 * 20 + 18, 1, fontKey).withOverlay (Overlay.Color (RGBA.White)),
          line._1.update match {
            case None =>
              Text (line._1.value + " " + line._1.material.getOrElse (""), fixedValueX, line._2 * 20 + 18, 1, fontKey).withOverlay (Overlay.Color (RGBA.White))
            case Some (_) =>
              Group (
                tintIfHover (decreaseButton.moveTo (decreaseX, line._2 * 20 + 20), decreaseX, line._2 * 20 + 20, mouse),
                Text (line._1.value.toString, adjustableValueX, line._2 * 20 + 18, 1, fontKey).withOverlay (Overlay.Color (RGBA.Yellow)),
                tintIfHover (increaseButton.moveTo (increaseX, line._2 * 20 + 18), increaseX, line._2 * 20 + 20, mouse)
              )
          })
      }
    )

  /** Returns a value if the (x, y) position is over an allocation adjustment button in the given info lines,
   * where the return value is the line is false for decrease button or true for increase button. */
  def adjustOver (x: Int, y: Int, lines: List[InfoLine]): Option[(InfoLine, Boolean)] =
    lines.zipWithIndex.filter (_._1.update.isDefined)
      .flatMap (line => List ((line, false), (line, true)))
      .find { case ((line, index), isIncrease) =>
        ((!isIncrease && x >= decreaseX && x < decreaseX + 16) || (isIncrease && x > increaseX && x < increaseX + 16)) &&
          y >= index * 20 + (if (!isIncrease) 20 else 18) && y < index * 20 + (if (!isIncrease) 20 else 18) + 16
      }.map (lii => (lii._1._1, lii._2))

  def infoBar (view: ControlView, player: Lord, world: World, viewport: GameViewport, mouse: Mouse): Group =
    Group (
      drawBar (menuBackground, 32, (0, 18), 200, 1, RGBA (0.3, 0.3, 0.3)),
      drawBar (log, 20, (200, 18), viewport.width - 200, view.infoLines.size, RGBA (0.3, 0.3, 0.3)),
      if (!view.militaryView)
        Group (producers (view.material).moveTo (0, 18),
          Text (world.terrain.materialName (view.material), 36, 22, 1, fontKey).withOverlay (Overlay.Color (RGBA.White)))
      else
        Text ("military", 36, 22, 1, fontKey).withOverlay (Overlay.Color (RGBA.White)),
      showInfo (view.infoLines, mouse)
    )

  def controls (view: ControlView, player: Lord, world: World, viewport: GameViewport, mouse: Mouse): Group =
    Group (titleBar (mouse), infoBar (view, player, world, viewport, mouse))
}
