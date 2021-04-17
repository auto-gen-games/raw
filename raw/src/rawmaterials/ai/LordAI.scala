package rawmaterials.ai

import indigo.GameTime
import rawmaterials.world.World

trait LordAI {
  /** Checks whether this AI has done all the reallocation it wants before the next round starts */
  def readyToProceed (world: World): Boolean

  /** Informs the AI that a new round has now started, possibly changing the AI's state */
  def newRoundStarted (world: World): LordAI

  /** Perform reallocations and update the AI state within a round: this will be called iteratively
   * until the AIs are ready to move to the next round allowing iterative execution */
  def update (world: World, gameTime: GameTime): (World, LordAI)
}
