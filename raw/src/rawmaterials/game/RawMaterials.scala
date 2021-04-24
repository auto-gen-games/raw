package rawmaterials.game

import scala.scalajs.js.annotation.JSExportTopLevel
import indigo.scenes.{Scene, SceneName}
import indigo._
import indigo.json.Json
import rawmaterials.Settings
import rawmaterials.Settings.DefaultWorld
import rawmaterials.game.GameAssets.fontKey

@JSExportTopLevel ("IndigoGame")
object RawMaterials extends IndigoGame[GameViewport, ReferenceData, GameModel, ViewModel] {
  val eventFilters: EventFilters = EventFilters.AllowAll

  def scenes (bootData: GameViewport): NonEmptyList[Scene[ReferenceData, GameModel, ViewModel]] =
    NonEmptyList (PlayScene)

  def initialScene (bootData: GameViewport): Option[SceneName] =
    Some (PlayScene.name)

  def boot (flags: Map[String, String]): Outcome[BootResult[GameViewport]] = {
    val assetPath: String = flags.getOrElse ("baseUrl", "")
    val config = GameConfig (
      viewport = GameViewport (Settings.initialViewportWidth, Settings.initialViewportHeight),
      frameRate = 60,
      clearColor = RGBA.Black,
      magnification = Settings.magnificationLevel
    )

    Outcome (
      BootResult (config, config.viewport)
        .withAssets (GameAssets.assets (assetPath))
    )
  }

  def setup (viewport: GameViewport, assetCollection: AssetCollection, dice: Dice): Outcome[Startup[ReferenceData]] = {
    def makeFontInfo (unknownChar: FontChar, fontChars: List[FontChar]): FontInfo =
      FontInfo (
        fontKey         = fontKey,
        fontSpriteSheet = FontSpriteSheet (Material.Textured (AssetName ("roboto-font")), Point (271, 232)),
        unknownChar     = unknownChar,
        fontChars       = fontChars,
        caseSensitive   = true,
      ).addChar (FontChar (" ", 280, 0, 4, 4))

    Outcome (assetCollection.findTextDataByName (AssetName ("roboto-font")).map {
      json => Json.readFontToolJson (json).map {
        chars => chars.find (_.character == "?").map {
          unknownChar => Startup.Success (ReferenceData (viewport)).addFonts (makeFontInfo (unknownChar, chars))
        }.getOrElse (Startup.Failure ("Couldn't find unknown char in font"))
      }.getOrElse (Startup.Failure ("Couldn't parse JSON font data"))
    }.getOrElse (Startup.Failure ("Couldn't find JSON file")))
  }

  def initialModel (startupData: ReferenceData): Outcome[GameModel] =
    Outcome (GameModel (DefaultWorld.world, DefaultWorld.lordAIs))

  def initialViewModel (startupData: ReferenceData, model: GameModel): Outcome[ViewModel] =
    Outcome (ViewModel (startupData.initialViewport))

  def updateModel (context: FrameContext[ReferenceData], model: GameModel): GlobalEvent => Outcome[GameModel] =
    _ => Outcome (model)

  def updateViewModel (context: FrameContext[ReferenceData], model: GameModel, viewModel: ViewModel): GlobalEvent => Outcome[ViewModel] = {
    case ViewportResize (newViewport) => Outcome (viewModel.copy (viewport = newViewport))
    case _ => Outcome (viewModel)
  }

  def present (context: FrameContext[ReferenceData], model: GameModel, viewModel: ViewModel): Outcome[SceneUpdateFragment] =
    Outcome (SceneUpdateFragment.empty)
}
