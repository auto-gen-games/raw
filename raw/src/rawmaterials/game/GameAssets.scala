package rawmaterials.game

import indigo._

object GameAssets {
  type Bitmap = Graphic[Material.Bitmap]

  val imageFiles: Set[String] = Set ("base", "cell", "defence", "log", "no-production", "siege", "roboto-font",
    "balance-option", "build-option", "close-option", "deposits-option", "menu-background", "military-option", "next-option",
    "option-background", "producers-option", "siege-option", "transport-option", "decrease") ++
    (0 to 15).map (material => s"producers$material")
  val buttonFiles: Set[String] = Set ()
  val textFiles: Set[String] = Set ()
  val jsonFiles: Set[String] = Set ("roboto-font")
  val audioFiles: Set[String] = Set ()

  def assets (baseUrl: String): Set[AssetType] =
    imageFiles.map   (file => AssetType.Image (AssetName (file), AssetPath (baseUrl + s"assets/$file.png"))) ++
      buttonFiles.map (file => AssetType.Image (AssetName (file), AssetPath (baseUrl + s"assets/$file.png"))) ++
      textFiles.map   (file => AssetType.Text  (AssetName (file), AssetPath (baseUrl + s"assets/$file.txt"))) ++
      audioFiles.map  (file => AssetType.Audio (AssetName (file), AssetPath (baseUrl + s"assets/$file.mp3"))) ++
      jsonFiles.map   (file => AssetType.Text  (AssetName (file), AssetPath (baseUrl + s"assets/$file.json")))

  val materials: Map[String, Material.Bitmap] =
    imageFiles.map (image => (image, Material.Bitmap (AssetName (image)))).toMap

  def graphic (asset: String, width: Int, height: Int): Bitmap =
    Graphic[Material.Bitmap] (0, 0, width, height, 2, materials (asset))

  val base: Bitmap              = graphic ("base", 64, 64)
  val cell: Bitmap              = graphic ("cell", 64, 64)
  val defence: Bitmap           = graphic ("defence", 64, 64)
  val log: Bitmap               = graphic ("log", 20, 20)
  val noProduction: Bitmap      = graphic ("no-production", 32, 32)
  val siege: Bitmap             = graphic ("siege", 64, 64)
  val producers: List[Bitmap]   = (0 to 15).map (material => graphic (s"producers$material", 32, 32)).toList
  val menuBackground: Bitmap    = graphic ("menu-background", 32, 32)
  val optionsBackground: Bitmap = graphic ("option-background", 16, 16)

  val nextOption: Bitmap = graphic ("next-option", 16, 16)
  val optionIcons: List[Bitmap] =
    List (nextOption.flipHorizontal (true), nextOption) ++
      List ("military-option", "deposits-option", "producers-option", "balance-option", "build-option",
        "transport-option", "siege-option", "close-option")
        .map (graphic (_, 16, 16))
  val decreaseButton: Bitmap = graphic ("decrease", 16, 16)
  val increaseButton: Bitmap = decreaseButton.flipVertical (true)

  val optionGroup: List[Int] = List (0, 0, 0, 1, 1, 1, 2, 2, 2, 3)
  def optionPos (index: Int): Int = 4 + index * 20 + optionGroup (index) * 80

  val fontMaterial              = Material.ImageEffects (AssetName ("roboto-font"))
  val fontKey: FontKey          = FontKey ("Roboto font")
  def NormalText (text: String, x: Int, y: Int, layer: Int, colour: RGBA) = 
    Text (text, x, y, layer, fontKey, fontMaterial.withOverlay (Fill.Color (colour)))
  def SolidBox (x: Int, y: Int, width: Int, height: Int, colour: RGBA): Shape.Box =
    Shape.Box (Rectangle (Point (x, y), Size (width, height)), Fill.Color (colour), Stroke (1, colour))
}
