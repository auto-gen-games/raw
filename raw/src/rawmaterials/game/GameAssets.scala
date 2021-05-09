package rawmaterials.game

import indigo._

object GameAssets {
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

  val materials: Map[String, Material.Textured] =
    imageFiles.map (image => (image, Material.Textured (AssetName (image)))).toMap

  def graphic (asset: String, width: Int, height: Int): Graphic =
    Graphic (0, 0, width, height, 2, materials (asset))

  val base: Graphic              = graphic ("base", 64, 64)
  val cell: Graphic              = graphic ("cell", 64, 64)
  val defence: Graphic           = graphic ("defence", 64, 64)
  val log: Graphic               = graphic ("log", 20, 20)
  val noProduction: Graphic      = graphic ("no-production", 32, 32)
  val siege: Graphic             = graphic ("siege", 64, 64)
  val producers: List[Graphic]   = (0 to 15).map (material => graphic (s"producers$material", 32, 32)).toList
  val menuBackground: Graphic    = graphic ("menu-background", 32, 32)
  val optionsBackground: Graphic = graphic ("option-background", 16, 16)
  val fontKey: FontKey           = FontKey ("Roboto font")

  val nextOption: Graphic = graphic ("next-option", 16, 16)
  val optionIcons: List[Graphic] =
    List (nextOption.flipHorizontal (true), nextOption) ++
      List ("military-option", "deposits-option", "producers-option", "balance-option", "build-option",
        "transport-option", "siege-option", "close-option")
        .map (graphic (_, 16, 16))
  val decreaseButton: Graphic = graphic ("decrease", 16, 16)
  val increaseButton: Graphic = decreaseButton.flipVertical (true)

  val optionGroup: List[Int] = List (0, 0, 0, 1, 1, 1, 2, 2, 2, 3)
  def optionPos (index: Int): Int = 4 + index * 20 + optionGroup (index) * 40



}
