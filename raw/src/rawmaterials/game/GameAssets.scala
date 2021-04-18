package rawmaterials.game

import indigo._

object GameAssets {
  val imageFiles: Set[String] = Set ("base", "cell", "defence", "log", "no-production", "siege", "roboto-font") ++
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

  val base: Graphic            = graphic ("base", 64, 64)
  val cell: Graphic            = graphic ("cell", 64, 64)
  val defence: Graphic         = graphic ("defence", 64, 64)
  val log: Graphic             = graphic ("log", 20, 20)
  val noProduction: Graphic    = graphic ("no-production", 32, 32)
  val siege: Graphic           = graphic ("siege", 64, 64)
  val producers: List[Graphic] = (0 to 15).map (material => graphic (s"producers$material", 32, 32)).toList
  val fontKey: FontKey         = FontKey ("Roboto font")
}
