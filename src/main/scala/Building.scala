abstract class Building(player: Player, Loc: Tile) {
  var buildingtype: String
  var Player: Player



}

class HQ(player: Player, Loc: Tile) extends Building(player, Loc) {
  var buildingtype = "HQ"
  var Player = player
  var location = Loc

  Loc.addBuilding(this)




}
