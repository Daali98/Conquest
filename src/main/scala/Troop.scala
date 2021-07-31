import scala.math._


abstract class Troop(Player: Player, Loc: Tile) {
 // def takeDamage: Unit
  var player = Player
  var currentHP: Int
  var attack_strength: Vector[Int]
  var max_health: Int
  var current_location: Option[Tile]
  def move(moveToTile: Tile): Unit
  def isDead = currentHP == 0

  def takeDamage(otherTroop: Troop) = {

   var damage = otherTroop.attack_strength(player.Game.randomizer(otherTroop.attack_strength.size))
   var takenDamage = min(currentHP, damage)
   currentHP = max(currentHP - damage, 0)

   if(!this.isDead) {
     player.Game.randomizerNumber += 1
     var otherTroopDamage = this.attack_strength(player.Game.randomizer(otherTroop.attack_strength.size))
     var othertakenDamage = min(otherTroop.currentHP, otherTroopDamage)
     otherTroop.currentHP = max(otherTroop.currentHP - damage, 0)

     player.Game.GUI.eventLabel(player.Game.activePlayer.get.id + "'s troop deals " + takenDamage + " to " + this.player.id + "'s troop at " + this.current_location.get.idx + ", " + this.current_location.get.idy + " and takes " + othertakenDamage + " damage")

   }
   else {
     if(!otherTroop.isDead) {
     player.Game.GUI.eventLabel(player.Game.activePlayer.get.id + " kills " + this.player.id + "'s troop at " + this.current_location.get.idx + ", " + this.current_location.get.idy)
     }
     else {
       player.Game.GUI.eventLabel(player.Game.activePlayer.get.id + "'s troop at " + otherTroop.current_location.get.idx + ", " + otherTroop.current_location.get.idy + " deals " + takenDamage + " damage and dies")
     }
   }
  }


  }

class Swordsman(Player: Player, Loc: Tile) extends Troop(Player, Loc) {
  var currentHP = 10
  var attack_strength = 2.to(4).toVector
  var max_health = 10
  var current_location: Option[Tile] = Some(Loc)
  var stepsTaken = 0
  var AIDesignatedRole = ""



  def isEnemy(otherTroop: Troop) = {
    otherTroop.player != this.player
  }

  def isEnemyBuilding(otherBuilding: Building) = {
    otherBuilding.Player != this.player
  }


  def move(moveToTile: Tile): Unit = {
    if(moveToTile.tileUnoccupied() && Player.energy >= 1) {

    current_location.get.hasUnit = None
    current_location = Some(moveToTile)
    moveToTile.hasUnit = Some(this)
    Player.energy -= 1
    this.stepsTaken += 1

  }

  if(moveToTile.hasUnit.isDefined) {
      if(isEnemy(moveToTile.hasUnit.get)) {

        moveToTile.hasUnit.get.takeDamage(this)
        if(moveToTile.hasUnit.get.isDead) {

          moveToTile.hasUnit.get.player.removeDead(moveToTile.hasUnit.get)
          moveToTile.hasUnit = None


        }
        if(this.isDead) {
          this.player.removeDead(this)
          this.current_location.get.hasUnit = None
        }
      }

  }

  if(moveToTile.hasBuilding.isDefined) {
      if(isEnemyBuilding(moveToTile.hasBuilding.get)) {
        if(moveToTile.hasBuilding.get.buildingtype == "HQ") {
          player.capturedHQ = Some(moveToTile.hasBuilding.get)
          player.capturedHQ.get.Player = player
          if(player.capturedHQ.isDefined) {
            player.Game.GUI.endGame(player)
          }


        }
      }
  }

  if(Player.energy == 0) {
      //println("Not enough energy! Current energy: " + Player.energy)
    }
    else {
      //println("Cannot Move")
   }

    player.Game.GUI.updateGui()
  }
}




