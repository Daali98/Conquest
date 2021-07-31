import scala.collection.mutable
import scala.math._


abstract class Player(Name: String, game: Game) {
  var points: Int
  var id: String
  var energy: Int
  var Troops: mutable.Buffer[Swordsman]
  var HQ: Option[HQ]
  var Buildings: Array[Building]
  var name: String
  var Game = game

  var selectedTroop: Option[Troop]
  var capturedHQ: Option[Building]
  def act(): Unit

  def regainEnergy(): Unit

  def deployTroop(tile: Tile): Boolean




  def endTurn(): Unit

  def removeDead(troop: Swordsman): Unit

}

class User(Name: String, game: Game) extends Player(Name, game){

  var name = Name
  var id = "User"
  var points = 0
  var energy = 10
  var Troops = mutable.Buffer[Swordsman]()
  var HQ: Option[HQ] = None
  var Buildings = Array[Building]()
  var capturedHQ: Option[Building] = None

  var selectedTroop: Option[Troop] = None

  def act() = {

  }

  def removeDead(troop: Swordsman) = {
     Troops -= troop
  }

  def deployTroop(tile: Tile): Boolean = {

    if(energy >= HQ.get.location.determineTileDistance(tile) + 2) {

    energy -= (HQ.get.location.determineTileDistance(tile) + 2)
      game.GUI.updateGui()

      true
  }
    else {
      println("Not enough energy!")
      game.GUI.updateGui()
      false
    }
  }

  def regainEnergy(): Unit = {
    energy = min(10, energy + 2)
  }


  def endTurn() = {
      game.activePlayer = Some(game.players("AI"))
      game.turnCount += 1
      game.randomizerNumber += 1
      game.players("AI").act()
      regainEnergy()
      game.GUI.updateGui()

  }



}

class AI(Name: String, game: Game) extends Player(Name, game) {

  var name = Name
  var id = "AI"
  var points = 0
  var energy = 10
  var Troops = mutable.Buffer[Swordsman]()
  var HQ: Option[HQ] = None
  var Buildings = Array[Building]()
  var capturedHQ: Option[Building] = None
  var enemy = game.players("User")
  var troopRoles = mutable.Buffer[String]()

  var selectedTroop: Option[Troop] = None

  def removeDead(troop: Swordsman) = {
     troopRoles += troop.AIDesignatedRole
     Troops -= troop
  }

  def regainEnergy(): Unit = {
    energy = min(10, energy + 3)
  }


  def deployTroop(tile: Tile): Boolean = {

    if(energy >= HQ.get.location.determineTileDistance(tile) + 2) {

    energy -= (HQ.get.location.determineTileDistance(tile) + 2)

      var troop = new Swordsman(this, tile)
      troop.AIDesignatedRole = troopRoles.head
      Troops += troop
      troopRoles -= troop.AIDesignatedRole
      tile.addTroop(troop)
      game.GUI.updateGui()

       true

  }
    else {
      println("Not enough energy!")
      game.GUI.updateGui()
      false
    }



  }

  def endTurn() = {

    game.activePlayer = Some(game.players("User"))
    regainEnergy()
    game.GUI.updateGui()
  }

  def enemyIntel() = {

    // Type of intel; Closest enemy troop to it's own base and to AI base, Closest AI troop to it's base and enemy base
    // enemy current strategy 0 for defensive, 1 for offfensive

    var intel = Map[String, Int]()


    var enemyTotalStrength = 0
    var ownTotalStrength = 0

    if(enemy.Troops.nonEmpty) {


    var distancesEnemyToOwnHQ = mutable.Buffer[Int]()
    var distancesEnemyToEnemyHQ = mutable.Buffer[Int]()


    for (x <- enemy.Troops) {
      var distance = HQ.get.location.determineTileDistance(x.current_location.get)
      distancesEnemyToOwnHQ += distance
      enemyTotalStrength += x.currentHP

    }

    for (x <- enemy.Troops) {
      var distance = enemy.HQ.get.location.determineTileDistance(x.current_location.get)
      distancesEnemyToEnemyHQ += distance


    }

      var combined = distancesEnemyToEnemyHQ.zip(distancesEnemyToOwnHQ)
      var aggressionRating = 0

      for (x <- combined) {
        if(x._1 > x._2) {
          aggressionRating += -1
        }

        else {
          aggressionRating += 1
        }
      }

      if(aggressionRating > 0) {
        intel += ("enemyStrategy" -> 1)
      }
      else {
        intel += ("enemyStrategy" -> 0)
      }


      intel += ("enemyDistanceToOwnHQ" -> distancesEnemyToOwnHQ.min)
      intel += ("enemyDistanceToEnemyHQ" -> distancesEnemyToEnemyHQ.min)
    }
    else {
      intel += ("enemyDistanceToOwnHQ" -> 100)
      intel += ("enemyDistanceToEnemyHQ" -> 100)
      intel += ("enemyStrategy" -> 0)
    }

    // Distance of own troops



    if (Troops.nonEmpty) {

      var distancesOwnToOwnHQ = mutable.Buffer[Int]()
      var distancesOwnToEnemyHQ = mutable.Buffer[Int]()

      for (x <- Troops) {
      var distance = HQ.get.location.determineTileDistance(x.current_location.get)
      distancesOwnToOwnHQ += distance
      ownTotalStrength += x.currentHP

    }

      for (x <- Troops) {
      var distance = enemy.HQ.get.location.determineTileDistance(x.current_location.get)
      distancesOwnToEnemyHQ += distance

    }
      intel += ("ownDistanceToOwnHQ" -> distancesOwnToOwnHQ.min)
      intel += ("ownDistanceToEnemyHQ" -> distancesOwnToEnemyHQ.min)
    }
    else {
      intel += ("ownDistanceToOwnHQ" -> 100)
      intel += ("ownDistanceToEnemyHQ" -> 100)
    }

    intel += ("ownStrength" -> ownTotalStrength)
    intel += ("enemyStrength" -> enemyTotalStrength)


    intel

  }


  def determineStrategy() = {

    var ownHQinDanger = enemyIntel()("enemyDistanceToOwnHQ") < enemyIntel()("ownDistanceToOwnHQ")
    var enemyHQinDanger = enemyIntel()("ownDistanceToEnemyHQ") < enemyIntel()("enemyDistanceToEnemyHQ")

    // Critical Situations
    if(ownHQinDanger || enemyHQinDanger) {
      if(ownHQinDanger && enemyHQinDanger) {
        if(enemyIntel()("enemyDistanceToOwnHQ") > enemyIntel()("ownDistanceToEnemyHQ")) {
          "Conquer"
        }
        else {
          "Turtle"
        }
      }
      else {
        if(ownHQinDanger) {
          "Turtle"
        }
        else {
          "Conquer"
        }
      }


    }
    // Build up gameplay
    else {
      if(enemyIntel()("enemyStrategy") == 0) {
        "Attack"
      }
      else {
        if(enemyIntel()("ownStrength") > enemyIntel()("enemyStrength")) {
          "Attack"
        }
        else {
          "Defend"
        }
      }
    }

  }

  def roleGiver() = {
   HQ.get.location.determineDirection(enemy.HQ.get.location) match {
     case "S" => troopRoles.appendAll(Array("S", "SE","SW"))
     case "SW" => troopRoles.appendAll(Array("SW", "NW","S"))
     case "NW" => troopRoles.appendAll(Array("NW", "SW","N"))
     case "N" => troopRoles.appendAll(Array("N", "NW","NE"))
     case "NE" => troopRoles.appendAll(Array("NE", "N","SE"))
     case "SE" => troopRoles.appendAll(Array("SE", "NE","S"))
   }
  }

  def buildTroops(strategy: String) = {
     strategy match {
       case "attack" => {
        var deploymentTile = HQ.get.location.neighbors(HQ.get.location.determineDirection(enemy.HQ.get.location)).neighbors(HQ.get.location.determineDirection(enemy.HQ.get.location))
        if(deploymentTile.tileUnoccupied() && Troops.size < 3) {
          deployTroop(deploymentTile)

          for (x <- Troops) {
            if (x.stepsTaken < HQ.get.location.determineTileDistance(enemy.HQ.get.location)/8)
            x.move(x.current_location.get.neighbors(x.AIDesignatedRole))
          }
        }

        else {
          if((deploymentTile.hasUnit.isDefined) && deploymentTile.hasUnit.get.player == this) {
            var troop = deploymentTile.hasUnit.get
            troop.move(troop.current_location.get.neighbors(troop.AIDesignatedRole))
          }
        }
       }
       case "defend" => {
        var deploymentTile = HQ.get.location.neighbors(HQ.get.location.determineDirection(enemy.HQ.get.location)).neighbors(HQ.get.location.determineDirection(enemy.HQ.get.location))
        if(deploymentTile.tileUnoccupied() && Troops.size < 3) {
          deployTroop(deploymentTile)

          for (x <- Troops) {
            if (x.stepsTaken < HQ.get.location.determineTileDistance(enemy.HQ.get.location)/8)
            x.move(x.current_location.get.neighbors(x.AIDesignatedRole))
          }
        }

        else {
          if((deploymentTile.hasUnit.isDefined) && deploymentTile.hasUnit.get.player == this) {
            var troop = deploymentTile.hasUnit.get
            troop.move(troop.current_location.get.neighbors(troop.AIDesignatedRole))
          }
        }
       }
       case "turtle" => {
        var deploymentTile = HQ.get.location.neighbors(HQ.get.location.determineDirection(enemy.HQ.get.location)).neighbors(HQ.get.location.determineDirection(enemy.HQ.get.location))
        if(deploymentTile.tileUnoccupied() && Troops.size < 3) {
          deployTroop(deploymentTile)

          for (x <- Troops) {
            if (x.stepsTaken < HQ.get.location.determineTileDistance(enemy.HQ.get.location)/8)
            x.move(x.current_location.get.neighbors(x.AIDesignatedRole))
          }
        }

        else {
          if((deploymentTile.hasUnit.isDefined) && deploymentTile.hasUnit.get.player == this) {
            var troop = deploymentTile.hasUnit.get
            troop.move(troop.current_location.get.neighbors(troop.AIDesignatedRole))
          }
        }
       }
     }
  }

  def attack(yourTroop: Swordsman, enemyTroop: Swordsman) = {
    var failsafe = 4
    while((!yourTroop.isDead && !enemyTroop.isDead) && failsafe > 0) {
      yourTroop.move(yourTroop.current_location.get.neighbors(yourTroop.current_location.get.determineDirection(enemyTroop.current_location.get)))
      failsafe += -1
    }
  }

  def attackStrategy() = {
    var troopSituation = Troops.size - enemy.Troops.size
    var troopCount = Troops.size
    var targets = mutable.Buffer[(Swordsman, Swordsman)]()

    // Determines targets for each of the AIs troops
    if(((Troops.nonEmpty && enemy.Troops.nonEmpty) && troopSituation >= 0) || Troops.size == 3) {

      for (x <- Troops) {
        var target: Option[(Swordsman, Int)] = None

        for(y <- enemy.Troops) {

        x.current_location.get.determineTileDistance(y.current_location.get)
        if(target.isDefined) {
          if(target.get._2 > x.current_location.get.determineTileDistance(y.current_location.get)) {
            target = Some(y, x.current_location.get.determineTileDistance(y.current_location.get))
          }
          else {

          }
        }
        else {
          target = Some(y, x.current_location.get.determineTileDistance(y.current_location.get))
        }

        }
        var holder = (x, target.get._1)
        targets += holder

      }

      // the attack method should reduce energy but in case the attack method would fail to move any troops
      // the failsafe will trigger and prevent an infinite loop

      var failsafe = 10

      while (energy > 0 && failsafe > 0) {
      for (x <- targets) {
        attack(x._1, x._2)
      }
       failsafe += -1

      }

    }

    else {
       buildTroops("attack")
    }

    game.GUI.updateGui()


  }

  def defenceStrategy() = {
   var troopSituation = Troops.size - enemy.Troops.size
    var troopCount = Troops.size
    var targets = mutable.Buffer[(Swordsman, Swordsman)]()

    // Determines targets for each of the AIs troops
    if(((Troops.nonEmpty && enemy.Troops.nonEmpty) && troopSituation >= 0) || Troops.size == 3) {

      for (x <- Troops) {
        var target: Option[(Swordsman, Int)] = None

        for(y <- enemy.Troops) {

        x.current_location.get.determineTileDistance(y.current_location.get)
        if(target.isDefined) {
          if(target.get._2 > x.current_location.get.determineTileDistance(y.current_location.get)) {
            target = Some(y, x.current_location.get.determineTileDistance(y.current_location.get))
          }
          else {

          }
        }
        else {
          target = Some(y, x.current_location.get.determineTileDistance(y.current_location.get))
        }

        }
        var holder = (x, target.get._1)
        targets += holder

      }

      // the attack method should reduce energy but in case the attack method would fail to move any troops
      // the failsafe will trigger and prevent an infinite loop

      var failsafe = 10

      while (energy > 0 && failsafe > 0) {
      for (x <- targets) {
        attack(x._1, x._2)
      }
       failsafe += -1

      }

    }

    else {
       buildTroops("defend")
    }
    game.GUI.updateGui()
  }

  def turtleStrategy() = {
    buildTroops("turtle")
    game.GUI.updateGui()
  }

  def conquerStrategy() = {

    if(Troops.nonEmpty) {

      var closestTroop: Option[(Swordsman, Int)] = None

      for (x <- Troops) {

        var distanceToHQ = x.current_location.get.determineTileDistance(enemy.HQ.get.location)

        if(closestTroop.isEmpty) {
          closestTroop = Some(x, distanceToHQ)
        }

        if(closestTroop.isDefined) {
          if(closestTroop.get._2 > distanceToHQ) {
            closestTroop = Some(x, distanceToHQ)
          }
        }
      }

     var conquerTroop = closestTroop.get._1
     var failsafe = 10

     while ((energy > 0 && failsafe  > 0) && capturedHQ.isEmpty) {
        conquerTroop.move(conquerTroop.current_location.get.neighbors(conquerTroop.current_location.get.determineDirection(enemy.HQ.get.location)))
        failsafe += -1
     }

    }

    game.GUI.updateGui()
  }

  def act() = {
    if(game.turnCount == 1) {
      var createHQ = new HQ(this, game.gameTiles((5,4)))
      HQ = Some(createHQ)
      roleGiver()

    }

    else {
      determineStrategy() match {
        case "Attack" => attackStrategy()
        case "Defend" => defenceStrategy()
        case "Conquer" => conquerStrategy()
        case "Turtle" => turtleStrategy()
      }
    }

    endTurn()
    game.updateGUI()
  }
}
