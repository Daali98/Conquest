import scalafx.Includes._
import scalafx.scene.control.Button
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color._
import scalafx.scene.shape._

import scala.math._
import scala.collection.mutable


class Tile (val x: Double, val y: Double, D: Int, IDX: Int, IDY: Int, game: Game) extends Polyline {


  var a = D/4
  var B = sqrt(3) * a


  var active = false
  var highlighted = false
  var hasBuilding: Option[Building] = None
  var hasUnit: Option[Swordsman] = None
  var neighbors = Map[String, Tile]()
  var idx = IDX
  var idy = IDY


  def determineDirection(otherTile: Tile): String = {
    if(this.y - otherTile.y < 0) {
      //SW, S, SE

      if(this.x == otherTile.x) {
        return ("S")
      }

      if(this.x > otherTile.x) {
        if(atan2((otherTile.y - this.y), (this.x - otherTile.x)).toDegrees < 60) {
          return ("SW")
        }
        else {
          return ("S")
        }
      }

      else {
        if(atan2((otherTile.y - this.y), (otherTile.x - this.x)).toDegrees < 60) {
          return ("SE")
        }
        else {
          return ("S")
        }
      }

    }


    else {
       if(this.x == otherTile.x) {
        return ("N")
      }

      if(this.x > otherTile.x) {
        if(atan2(abs(otherTile.y - this.y), abs(this.x - otherTile.x)).toDegrees < 60) {
          return ("NW")
        }
        else {
          return ("N")
        }
      }

      else{
        if(atan2(abs(otherTile.y - this.y), abs(otherTile.x - this.x)).toDegrees < 60) {
          return ("NE")
        }
        else {
          return ("N")
        }
      }
    }
  }

  def determineTileDistance(otherTile: Tile) = {
    var endTile = otherTile
    var startTile = this
    var stepTile = this
    var distanceCounter = 0

    while(stepTile != endTile) {
      stepTile = stepTile.neighbors(stepTile.determineDirection(endTile))
      distanceCounter += 1
    }

    distanceCounter
  }

  def tileUnoccupied() = {
    this.hasBuilding.isEmpty && this.hasUnit.isEmpty
  }


  def distanceToTile(otherTile: Tile): Double = {
    sqrt(pow(this.x - otherTile.x, 2) + pow(this.y - otherTile.y, 2))

  }

  def addTroop(someTroop: Swordsman) = {
    hasUnit = Some(someTroop)
    this.update()
  }


  def addBuilding(someBuilding: Building) = {
    hasBuilding = Some(someBuilding)
    for (x <- neighbors.values) {
      x.hasBuilding = Some(someBuilding)
      x.update()
    }
    update()
  }

  def fillNeighbors() = {
    for (x <- game.gameTiles.keys) {
      if((distanceToTile(game.gameTiles(x)) > 20) && (distanceToTile(game.gameTiles(x)) < 35)) {
        neighbors += determineDirection(game.gameTiles(x)) -> game.gameTiles(x)
      }
    }
  }


  def highlight(active: Boolean) = {
    if(active) {
      hexagonTile.setFill(Gray)
    }
    else {
      hexagonTile.setFill(Blue)

    }
  }

  def removeDeadTroop() = {
    if(hasUnit.isDefined && hasUnit.get.isDead) {
      hasUnit = None
    }
  }

  def update() = {

    removeDeadTroop()
    if(active) {
      hexagonTile.setFill(DarkGreen)
    }
    if(highlighted) {
      hexagonTile.setFill(LightGreen)
    }
    if(!active && !highlighted && tileUnoccupied()) {
      hexagonTile.setFill(White)
    }

    if(hasBuilding.isDefined) {
       if(hasBuilding.get.Player.id == "User") {
        hexagonTile.setFill(DarkBlue)
      }
      else {
        hexagonTile.setFill(DarkRed)
      }
    }

    if(hasUnit.isDefined) {
      if(hasUnit.get.player.id == "User") {
        hexagonTile.setFill(Blue)
      }
      else {
        hexagonTile.setFill(Red)
      }
    }




  }

  def returnToDefaultState() = {
    for (x <- game.gameTiles.values) {
        if (x.active) {
          x.active = false
          game.activeTile = None
        }
        if(x.highlighted) {
          x.highlighted = false
        }

        x.update()
        }
  }


  var hexagonTile = Polyline(x-a, B+y, a+x, B+y, 2*a+x, y, x+a, y-B, x-a, y-B, x-2*a, y, x-a, B+y)
  hexagonTile.setFill(White)
  hexagonTile.setStroke(Blue)
  var clicks = 2


  hexagonTile.onMouseEntered = (e:MouseEvent) => {

    hexagonTile.setFill(Gray)



    if(game.activePlayer.get.selectedTroop.isDefined && this.highlighted) {
      hexagonTile.onMouseClicked = (e:MouseEvent) => {

        game.activePlayer.get.selectedTroop.get.move(this)
        returnToDefaultState()



    }
    }
    else {

    hexagonTile.onMouseClicked = (e:MouseEvent) => {
    // Update GUI for all tiles
      returnToDefaultState()

      active = true
      game.activeTile = Some(this)

      game.GUI.updateGui()

      if(hasUnit.isDefined) {

        hasUnit.get.player.selectedTroop = hasUnit

      }

      clicks += 1

      update()
      for (x <- neighbors.values) {
        x.highlighted = true
        x.update()
      }


   }
   }


    hexagonTile.onMouseExited = (e:MouseEvent) => {
        this.update()

  }


    }


  }





