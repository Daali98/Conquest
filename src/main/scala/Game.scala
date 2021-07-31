import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.control.Button
import scalafx.scene.{Node, Scene, shape}
import scalafx.scene.shape.Polyline

import scala.math.sqrt
import scala.collection.mutable
import scala.util.Random

class Game (xDir: Int, yDir: Int, size: Int, gui: Main.type){

  // Hexagon geometry

  var startX = size/2
  var startY = size/4*sqrt(3)
  var startXOdd = size + size/4-1
  var startYOdd = startY*2

  var gameTiles = Map[(Int, Int), Tile]()
  var guiTiles = mutable.Buffer[Polyline]()
  var players = Map[String, Player]()


  var activeTile: Option[Tile] = None
  var activePlayer: Option[Player] = None
  var turnCount = 0

  var GUI = gui

  // Attacking randomizer engine

  var randomizerNumber = 0
  var randomizerEngine = new Random

  def randomizer(toRandom: Int) = {
    var randomNumbers = LazyList.continually(randomizerEngine.nextInt(toRandom))
    randomNumbers(randomizerNumber)
  }

  // -||-


   def updateGUI() = {
    for (x <- gameTiles.values) {
        if (x.active) {
          x.active = false
          activeTile = None
        }
        if(x.highlighted) {
          x.highlighted = false
        }

        x.update()
        }
  }

 def addTile(xLoc: Double, yLoc: Double, IDX: Int, IDY: Int, game: Game) = {
   var tile = new Tile(xLoc, yLoc, size, IDX, IDY, this)
   tile.setViewOrder(0.0)
   gameTiles += ((IDX, IDY) -> tile)
   guiTiles += tile.hexagonTile
   gui.tileMap += ((IDX, IDY, "Tile") -> tile.hexagonTile)

 }


  def fillBoard(xDir: Int, yDir: Int) = {
    var holderX = 0
    var holderY = 0
    var IDX = 0
    var IDY = 0

    val stepSizeX = size+size/2-4
    val stepSizeY = size/2*sqrt(3)-1

    while (holderX < xDir+1) {
      addTile(startX+stepSizeX*holderX, startY, IDX, IDY, this)
      holderY += 1
      IDY += 1



      while (holderY < yDir+1) {

          addTile(startX+stepSizeX*holderX, startY+stepSizeY*holderY, IDX, IDY, this)
          holderY += 1
          IDY += 1




    }
      holderY = 0
      holderX += 1
      IDX += 2
      IDY = 0
  }
    holderX = 0
    IDX = 1

    while (holderX < xDir+1) {
      addTile(startXOdd+stepSizeX*holderX, startYOdd, IDX, IDY, this)
      holderY += 1
      IDY += 1

      while (holderY < yDir+1) {

          addTile(startXOdd+stepSizeX*holderX, startYOdd+stepSizeY*holderY, IDX, IDY, this)
          holderY += 1
          IDY += 1



    }
      holderY = 0
      holderX += 1
      IDX += 2
      IDY = 0
    }
  }
// Initialize
  fillBoard(xDir, yDir)
  gameTiles.values.foreach{
    _.fillNeighbors
  }



}
