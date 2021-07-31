import scalafx.application.JFXApp
import scalafx.scene.{Node, Scene, shape}
import scalafx.scene.layout._
import scalafx.Includes._
import scalafx.geometry.Pos.BaselineCenter
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Circle, Polygon, Polyline, Rectangle}
import scalafx.scene.control.{Button, Label, ListView, ScrollPane, SplitPane, ToolBar}
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Font
import scala.math.sqrt


object Main extends JFXApp{
  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Stage"
    width = 1920
    height = 1080


}
  val tileX = 28
  val tileY = 35
  val tileSize = 35

  val root = new Pane //Simple pane component

  val scene = new Scene(root) {

  }//Scene acts as a container for the scene graph
  stage.scene = scene

  // ScoreBox
  val scoreBox = new VBox()

  scoreBox.setPrefWidth(515)
  scoreBox.setPrefHeight(105)
  scoreBox.setBackground(new Background(Array(new BackgroundFill((White), CornerRadii.Empty, Insets.Empty))))
  scoreBox.layoutX = 1405
  scoreBox.layoutY = 0
  scoreBox.setAlignment(Pos.Center)

  var title = addLabel("CONQUEST")

  title.setFont(new Font(30.0))
  scoreBox.children.add(title)

  // ScoreBox


  // EventBox Structure
  val eventBox = new VBox

  var eventLog = new ScrollPane
  var eventText = new VBox



  eventLog.content = eventText


  eventBox.layoutX = 1405
  eventBox.layoutY = 105
  eventBox.setPrefWidth(515)
  eventBox.setPrefHeight(480)
  eventBox.setBackground(new Background(Array(new BackgroundFill((Green), CornerRadii.Empty, Insets.Empty))))
  eventBox.children.add(eventLog)

  eventLog.layoutX = 1405
  eventLog.layoutY = 105
  eventLog.setPrefWidth(515)
  eventLog.setPrefHeight(480)


  var D = 15
  var a = D/4
  var B = sqrt(3) * a

  def eventLabel(event: String) = {
    var container = new VBox()
    var playerIndicator = Polyline(-a, B, a, B, 2*a, 0, a, -B, -a, -B, -2*a, 0, -a, B)
    playerIndicator.setFill(

      if(peli.activePlayer.get.id == "User") {
        Blue
      }
      else {
        Red
      }
    )

    container.children.add(playerIndicator)
    container.children.add(new Label(event))
    eventText.children.insert(0, container)

  }

  // EventBox Structure


  val actionBox = new VBox()

  actionBox.setPrefWidth(515)
  actionBox.setPrefHeight(350)
  actionBox.layoutX = 1405
  actionBox.layoutY = 585
  actionBox.setStyle("-fx-padding: 10;" +
                  "-fx-border-style: solid inside;" +
                  "-fx-border-width: 2;" +
                  "-fx-border-insets: 5;" +
                  "-fx-border-radius: 5;" +
                  "-fx-border-color: blue;")



  var actionBoxControl = Map[String, Button]()
  var actionBoxInfo = new VBox()

  actionBoxInfo.setPrefWidth(515)
  actionBoxInfo.setPrefHeight(300)
  actionBoxInfo.layoutY = 585
  actionBoxInfo.setBackground(new Background(Array(new BackgroundFill((White), CornerRadii.Empty, Insets.Empty))))
  actionBoxInfo.setStyle("-fx-padding: 10;" +
                  "-fx-border-style: solid inside;" +
                  "-fx-border-width: 2;" +
                  "-fx-border-insets: 5;" +
                  "-fx-border-radius: 5;" +
                  "-fx-border-color: Green;")

  var actionBoxControlBox = new VBox()


    actionBoxControlBox.setAlignment(BaselineCenter)





  actionBox.children.add(actionBoxInfo)

  def updateActionBox() = {
    actionBox.children.clear()
    actionBoxControlBox.children.clear()
    actionBox.children.add(actionBoxInfo)
    for (x <- actionBoxControl.values) {
      actionBoxControlBox.children.add(x)
    }
    actionBox.children.add(actionBoxControlBox)
  }

  val endTurnBox = new VBox()

  endTurnBox.setPrefWidth(515)
  endTurnBox.setPrefHeight(120)
  endTurnBox.setBackground(new Background(Array(new BackgroundFill((Blue), CornerRadii.Empty, Insets.Empty))))
  endTurnBox.layoutX = 1405
  endTurnBox.layoutY = 935
  endTurnBox.setAlignment(Pos.Center)


  //End scenes
  val winScene = new Scene

  val winsceneVBox = new VBox()
  winsceneVBox.setPrefWidth(1920)
  winsceneVBox.setPrefHeight(1080)
  winsceneVBox.setAlignment(Pos.Center)
  val winLabel = new Label("You win!")
  winLabel.setFont(new Font(45.0))

  winsceneVBox.children.add(winLabel)

  winScene.content.add(winsceneVBox)

   val loseScene = new Scene

  val losesceneVBox = new VBox()
  losesceneVBox.setPrefWidth(1920)
  losesceneVBox.setPrefHeight(1080)
  losesceneVBox.setAlignment(Pos.Center)
  val loseLabel = new Label("You lose!")
  loseLabel.setFont(new Font(45.0))

  losesceneVBox.children.add(loseLabel)

  loseScene.content.add(losesceneVBox)

  var deployTroopButton = new Button("Deploy Troop")

  deployTroopButton.onAction = (event) => {
    if(peli.activeTile.isDefined) {
      if(peli.activePlayer.get.deployTroop(peli.activeTile.get)) {

        var troop = new Swordsman(peli.activePlayer.get, peli.activeTile.get)
        peli.activeTile.get.addTroop(troop)
        peli.activePlayer.get.Troops = peli.activePlayer.get.Troops.appended(troop)
        println("Success")
        println(peli.players(peli.activePlayer.get.id).HQ.get.location.determineTileDistance(peli.activeTile.get))
      }

    }
    else {
      println("No tile selected!")
    }
  }

  var deployHQButton = new Button("Deploy HQ")

  deployHQButton.onAction = (event) => {

    if(peli.activeTile.isDefined) {

      var HQ = new HQ(peli.activePlayer.get, peli.activeTile.get)
      peli.activeTile.get.addBuilding(HQ)
      peli.players(peli.activePlayer.get.id).HQ = Some(HQ)
      actionBoxControl -= "deployHQButton"

    }
    else {
      println("No tile selected!")
    }

    updateActionBox()
      updateGui()
  }

  var endTurnButton = new Button("End Turn")

  endTurnButton.onAction = (event) => {
    peli.activePlayer.get.endTurn()

  }

  def endGame(player: Player) = {
    if(player.id == "User") {
      stage.scene = winScene
    }

    if(player.id == "AI") {
      stage.scene = loseScene
    }
  }


  actionBoxControl += "deployHQButton" -> deployHQButton
  updateActionBox()
  endTurnBox.children.add(endTurnButton)


  var tileMap = Map[(Int, Int, String), Node]()

  val peli = new Game(tileX, tileY, tileSize, this)

  var user = new User("Daniel", peli)
  peli.players += (user.id -> user)

  var AI = new AI("AI", peli)
  peli.players += (AI.id -> AI)



  peli.activePlayer = Some(user)

  scene.content = tileMap.values
  scene.content += actionBox
  scene.content += eventBox
  scene.content += scoreBox
  scene.content += endTurnBox



  def addLabel(text: String) = {
    new Label(text)
  }

  def energyLabel = if(peli.activePlayer.isDefined) {
    addLabel("Energy: " + peli.activePlayer.get.energy)
  }
  else {
    addLabel("Failed")
  }

  def troopInfoLabel() = {
    if (peli.activeTile.isDefined) {
      if(peli.activeTile.get.hasUnit.isDefined) {
        var troop = peli.activeTile.get.hasUnit.get
        addLabel("Troop HP: " + troop.currentHP + "/" + troop.max_health)
      }
      else {
        addLabel("No Troop info")
      }
    }
    else {
      addLabel("No Troop info")
    }
  }



  var energyHbox = new HBox()
  energyHbox.children.add(energyLabel)
  energyHbox.setAlignment(Pos.BaselineCenter)
  energyHbox.setStyle("-fx-padding: 10;" +
                  "-fx-border-style: solid inside;" +
                  "-fx-border-width: 2;" +
                  "-fx-border-insets: 5;" +
                  "-fx-border-radius: 5;" +
                  "-fx-border-color: blue;")

  var troopInfoBox = new HBox()
  troopInfoBox.children.add(troopInfoLabel())
  troopInfoBox.setAlignment(Pos.BaselineCenter)
  troopInfoBox.setStyle("-fx-padding: 10;" +
                  "-fx-border-style: solid inside;" +
                  "-fx-border-width: 2;" +
                  "-fx-border-insets: 5;" +
                  "-fx-border-radius: 5;" +
                  "-fx-border-color: blue;")



  actionBoxInfo.children.add(energyHbox)
  actionBoxInfo.children.add(troopInfoBox)

  def updateGui() = {
    scene.content = tileMap.values
    scene.content += actionBox
    scene.content += eventBox
    scene.content += scoreBox
    scene.content += endTurnBox
    if(peli.players("User").HQ.isDefined) {
      actionBoxControl += "deployTroopButton" -> deployTroopButton
      updateActionBox()
    }
    energyHbox.children.remove(0)
    troopInfoBox.children.remove(0)
    energyHbox.children.add(energyLabel)
    troopInfoBox.children.add(troopInfoLabel())
  }



}
