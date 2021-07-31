package mmb

import scala.collection.mutable.Buffer
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication

/**
 * An application that opens up a simple window.
 *
 * @param title a window title
 * @param width the window width in pixels
 * @param height the window height in pixels
 */
class SimpleWindow(title: String, val width: Int, val height: Int)
  extends SimpleSwingApplication {

  javax.swing.UIManager.setLookAndFeel(
    javax.swing.UIManager.getSystemLookAndFeelClassName())

  val top = new MainFrame {
    this.title = title
    this.preferredSize = new java.awt.Dimension(width, height)
    this.resizable = false
  }
}

/**
 * An application that opens up a simple image window.
 *
 * @param title a window title
 * @param width the window and image width in pixels
 * @param height the window and image height in pixels
 */
class SimpleImageWindow(title: String, width: Int, height: Int)
  extends SimpleWindow(title, width, height) {

  val image = new java.awt.image.BufferedImage(
    width, height, java.awt.image.BufferedImage.TYPE_INT_RGB)

  top.contents = new Label {
    this.preferredSize = top.preferredSize
    this.icon = new javax.swing.ImageIcon(image)
  }

  def makeIntPixel(rgb: (Int, Int, Int)) =
    new java.awt.Color(rgb._1, rgb._2, rgb._3).getRGB

  def makeIntArray(pixels: IndexedSeq[(Int, Int, Int)]) =
    pixels.map(this.makeIntPixel).toArray

  def paintIntArray(pixels: Array[Int]) = {
    this.image.setRGB(0, 0, this.width, this.height, pixels, 0, this.width)
    this.top.repaint()
  }

  /**
   * Sets the image.
   *
   * @param pixels the pixel colors as tuples (red, green, blue) 0-255
   */
  def setImage(pixels: IndexedSeq[(Int, Int, Int)]) = {
    this.paintIntArray(this.makeIntArray(pixels))
  }

  /**
   * Sets a pixel.
   *
   * @param x the pixel x coordinate
   * @param y the pixel y coordinate
   * @param rgb a pixel color tuple (red, green, blue) 0-255
   */
  def setPixel(x: Int, y: Int, rgb: (Int, Int, Int)) = {
    this.image.setRGB(x, y, this.makeIntPixel(rgb))
    this.top.repaint()
  }
}

/**
 * An application that opens up a simple animation window.
 *
 * @param title a window title
 * @param width the window and image width in pixels
 * @param height the window and image height in pixels
 */
class SimpleAnimationWindow(title: String, width: Int, height: Int, frameRate: Int)
  extends SimpleImageWindow(title, width, height) {

  val frames = Buffer[Array[Int]]()
  var currentFrame = 0

  /**
   * A repeatedly called action to render the animation.
   */
  val renderLoop = new javax.swing.AbstractAction() {
    def actionPerformed(e: java.awt.event.ActionEvent) = {
      if (frames.size > 0) {
        paintIntArray(frames(currentFrame))
        currentFrame = if (currentFrame < frames.size - 1) currentFrame + 1 else 0
      }
    }
  }
  val timer = new javax.swing.Timer(1000 / frameRate, this.renderLoop)
  timer.setRepeats(true)

  /**
   * Adds a new frame to the animation.
   *
   * @param pixels the pixel values as tuples (red, green, blue) 0-255
   */
  def addFrame(pixels: IndexedSeq[(Int, Int, Int)]) = {
    frames += this.makeIntArray(pixels)
  }

  /**
   * Starts animation.
   */
  def start() = {
    this.timer.start()
  }

  /**
   * Stops animation.
   */
  def stop() = {
    this.timer.stop()
  }
}