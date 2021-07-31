package mmb

object Run extends App {

  // Window dimension and animation frame rate.
  val width = 400
  val height = 300
  val fps = 20

  // Some interesting targets.
  val mandelbrotAntennas = (-0.1638, 1.0353)
  val seahorseValley = (-0.746, -0.1)
  val elephantValley = (0.2855, 0.012)

  // Animation parameters.
  val target = seahorseValley
  val steps = 100
  val x = new Easing(-0.5, target._1, steps)
  val y = new Easing(0.0, target._2, steps)
  val w = new Easing(3, 0.0005, steps)

  // Create window.
  val win = new SimpleAnimationWindow("Mini Mandelbrot", width, height, fps)
  win.main(args)

  // Calculate fractal image frames.
  val mandelbrot = new MandelbrotSet(width, height, 1000)
  for (i <- 0 until steps) {
    val frame = mandelbrot.calculate((x(i), y(i)), w(i))
    win.setImage(frame)
    win.addFrame(frame)
  }
  win.start()
}