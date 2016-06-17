import java.awt.Color
import java.awt.Color._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import Types._

class Output (configuration: Engine) {
  import configuration._

  def drawPuzzle (puzzle: Puzzle, file: String) = {
    val canvas = new BufferedImage (cellPixels * size + 1, cellPixels * size + 1, BufferedImage.TYPE_INT_RGB)
    val graphics = canvas.createGraphics
    import graphics.{dispose, setColor, fillRect, drawLine}

    def line (x1: Int, y1: Int, width: Int, height: Int, colour: Color) = {
      setColor (colour)
      drawLine (x1, y1, x1 + width, y1 + height)
    }

    setColor (WHITE)
    fillRect (0, 0, canvas.getWidth, canvas.getHeight)
    for (row <- axis; column <- axis) {
      line (column * cellPixels, row * cellPixels, cellPixels, 0,
        if (row == 0 || puzzle.regionAt (row, column) != puzzle.regionAt (row - 1, column)) BLACK else LIGHT_GRAY)
      line (column * cellPixels, row * cellPixels, 0, cellPixels,
        if (column == 0 || puzzle.regionAt (row, column) != puzzle.regionAt (row, column - 1)) BLACK else LIGHT_GRAY)
    }
    line (0, size * cellPixels, size * cellPixels, 0, BLACK)
    line (size * cellPixels, 0, 0, size * cellPixels, BLACK)

    dispose ()
    ImageIO.write (canvas, "png", new File (file))
  }

  def drawSolution (puzzle: Puzzle, partial: Partial, file: String) = {
    val canvas = new BufferedImage (cellPixels * size + 1, cellPixels * size + 1, BufferedImage.TYPE_INT_RGB)
    val graphics = canvas.createGraphics
    import graphics.{dispose, setColor, fillRect, drawLine}

    def line (x1: Int, y1: Int, width: Int, height: Int, colour: Color) = {
      setColor (colour)
      drawLine (x1, y1, x1 + width, y1 + height)
    }
    def star (cx: Int, cy: Int, width: Int) = {
      setColor (BLACK)
      drawLine (cx - width / 2, cy, cx + width / 2, cy)
      drawLine (cx - width / 2, cy - width / 2, cx + width / 2, cy + width / 2)
      drawLine (cx, cy - width / 2,  cx, cy + width / 2)
      drawLine (cx - width / 2, cy + width / 2, cx + width / 2, cy - width / 2)
    }

    setColor (WHITE)
    fillRect (0, 0, canvas.getWidth, canvas.getHeight)
    for (row <- axis; column <- axis) {
      line (column * cellPixels, row * cellPixels, cellPixels, 0,
        if (row == 0 || puzzle.regionAt (row, column) != puzzle.regionAt (row - 1, column)) BLACK else LIGHT_GRAY)
      line (column * cellPixels, row * cellPixels, 0, cellPixels,
        if (column == 0 || puzzle.regionAt (row, column) != puzzle.regionAt (row, column - 1)) BLACK else LIGHT_GRAY)
      if (partial (row)(column) == StarCell)
        star (column * cellPixels + cellPixels / 2, row * cellPixels + cellPixels / 2, cellPixels - 4)
    }
    line (0, size * cellPixels, size * cellPixels, 0, BLACK)
    line (size * cellPixels, 0, 0, size * cellPixels, BLACK)

    dispose ()
    ImageIO.write (canvas, "png", new File (file))
  }

  def puzzleToString (puzzle: Puzzle) = {
    def edge (row: Int, column: Int, dRow: Int, dColumn: Int): String =
      if (row + dRow < 0 || column + dColumn < 0 || puzzle.regionAt (row, column) != puzzle.regionAt (row + dRow, column + dColumn))
        "#" else if (dRow == 0) "|" else "-"
    (for (row <- axis) yield {
      val twoRows = (for (column <- axis) yield
        ("#" + edge (row, column, -1, 0), edge (row, column, 0, -1) + " ")).
        reduce ((x, y) => (x._1 + y._1, x._2 + y._2))
      twoRows._1 + "#\n" + twoRows._2 + "#"
    }).mkString ("\n") + "\n" + Seq.fill (size * 2 + 1)("#").mkString
  }
}
