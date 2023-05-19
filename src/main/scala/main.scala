import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BasicStroke, BorderLayout, Color, Dimension, Font, Graphics, GridLayout}
import java.io.{File, PrintWriter}
import java.util.Scanner
import javax.imageio.ImageIO
import javax.swing.*
import javax.swing.JFrame
import scala.io.{Source, StdIn}
import scala.io.StdIn.*
import scala.collection.mutable.Set
import scala.util.Random
import org.jpl7.*



def initCheckersBoard(): Array[Array[String]] ={
  val sHeight: Int = 8
  val sWidth: Int = 8
  val pieceToChar: Map[String, String] = Map(
    "piece" -> "O"
  )
  def doAlternate(i: Int, j: Int, f: () => Unit, g: () => Unit): Unit = {
    i match {
      case n if n % 2 == 0 => {
        if (j % 2 == 0) f()
        else g()
      }
      case _ => {
        if (j % 2 == 0) g()
        else f()
      }
    }
  }
  val grid: Array[Array[String]] = Array.ofDim(sHeight, sWidth)

  for (i <- 0 until 3) {
    for (j <- 0 until sWidth) {
      doAlternate(i, j, () => null, () => grid(i)(j) = "b, " + pieceToChar("piece"))
      doAlternate(sHeight - i - 1, j, () => null, () => grid(sHeight - i - 1)(j) = "w, " + pieceToChar("piece"))

    }
  }
  grid
}
def checkersController(move: String, state:(Array[Array[String]], Boolean)): (Array[Array[String]], Boolean)={
  val sHeight: Int = 8
  val grid: Array[Array[String]] = state(0)
  var whitesTurn: Boolean = state(1)
  def checkNumber(s: String): Boolean = {
    if (s.forall(_.isDigit)) {
      if (1 to 8 contains s.toInt) {
        return true
      }
    }
    false
  }

  def checkPair(s: String): Boolean = {
    if (s.length == 2) {
      if (('a' to 'h' contains s.charAt(0)) && checkNumber(s.charAt(1).toString)) {
        return true
      }
    }
    false
  }

  def validateStringMove(move: String): Boolean = {
    if(move.length != 5)
      return false
    move match {
      case s"$p1,$p2" =>
        checkPair(p1) && checkPair(p2)
      case _ => false
    }
  }

  def validateMove(l1: Int, n1: Int, l2: Int, n2: Int): Boolean = {
    if (grid(n1)(l1) == null) {
      return false
    } else if (grid(n1)(l1).charAt(0) == 'w' && !whitesTurn) {
      return false
    } else if (grid(n1)(l1).charAt(0) == 'b' && whitesTurn) {
      return false
    } else if (grid(n2)(l2) != null) {
      return false
    }


    val iStep = n2 - n1
    val jStep = l2 - l1

    val simpleMove: Boolean = (Math.abs(iStep) == 1 && Math.abs(jStep) == 1)
    val killMove: Boolean = (Math.abs(iStep) == 2 && Math.abs(jStep) == 2)

    if (!(simpleMove || killMove)) {
      return false
    }

    val upgraded = grid(n1)(l1).contains("Q")

    if ((!upgraded && whitesTurn && iStep > 0) ||
      (!upgraded && !whitesTurn && iStep < 0)) {
      return false
    }

    //for kill only
    if (killMove &&
      (grid(n1 + iStep / 2)(l1 + jStep / 2) == null ||
        grid(n1)(l1).charAt(0) == grid(n1 + iStep / 2)(l1 + jStep / 2).charAt(0))
    ) {
      return false
    } else if (killMove) {
      //
      grid(n1 + iStep / 2)(l1 + jStep / 2) = null
    }
    true
  }

  def upgradePiece(piece: String): String = {
    piece match {
      case s"b,$piece" => s"b,Q$piece"
      case s"w,$piece" => s"w,Q$piece"
      case _ => ""
    }
  }

  if (!validateStringMove(move)) {
    return (grid, whitesTurn)
  }

  //extract coordinates
  //<l1><n1>,<l2><n2>
  val l1: Int = move.charAt(0) - 'a'
  val l2: Int = move.charAt(3) - 'a'
  val n1: Int = sHeight - move.charAt(1).toString.toInt
  val n2: Int = sHeight - move.charAt(4).toString.toInt


  if (!validateMove(l1, n1, l2, n2))
    return (grid, whitesTurn)

  //move
  grid(n2)(l2) = grid(n1)(l1)
  grid(n1)(l1) = null

  //upgrading piece
  if (whitesTurn && n2 == 0 || !whitesTurn && n2 == 7) {
    grid(n2)(l2) = upgradePiece(grid(n2)(l2))
  }

  //killing piece
  val iStep = n2 - n1
  val jStep = l2 - l1
  val killMove: Boolean = (Math.abs(iStep) == 2 && Math.abs(jStep) == 2)

  if (killMove) {
    grid(n1 + iStep / 2)(l1 + jStep / 2) = null
  }
  (grid, !whitesTurn)
}
def checkersDrawer(grid: Array[Array[String]]): Unit ={
  new JFrame {
    val frameWidth = 800
    val frameHeight = 600

    this.setPreferredSize(new Dimension(800, 600))
    this.setDefaultCloseOperation(2)
    this.setResizable(false)
    this.setLayout(null)
    this.pack()
    this.setVisible(true)

    class CheckersArea(parentWidth: Int, parentHeight: Int, sWidth: Int, sHeight: Int) extends JPanel {
      val pieceToChar: Map[String, String] = Map(
        "piece" -> "O"
      )

      val PIECE_BLACK: Color = Color.BLACK
      val PIECE_WHITE: Color = Color.decode("#d3d3d3")
      val SQUARE_BLACK: Color = Color.decode("#769655")
      val SQUARE_WHITE: Color = Color.decode("#eeeed2")


      //calculating dimensions of panel
      val areaWidth: Int = sWidth * 50
      val areaHeight: Int = sHeight * 50
      //centering panel
      this.setBounds(
        (parentWidth - areaWidth) / 2,
        (parentHeight - areaHeight) / 2,
        areaWidth,
        areaHeight
      )
      private def drawNumbersOnSquares(g: Graphics): Unit = {
        g.setColor(Color.yellow)
        val fontSize = 12f
        g.setFont(g.getFont.deriveFont(fontSize))
        //    g.drawString(piece, 25+j*50-(fontSize/2).toInt, 25+i*50+(fontSize/3).toInt)
        for (i <- sHeight until 0 by -1) {
          this.doAlternate(2, i, () => g.setColor(SQUARE_BLACK), () => g.setColor(SQUARE_WHITE))

          g.drawString(i.toString, 2, -30 + (sHeight - i + 1) * 50 - (fontSize / 2).toInt)

        }

        for (i <- 1 to sWidth) {
          this.doAlternate(2, i, () => g.setColor(SQUARE_BLACK), () => g.setColor(SQUARE_WHITE))
          g.drawString((('a' - 1) + i).toChar.toString, -3 + (i) * 50 - (fontSize / 2).toInt, 398)
        }
      }
      private def doAlternate(i: Int, j: Int, f: () => Unit, g: () => Unit): Unit = {
        i match {
          case n if n % 2 == 0 => {
            if (j % 2 == 0) f()
            else g()
          }
          case _ => {
            if (j % 2 == 0) g()
            else f()
          }
        }
      }
      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)

        val fontSize = 45f


        g.setFont(g.getFont.deriveFont(fontSize))

        for (i <- 0 until this.sHeight; j <- 0 until this.sWidth) {
          //alternate squares
          doAlternate(i, j, () => g.setColor(SQUARE_WHITE), () => g.setColor(SQUARE_BLACK))
          g.fillRect(i * 50, j * 50, 50, 50)
        }

        for (i <- 0 until this.sHeight; j <- 0 until this.sWidth) {
          //drawing pieces
          grid(i)(j) match {
            case s"b,$piece" =>
              g.setColor(PIECE_BLACK)
              g.fillOval(j * 50, i * 50, 50, 50)
              if(piece.contains("Q")){
                g.setColor(PIECE_WHITE)
                g.drawString("\u265B", 24 + j * 50 - (fontSize / 2).toInt, 25 + i * 50 + (fontSize / 3).toInt)
              }
            case s"w,$piece" =>
              g.setColor(PIECE_WHITE)
              g.fillOval(j * 50, i * 50, 50, 50)
              if (piece.contains("Q")) {
                g.setColor(PIECE_BLACK)
                g.drawString("\u265B", 24 + j * 50 - (fontSize / 2).toInt, 25 + i * 50 + (fontSize / 3).toInt)
              }
            case _ =>
          }
        }
        drawNumbersOnSquares(g)
      }
    }
    this.add(new CheckersArea(frameWidth, frameHeight, 8, 8))

  }
}

def initTicTacToeBoard(): Array[Array[String]] ={
  Array.fill(3, 3)("-")
}
def ticTacToeController(move: String, state:(Array[Array[String]], Boolean)): (Array[Array[String]], Boolean)={
  val grid: Array[Array[String]] = state(0)
  var xMove: Boolean = state(1)
  def checkNumber(s: String): Boolean = {
    if (s.forall(_.isDigit)) {
      if (1 to 3 contains s.toInt) {
        return true
      }
    }
    false
  }


  def validateStringMove(move: String): Boolean = {
    if(move.length != 3)
      return false
    move match {
      case s"$n2,$n1" =>
        checkNumber(n2) && checkNumber(n1)
      case _ => false
    }
  }

  def validateMove(n: Int, l: Int): Boolean = {
    if (!grid(n)(l).equals("-"))
      return false
    true
  }

  if (!validateStringMove(move)) {
    return (grid, xMove)
  }
  val n = move(0).toString.toInt - 1
  val l = move(2).toString.toInt - 1
  if (!validateMove(n, l))
    return (grid, xMove)
  if (xMove)
    grid(n)(l) = "X"
  else
    grid(n)(l) = "O"
  (grid, !xMove)
}
def ticTacToeDrawer(grid: Array[Array[String]]): Unit ={
  new JFrame {
    val frameWidth = 800
    val frameHeight = 600

    this.setPreferredSize(new Dimension(800, 600))
    this.setDefaultCloseOperation(2)
    this.setResizable(false)
    this.setLayout(null)
    this.pack()
    this.setVisible(true)

    class TicTacToeArea(parentWidth: Int, parentHeight: Int, sWidth: Int, sHeight: Int) extends JPanel {

      this.setBackground(Color.decode("#d8f9ff"))
      val offset = 50;
      val areaWidth = sWidth * 100
      val areaHeight = sHeight * 100
      var xMove: Boolean = true
      this.setBounds(
        (parentWidth - areaWidth) / 2 - 50 - offset,
        (parentHeight - areaHeight) / 2 - 50 - offset,
        areaWidth + offset,
        areaHeight + offset
      )

      private def drawNumbersOnSquares(g: Graphics): Unit = {
        g.setColor(Color.black)
        val fontSize = 20f
        g.setFont(g.getFont.deriveFont(fontSize))
        for (i <- sHeight until 0 by -1) {
          g.drawString(i.toString, 20, 30 + 100 * i)
          g.drawString(i.toString, 22 + 100 * i, 43)
        }
      }


      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)

        g.setColor(Color.white)
        g.fillRect(offset, offset, 100 * sWidth, 100 * sHeight)
        val fontSize = 55f
        g.setFont(g.getFont.deriveFont(fontSize))
        g.setColor(Color.BLACK)


        for (i <- 1 to sWidth) {
          g.drawLine(100 * i + offset, 0, 100 * i + offset, offset)
          g.drawLine(0, 100 * i + offset, offset, 100 * i + offset)
          g.fillRect(100 * i - 2 + offset, offset, 4, 100 * sHeight)
          g.fillRect(offset, 100 * i - 2 + offset, 100 * sHeight, 4)
        }
        for (i <- 0 until this.sHeight; j <- 0 until this.sWidth) {
          if (grid(i)(j).equals("X"))
            g.setColor(Color.red)
          else
            g.setColor(Color.blue)
          if (!grid(i)(j).equals("-"))
            g.drawString(grid(i)(j), 50 + j * 100 - (fontSize / 3).toInt + offset, 50 + i * 100 + (fontSize / 3).toInt + offset)
        }
        drawNumbersOnSquares(g)
      }
    }

    this.add(new TicTacToeArea(frameWidth, frameHeight, 3, 3))

  }
}

def initSudokuBoard(): Array[Array[String]] ={
  object SudokuApp extends App {


    // create a possible solution
    val puzzle = new Sudoku(Array.fill(9, 9)(0)).a

    // create a puzzle by remove a number of cells
    remove(puzzle, 60);


    // solve the puzzle

    def remove(a: Array[Array[Int]], count: Int) ={
      val rs = Random.shuffle(List.range(0, 81))
      for (i <- 0 until count)
        a(rs(i) / 9)(rs(i) % 9) = 0
    }
  }

  class Sudoku(val a: Array[Array[Int]]) {
    val r = Array.fill(9)(Set[Int]())
    val c = Array.fill(9)(Set[Int]())
    val z = Array.fill(3, 3)(Set[Int]())

    for (x <- 0 to 8; y <- 0 to 8)
      if (a(x)(y) != 0)
        setExist(a(x)(y), x, y)

    def setExist(v: Int, x: Int, y: Int) ={
      r(x) += v
      c(y) += v
      z(x / 3)(y / 3) += v
    }

    def fill(x: Int, y: Int): Boolean = {
      if (a(x)(y) == 0) {
        val candidates = Set() ++ (1 to 9) -- r(x) -- c(y) -- z(x / 3)(y / 3)

        def current(): Boolean = {
          if (candidates.isEmpty)
            false
          else {
            val v = Random.shuffle(candidates.toList).iterator.next
            candidates -= v
            a(x)(y) = v
            setExist(v, x, y)
            val good = if (y < 8) fill(x, y + 1) else if (x < 8) fill(x + 1, 0) else true
            if (good)
              true
            else {
              a(x)(y) = 0
              r(x) -= v
              c(y) -= v
              z(x / 3)(y / 3) -= v
              current()
            }
          }
        }

        current()
      }
      else if (y < 8) fill(x, y + 1) else if (x < 8) fill(x + 1, 0) else true
    }

    fill(0, 0)
  }

  val grid = Array.fill(9, 9)("0")
  SudokuApp.main(null)
  val puzzle = SudokuApp.puzzle
  for (i <- 0 until puzzle.length; j <- 0 until puzzle(0).length) {
    if(puzzle(i)(j) == 0){
      grid(i)(j) = s"${puzzle(i)(j)}"
    }else
      grid(i)(j) = s"o,${puzzle(i)(j)}"
  }
  grid
}
def sudokuDrawer(grid: Array[Array[String]]): Unit ={
  new JFrame {
    val frameWidth = 800
    val frameHeight = 600

    this.setPreferredSize(new Dimension(800, 600))
    this.setDefaultCloseOperation(2)
    this.setResizable(false)
    this.setLayout(null)
    this.pack()
    this.setVisible(true)

    class SudokuArea(parentWidth: Int, parentHeight: Int, sWidth: Int, sHeight: Int) extends JPanel {

      this.setBackground(Color.decode("#d8f9ff"))
      val offset = 50;
      val areaWidth = sWidth * 50
      val areaHeight = sHeight * 50
      this.setBounds(
        (parentWidth - areaWidth) / 2 - 50 - offset,
        (parentHeight - areaHeight) / 2 - 50 - offset,
        areaWidth + offset,
        areaHeight + offset
      )

      private def drawNumbersOnSquares(g: Graphics): Unit = {
        g.setColor(Color.black)
        val fontSize = 20f
        g.setFont(g.getFont.deriveFont(fontSize))
        for (i <- sHeight until 0 by -1) {
          g.drawString(i.toString, 20, 30 + 50 * (sHeight + 1 - i))
        }

        for (i <- 1 to sWidth) {
          g.drawString((('a' - 1) + i).toChar.toString, 22 + 50 * i, 43)
        }

      }


      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)

        g.setColor(Color.white)
        g.fillRect(offset, offset, 50 * sWidth, 50 * sHeight)
        val fontSize = 45f
        g.setFont(g.getFont.deriveFont(fontSize))
        g.setColor(Color.BLACK)

        for (i <- 1 to sWidth) {
          if (i % 3 == 0) {
            g.fillRect(50 * i - 2 + offset, 0 + offset, 4, 50 * sHeight)
            g.fillRect(0 + offset, 50 * i - 2 + offset, 50 * sHeight, 4)
          }
          g.drawLine(50 * i + offset, 0, 50 * i + offset, sHeight * 50 + offset)
          g.drawLine(0, 50 * i + offset, 50 * sWidth + offset, 50 * i + offset)
        }
        g.fillRect(0 + offset, 0 + offset, 4, 50 * sHeight)
        g.fillRect(0 + offset, 0 + offset, 50 * sWidth, 4)
        g.fillRect(446 + offset, 0 + offset, 4, 50 * sHeight)
        g.fillRect(0 + offset, 446 + offset, 50 * sWidth, 4)
        for (i <- 0 until this.sHeight; j <- 0 until this.sWidth) {
          val element = grid(i)(j)
          element match{
            case s"o,$num"=>
              g.setColor(Color.decode("#4e7ba5"))
              g.drawString(num, 25 + j * 50 - (fontSize / 3).toInt + offset, 25 + i * 50 + (fontSize / 3).toInt + offset)
              g.setColor(Color.BLACK)
            case s"$num"=>
              if (num.toInt != 0)
                g.drawString(num, 25 + j * 50 - (fontSize / 3).toInt + offset, 25 + i * 50 + (fontSize / 3).toInt + offset)
          }
        }
        drawNumbersOnSquares(g)
      }
    }
    this.add(new SudokuArea(frameWidth, frameHeight, 9, 9))

  }
}
def sudokuController(move: String, state:(Array[Array[String]], Boolean)): (Array[Array[String]], Boolean)={
  val grid: Array[Array[String]] = state(0)
  val sWidth: Int = 9
  val sHeight: Int = 9
  def checkCol(n: Int, l: Int, num: Int): Boolean = {
    for (i <- 0 to sHeight - 1; if (i != n)) {
      var numAsString = grid(i)(l)
      if (numAsString.length > 1)
        numAsString = numAsString.charAt(2).toString
      if (numAsString.toInt == num)
        return false
    }
    true
  }
  def solveGrid(oldGrid: Array[Array[String]]): Array[Array[String]] ={
    val fileName = "file.txt"
    val pw = new PrintWriter(new File(fileName))
    pw.write(oldGrid.map(row => {
      row.mkString("[", ",", "]")
    }).mkString("[", ",", "]"))
    pw.write(".")
    pw.close()
    val q1 = new Query("consult('sudokuSolver.pl').")
    if (!q1.hasSolution)
      return null
    val query = new Query(s"read_from_file('$fileName').")
    if (!query.hasSolution)
      return null
    val f = Source.fromFile("sol.txt")
    val sol = parseGrid(f.getLines().next())
    sol
  }
  def convertToPrologForm(oldGrid: Array[Array[String]]): Array[Array[String]] ={
    var newGrid = Array.fill(9, 9)("_")
    for(i <- 0 until 9; j <- 0 until 9){
      oldGrid(i)(j) match{
        case s"o,$num" =>
          newGrid(i)(j) = num
        case "0" =>
          newGrid(i)(j) = "_"
        case _ =>
          newGrid(i)(j) = oldGrid(i)(j)
      }
    }
    newGrid
  }

  def convertToOriginalForm(solvedGrid: Array[Array[String]]): Unit = {
    for (i <- 0 until 9; j <- 0 until 9) {
      grid(i)(j) match {
        case s"o,$num" =>
          grid(i)(j) = s"o,${solvedGrid(i)(j)}"
        case _ =>
          grid(i)(j) = solvedGrid(i)(j)
      }
    }
  }
  def parseGrid(str: String): Array[Array[String]] = {
    val grid = Array.ofDim[String](9, 9)
    var index = 0
    var element = 0
    while (element < str.length) {
      if (str.charAt(element).isDigit) {
        grid(index / 9)(index % 9) = str.charAt(element).toString
        index += 1
      }
      element += 1
    }
    grid
  }
  def checkRow(n: Int, l: Int, num: Int): Boolean = {
    for (i <- 0 to sWidth - 1; if i != l) {
      var numAsString = grid(n)(i)
      if (numAsString.length > 1)
        numAsString = numAsString.charAt(2).toString
      if (numAsString.toInt == num)
        return false
    }
    true
  }

  def checkBox(n: Int, l: Int, num: Int): Boolean = {
      val row = n - n % 3
    val col = l - l % 3
    for (i <- 0 until 3; j <- 0 until 3; if i != n || j != l) {
      var numAsString = grid(row + i)(col + j)
      if(numAsString.length > 1)
        numAsString = numAsString.charAt(2).toString
      if (numAsString.toInt == num)
        return false
    }
    true
  }

  def validateMove(n: Int, l: Int, num: Int): Boolean = {
    if(grid(n)(l).charAt(0) == 'o')
      return false
    checkBox(n, l, num) && checkRow(n, l, num) && checkCol(n, l, num)
  }

  def checkNumber(s: String): Boolean = {
    if (s.forall(_.isDigit)) {
      if (0 to 9 contains s.toInt) {
        return true
      }
    }
    false
  }

  def checkPair(s: String): Boolean = {
    if (s.length == 2) {
      if (('a' to 'i' contains s.charAt(0)) && checkNumber(s.charAt(1).toString)) {
        return true
      }
    }
    false
  }

  def validateStringMove(move: String): Boolean = {
    if(move.length != 4)
    return false
    move match {
      case s"$p1,$p2" =>
        checkPair(p1) && checkNumber(p2)
      case _ => false
    }
  }

  if(move.equalsIgnoreCase("solve")){
    val temp = solveGrid(convertToPrologForm(grid))
    if(temp != null)
      convertToOriginalForm(temp)
      return (grid, !state(1))
  }
  if (!validateStringMove(move)) {
    return (grid, state(1))
  }

  //extract coordinates
  //<l1><n1>,<l2><n2>
  val l: Int = move.charAt(0) - 'a'
  val n: Int = sHeight - move.charAt(1).toString.toInt
  val num: Int = move.charAt(3).toString.toInt
  if (grid(n)(l).charAt(0) == 'o')
    return (grid, state(1))
  if (grid(n)(l).toInt != 0 && num != 0) {
    return (grid, state(1))
  }
  if (!validateMove(n, l, num) && num != 0)
    return (grid, state(1))
  grid(n)(l) = num.toString
  (grid, !state(1))
}



def initEightQueensBoard():Array[Array[String]]={
  Array.ofDim(8, 8)
}
def eightQueensDrawer(grid: Array[Array[String]]): Unit ={
  new JFrame {
    val frameWidth = 800
    val frameHeight = 600

    this.setPreferredSize(new Dimension(800, 600))
    this.setDefaultCloseOperation(2)
    this.setResizable(false)
    this.setLayout(null)
    this.pack()
    this.setVisible(true)

    class EightQueensArea(parentWidth: Int, parentHeight: Int, sWidth: Int, sHeight: Int) extends JPanel {

      val SQUARE_BLACK: Color = Color.decode("#769655")
      val SQUARE_WHITE: Color = Color.decode("#eeeed2")

      val areaWidth: Int = sWidth * 50
      val areaHeight: Int = sHeight * 50

      this.setBounds(
        (parentWidth - areaWidth) / 2,
        (parentHeight - areaHeight) / 2,
        areaWidth,
        areaHeight
      )

      private def doAlternate(i: Int, j: Int, f: () => Unit, g: () => Unit): Unit = {
        i match {
          case n if n % 2 == 0 =>
            if (j % 2 == 0) f()
            else g()
          case _ =>
            if (j % 2 == 0) g()
            else f()
        }
      }

      private def drawNumbersOnSquares(g: Graphics): Unit = {
        g.setColor(Color.yellow)
        val fontSize = 12f
        g.setFont(g.getFont.deriveFont(fontSize))
        //    this.graphics.drawString(piece, 25+j*50-(fontSize/2).toInt, 25+i*50+(fontSize/3).toInt)
        for (i <- sHeight until 0 by -1) {
          this.doAlternate(2, i, () => g.setColor(SQUARE_BLACK), () => g.setColor(SQUARE_WHITE))

          g.drawString(i.toString, 2, -30 + (sHeight - i + 1) * 50 - (fontSize / 2).toInt)

        }

        for (i <- 1 to sWidth) {
          this.doAlternate(2, i, () => g.setColor(SQUARE_BLACK), () => g.setColor(SQUARE_WHITE))
          g.drawString((('a' - 1) + i).toChar.toString, -3 + i * 50 - (fontSize / 2).toInt, 398)
        }

      }


      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)

        val fontSize = 45f


        g.setFont(g.getFont.deriveFont(fontSize))

        for (i <- 0 until this.sHeight; j <- 0 until this.sWidth) {
          //alternate squares
          doAlternate(i, j, () => g.setColor(SQUARE_WHITE), () => g.setColor(SQUARE_BLACK))
          g.fillRect(i * 50, j * 50, 50, 50)
          //this.graphics.fillOval(i*50,j*50,50,50)
        }

        for (i <- 0 until this.sHeight; j <- 0 until this.sWidth) {
          //drawing pieces
          if (grid(i)(j) != null) {
            val image = ImageIO.read(new File("Pieces\\queen.png"))
            g.drawImage(image, j * 50, i * 50, 50, 50, null)
          }
        }
        drawNumbersOnSquares(g)

      }
    }

    this.add(new EightQueensArea(frameWidth, frameHeight, 8, 8))

  }
}
def eightQueensController(move: String, state:(Array[Array[String]], Boolean)): (Array[Array[String]], Boolean) ={
  var grid: Array[Array[String]] = state(0)
  val sWidth = 8
  val sHeight = 8
  val QUEEN = "\u265B"

  def solveGrid(oldGrid: Array[String]): Array[String] ={
    val fileName = "file.txt"
    val pw = new PrintWriter(new File(fileName))
    pw.write(oldGrid.mkString("[", ",", "]"))
    pw.write(".")
    pw.close()
    val consult = new Query("consult('8_Queens.pl').")
    if(!consult.hasSolution)
        return null
    val query = new Query(s"read_from_file('$fileName').")
    if (!query.hasSolution)
      return null
    val f = Source.fromFile("sol.txt")
    val sol = parseGrid(f.getLines().next())
    sol
  }
  def convertToPrologForm(oldGrid: Array[Array[String]]): Array[String] = {
    val newGrid = Array.fill(8)("_")
    for (i <- 0 until 8; j <- 0 until 8) {
      if(oldGrid(i)(j) != null)
        newGrid(i) = (8 - j).toString
    }
    newGrid
  }

  def convertToOriginalForm(solvedGrid: Array[String]): Array[Array[String]] = {
    val newGrid: Array[Array[String]] = Array.fill(8, 8)(null)
    for (i <- 0 until 8) {
      newGrid(i)(8 - solvedGrid(i).toInt) = QUEEN
    }
    newGrid
  }

  def parseGrid(str: String): Array[String] = {
    val grid = Array.ofDim[String](8)
    var index = 0
    var element = 0
    while (element < str.length) {
      if (str.charAt(element).isDigit) {
        grid(index) = str.charAt(element).toString
        index += 1
      }
      element += 1
    }
    grid
  }
  def checkNumber(s: String): Boolean = {
    if (s.forall(_.isDigit)) {
      if (1 to 8 contains s.toInt) {
        return true
      }
    }

    false

  }

  def checkPair(s: String): Boolean = {
    if (s.length == 2) {
      if (('a' to 'h' contains s.charAt(0)) && checkNumber(s.charAt(1).toString)) {
        return true
      }
    }

    false
  }


  def validateStringMove(move: String): Boolean = {
    if (move.length != 2)
      return false
    checkPair(move)
  }

  def validateMove(l: Int, n: Int): Boolean = {
    //vertical check
    for (i <- 0 until sHeight) {
      if (grid(i)(l) != null) return false
    }

    //horizontal check
    for (i <- 0 until sWidth) {
      if (grid(n)(i) != null) return false
    }

    //left diagonal
    for (i <- math.max(l - sWidth, n - sHeight) + 1 to math.min(l, n)) {
      if (grid(n - i)(l - i) != null) return false
    }

    //right diagonal
    for (i <- math.max(-l, n - sHeight + 1) to math.min(n, sWidth - 1 - l)) {
      if (grid(n - i)(l + i) != null) return false
    }

    true
  }

  if(move.equalsIgnoreCase("solve")){
    val solvedGrid = solveGrid(convertToPrologForm(grid))
    if(solvedGrid != null)
      grid = convertToOriginalForm(solvedGrid)
      return (grid, !state(1))
  }
  if (!validateStringMove(move)) {
    return (grid, state(1))
  }

  val l: Int = move.charAt(0) - 'a'
  val n: Int = sHeight - move.charAt(1).toString.toInt

  if (grid(n)(l) != null) {
    grid(n)(l) = null
    return (grid, !state(1))
  }

  if (!validateMove(l, n)) {
    return (grid, state(1))
  }

  grid(n)(l) = QUEEN

  (grid, !state(1))
}

def initConnectFourBoard(): Array[Array[String]] = {
  Array.fill(6, 7)("-")
}
def connectFourDrawer(grid:Array[Array[String]]):Unit={
  new JFrame{
    val frameWidth = 800
    val frameHeight = 600

    this.setPreferredSize(new Dimension(800, 600))
    this.setDefaultCloseOperation(2)
    this.setResizable(false)
    this.setLayout(null)
    this.pack()
    this.setVisible(true)

    class ConnectFourArea(parentWidth: Int, parentHeight: Int, sWidth: Int, sHeight: Int) extends JPanel {

      this.setBackground(Color.decode("#d8f9ff"))
      val offset: Int = 50
      val areaWidth: Int = sWidth * 50
      val areaHeight: Int = sHeight * 50
      this.setBounds(
        (parentWidth - areaWidth) / 2 - 50 - offset,
        (parentHeight - areaHeight) / 2 - 50 - offset,
        areaWidth,
        areaHeight + offset
      )

      private def drawNumbersOnSquares(g: Graphics): Unit = {
        g.setColor(Color.black)
        val fontSize = 20f
        g.setFont(g.getFont.deriveFont(fontSize))
        for (i <- sWidth - 1 to 0 by -1) {
          g.drawString((i + 1).toString, 22 + 50 * i, 43)
        }
      }

      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)

        g.setColor(Color.white)
        g.fillRect(0, offset, 50 * sWidth, 50 * sHeight)
        g.setColor(Color.BLACK)
        for (i <- 0 until sHeight; j <- 0 until sWidth) {
          g.setColor(Color.black)
          if (grid(i)(j).equals("-"))
            g.drawOval(j * 50, offset + i * 50, 50, 50)
          else if (grid(i)(j).equals("R"))
            g.setColor(Color.red)
            g.fillOval(j * 50 + 1, offset + i * 50 + 1, 49, 49)
          else
            g.setColor(Color.blue)
            g.fillOval(j * 50 + 1, offset + i * 50 + 1, 49, 49)
        }
        drawNumbersOnSquares(g)

      }
    }

    this.add(new ConnectFourArea(frameWidth, frameHeight, 7, 6))

  }
}
def connectFourController(move:String, state:(Array[Array[String]], Boolean)):(Array[Array[String]], Boolean)={
  val grid: Array[Array[String]] = state(0)
  var redMove: Boolean = state(1)


  def checkNumber(s: String): Boolean = {
    if (s.forall(_.isDigit)) {
      if (1 to 7 contains s.toInt) {
        return true
      }
    }
    false
  }

  def validateStringMove(move: String): Boolean = {
    if (move.length != 1)
      return false
    move match {
      case s"$n" =>
        checkNumber(n)
      case _ => false
    }
  }

  def validateMove(n: Int): Int = {
    for (i <- (6 - 1) to 0 by -1) {
      if (grid(i)(n).equals("-")) {
        return i
      }
    }
    -1
  }

  if (!validateStringMove(move)) {
    return (grid, redMove)
  }

  //extract coordinates
  //<l1><n1>,<l2><n2>
  val n = move(0).toString.toInt - 1
  val res = validateMove(n)
  if (res == -1)
    return (grid, redMove)
  if (redMove)
    grid(res)(n) = "R"
  else
    grid(res)(n) = "B"
  (grid, !redMove)
}


def initChessBoard(): Array[Array[String]] = {

  val sWidth: Int = 8
  val sHeight: Int = 8
  val grid: Array[Array[String]] = Array.ofDim(sHeight, sWidth)

  val pieceToChar: Map[String, String] = Map(
    "pawn" -> "\u265F",
    "rook" -> "\u265C",
    "bishop" -> "\u265D",
    "king" -> "\u265A",
    "queen" -> "\u265B",
    "knight" -> "\u265E"
  )

  //upper board
  grid(0)(0) = "b," + pieceToChar("rook")
  grid(0)(sWidth - 1) = "b," + pieceToChar("rook")
  grid(0)(1) = "b," + pieceToChar("knight")
  grid(0)(sWidth - 2) = "b," + pieceToChar("knight")
  grid(0)(2) = "b," + pieceToChar("bishop")
  grid(0)(sWidth - 3) = "b," + pieceToChar("bishop")
  grid(0)(3) = "b," + pieceToChar("queen")
  grid(0)(4) = "b," + pieceToChar("king")
  for (i <- 0 until sWidth)
    grid(1)(i) = "b," + pieceToChar("pawn")


  //lower board
  grid(sHeight - 1)(0) = "w," + pieceToChar("rook")
  grid(sHeight - 1)(sWidth - 1) = "w," + pieceToChar("rook")
  grid(sHeight - 1)(1) = "w," + pieceToChar("knight")
  grid(sHeight - 1)(sWidth - 2) = "w," + pieceToChar("knight")
  grid(sHeight - 1)(2) = "w," + pieceToChar("bishop")
  grid(sHeight - 1)(sWidth - 3) = "w," + pieceToChar("bishop")
  grid(sHeight - 1)(3) = "w," + pieceToChar("queen")
  grid(sHeight - 1)(4) = "w," + pieceToChar("king")
  for (i <- 0 until sWidth)
    grid(sHeight - 2)(i) = "w," + pieceToChar("pawn")
  grid
}
def chessDrawer(grid:Array[Array[String]]):Unit={
  new JFrame{
    val frameWidth: Int = 800
    val frameHeight: Int = 600

    this.setPreferredSize(new Dimension(800, 600))
    this.setDefaultCloseOperation(2)
    this.setResizable(false)
    this.setLayout(null)
    this.pack()
    this.setVisible(true)

    class ChessArea(parentWidth: Int, parentHeight: Int, sWidth: Int, sHeight: Int) extends JPanel {

      val PIECE_BLACK: Color = Color.BLACK
      val PIECE_WHITE: Color = Color.decode("#d3d3d3")
      val SQUARE_BLACK: Color = Color.decode("#769655")
      val SQUARE_WHITE: Color = Color.decode("#eeeed2")

      val pieceToChar: Map[String, String] = Map(
        "pawn" -> "\u265F",
        "rook" -> "\u265C",
        "bishop" -> "\u265D",
        "king" -> "\u265A",
        "queen" -> "\u265B",
        "knight" -> "\u265E"
      )

      //calculating dimensions of panel
      val areaWidth: Int = sWidth * 50
      val areaHeight: Int = sHeight * 50
      //centering panel
      this.setBounds(
        (parentWidth - areaWidth) / 2,
        (parentHeight - areaHeight) / 2,
        areaWidth,
        areaHeight
      )


      def doAlternate(i: Int, j: Int, f: () => Unit, g: () => Unit): Unit = {
        i match {
          case n if n % 2 == 0 =>
            if (j % 2 == 0) f()
            else g()
          case _ =>
            if (j % 2 == 0) g()
            else f()
        }
      }

      private def drawNumbersOnSquares(g:Graphics): Unit = {
        g.setColor(Color.yellow)
        val fontSize = 12f
        g.setFont(g.getFont.deriveFont(fontSize))
        //    g.drawString(piece, 25+j*50-(fontSize/2).toInt, 25+i*50+(fontSize/3).toInt)
        for (i <- sHeight until 0 by -1) {
          this.doAlternate(2, i, () => g.setColor(SQUARE_BLACK), () => g.setColor(SQUARE_WHITE))

          g.drawString(i.toString, 2, -30 + (sHeight - i + 1) * 50 - (fontSize / 2).toInt)

        }

        for (i <- 1 to sWidth) {
          this.doAlternate(2, i, () => g.setColor(SQUARE_BLACK), () => g.setColor(SQUARE_WHITE))
          g.drawString((('a' - 1) + i).toChar.toString, -3 + i * 50 - (fontSize / 2).toInt, 398)
        }

      }

      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)


        val fontSize = 45f


        g.setFont(g.getFont.deriveFont(fontSize))

        for (i <- 0 until this.sHeight; j <- 0 until this.sWidth) {
          //alternate squares
          doAlternate(i, j, () => g.setColor(SQUARE_WHITE), () => g.setColor(SQUARE_BLACK))
          g.fillRect(i * 50, j * 50, 50, 50)

        }

        for (i <- 0 until this.sHeight; j <- 0 until this.sWidth) {
          var name = " "
          //drawing pieces
          grid(i)(j) match {
            case s"b,$piece" => {
              if (pieceToChar("rook").equals(piece))
                name = "BLACK_ROOK"
              else if (pieceToChar("pawn").equals(piece))
                name = "BLACK_PAWN"
              else if (pieceToChar("king").equals(piece))
                name = "BLACK_KING"
              else if (pieceToChar("queen").equals(piece))
                name = "BLACK_QUEEN"
              else if (pieceToChar("knight").equals(piece))
                name = "BLACK_KNIGHT"
              else if (pieceToChar("bishop").equals(piece))
                name = "BLACK_BISHOP"
              name = s"Pieces\\$name.png"
              val image = ImageIO.read(new File(name))
              g.drawImage(image, j * 50, i * 50, 50, 50, null)
            }
            case s"w,$piece" => {
              if (pieceToChar("rook").equals(piece))
                name = "WHITE_ROOK"
              else if (pieceToChar("pawn").equals(piece))
                name = "WHITE_PAWN"
              else if (pieceToChar("king").equals(piece))
                name = "WHITE_KING"
              else if (pieceToChar("queen").equals(piece))
                name = "WHITE_QUEEN"
              else if (pieceToChar("knight").equals(piece))
                name = "WHITE_KNIGHT"
              else if (pieceToChar("bishop").equals(piece))
                name = "WHITE_BISHOP"
              name = s"Pieces\\$name.png"
              val image = ImageIO.read(new File(name))
              g.drawImage(image, j * 50, i * 50, 50, 50, null)
            }
            case _ => Nil
          }
        }
        drawNumbersOnSquares(g)

      }
    }

    this.add(new ChessArea(frameWidth, frameHeight, 8, 8))

  }
}
def chessController(move:String, state:(Array[Array[String]], Boolean)):(Array[Array[String]], Boolean)={

  val grid:Array[Array[String]] = state(0)
  var whitesTurn: Boolean = state(1)

  val pieceToChar = Map(
    "pawn" -> "\u265F",
    "rook" -> "\u265C",
    "bishop" -> "\u265D",
    "king" -> "\u265A",
    "queen" -> "\u265B",
    "knight" -> "\u265E"
  )


  def checkNumber(s: String): Boolean = {
    if (s.forall(_.isDigit)) {
      if (1 to 8 contains s.toInt) {
        return true
      }
    }

    false

  }

  def checkPair(s: String): Boolean = {
    if (s.length == 2) {
      if (('a' to 'h' contains s.charAt(0)) && checkNumber(s.charAt(1).toString)) {
        return true
      }
    }
    false
  }

  def validateStringMove(move: String): Boolean = {
    if (move.length != 5)
      return false
    move match {
      case s"$p1,$p2" =>
        checkPair(p1) && checkPair(p2)
      case _ => false
    }
  }

  def validateKnightMove(l1: Int, n1: Int, l2: Int, n2: Int): Boolean = {
    (l1 + 1 == l2 && n1 - 2 == n2) ||
      (l1 - 1 == l2 && n1 - 2 == n2) ||
      (l1 + 2 == l2 && n1 - 1 == n2) ||
      (l1 - 2 == l2 && n1 - 1 == n2) ||
      (l1 + 2 == l2 && n1 + 1 == n2) ||
      (l1 - 2 == l2 && n1 + 1 == n2) ||
      (l1 + 1 == l2 && n1 + 2 == n2) ||
      (l1 - 1 == l2 && n1 + 2 == n2)
  }

  def validateRookMove(l1: Int, n1: Int, l2: Int, n2: Int): Boolean = {
    if (l1 == l2) {
      //vertical move check
      var step = 0
      if (n2 < n1) {
        //up
        step = -1
      } else if (n2 > n1) {
        //down
        step = 1
      }
      for (i <- (n1 + step) until n2 by step) {
        if (grid(i)(l1) != null) return false
      }
      true
    } else if (n1 == n2) {
      //horizontal move check
      var step = 0
      if (l2 < l1) {
        //up
        step = -1
      } else{
        //down
        step = 1
      }
      for (i <- (l1 + step) until l2 by step) {
        if (grid(n1)(i) != null) return false
      }
      true
    } else {
      false
    }
  }

  def validatePawnMove(l1: Int, n1: Int, l2: Int, n2: Int): Boolean = {

    if (math.abs(l1 - l2) > 1) {
      return false
    }

    var step = 0

    val color = grid(n1)(l1).charAt(0)

    if (color == 'w') {

      step = -1
    } else {
      step = 1
    }

    //two steps move verification
    if (((n1 - step) % 7) != 0 && n2 - n1 == 2 * step) {
      return false
    }

    //if move is not one step forward or two steps forward then exit
    if (n2 - n1 != step && n2 - n1 != 2 * step) {
      return false
    }

    //move one step forward, check pieces in path

    if (l1 == l2) {
      if (grid(n1 + step)(l1) != null) return false
      if (n2 - n1 == 2 * step && grid(n1 + 2 * step)(l1) != null) return false
    } else if (grid(n1 + step)(l2) == null || grid(n1 + step)(l2).charAt(0) == color) {
      return false
    }
    true
  }

  def validateBishopMove(l1: Int, n1: Int, l2: Int, n2: Int): Boolean = {

    if (math.abs(l1 - l2) != math.abs(n1 - n2)) return false

    var hStep = 0
    var vStep = 0
    if (n2 > n1) vStep = 1 else vStep = -1
    if (l2 > l1) hStep = 1 else hStep = -1


    for (i <- 1 until math.abs(l1 - l2)) {
      if (grid(n1 + i * vStep)(l1 + i * hStep) != null) return false
    }

    true

  }

  def validateKingMove(l1: Int, n1: Int, l2: Int, n2: Int): Boolean = {
    if (math.abs(l1 - l2) > 1 || math.abs(n1 - n2) > 1) return false
    if (grid(n2)(l2) != null && grid(n2)(l2).charAt(0) == grid(n1)(l1).charAt(0)) return false
    true
  }

  def validateQueenMove(l1: Int, n1: Int, l2: Int, n2: Int): Boolean = {
    validateBishopMove(l1, n1, l2, n2) || validateRookMove(l1, n1, l2, n2)
  }

  def validateMove(l1: Int, n1: Int, l2: Int, n2: Int): Boolean = {
    if (grid(n1)(l1) == null) {
      return false
    } else if (grid(n1)(l1).charAt(0) == 'w' && !whitesTurn) {
      return false
    } else if (grid(n1)(l1).charAt(0) == 'b' && whitesTurn) {
      return false
    } else if (grid(n2)(l2) != null && grid(n2)(l2).charAt(0) == 'w' && whitesTurn) {
      return false
    } else if (grid(n2)(l2) != null && grid(n2)(l2).charAt(0) == 'b' && !whitesTurn) {
      return false
    }

    val piece = grid(n1)(l1).split(",")(1)
    piece match {
      case x if x.equals(pieceToChar("knight")) =>
        val check = validateKnightMove(l1, n1, l2, n2)
        return check

      case x if x.equals(pieceToChar("rook")) =>
        val check = validateRookMove(l1, n1, l2, n2)
        return check

      case x if x.equals(pieceToChar("pawn")) =>
        val check = validatePawnMove(l1, n1, l2, n2)
        return check
      case x if x.equals(pieceToChar("bishop")) =>
        val check = validateBishopMove(l1, n1, l2, n2)
        return check
      case x if x.equals(pieceToChar("queen")) =>
        val check = validateQueenMove(l1, n1, l2, n2)
        return check
      case x if x.equals(pieceToChar("king")) =>
        val check = validateKingMove(l1, n1, l2, n2)
        return check
      case _ =>
    }
    false
  }

  if (!validateStringMove(move)) {
    return (grid,whitesTurn)
  }

  //extract coordinates
  //<l1><n1>,<l2><n2>
  val l1: Int = move.charAt(0) - 'a'
  val l2: Int = move.charAt(3) - 'a'
  val n1: Int = 8 - move.charAt(1).toString.toInt
  val n2: Int = 8 - move.charAt(4).toString.toInt


  if (!validateMove(l1, n1, l2, n2))
    return (grid,whitesTurn)

  grid(n2)(l2) = grid(n1)(l1)
  if((n2 == 7 && !whitesTurn) || (n2 == 0 && whitesTurn)){
    val piece = grid(n1)(l1).split(",")(1)
    if(piece.equals(pieceToChar("pawn"))){
      if(whitesTurn)
      grid(n2)(l2) = "w,"
      else
      grid(n2)(l2) = "b,"
      grid(n2)(l2) = grid(n2)(l2) + pieceToChar("queen")
    }
  }
  grid(n1)(l1) = null
  (grid, !whitesTurn)

}



def gameEngine(grid:Array[Array[String]],
               drawer:Array[Array[String]]=>Unit,
               controller:(move:String, state:(Array[Array[String]], Boolean))=>(Array[Array[String]], Boolean)
              ):Unit={

  //show the user the chosen game
  drawer(grid)

  var state = (grid, true)
  while(true){

    val move = readLine("Enter next move:")
    val turn = state(1)
    state = controller(move,state)
    if(turn == state(1))
      println("Try Again")
    else
      println("Successful Move")
      drawer(state(0))
  }
}


def init(options:Array[String]): Unit = {

  println("Please choose a game:")

  options.foreach(game => {
    println(s"${options.indexOf(game) + 1}) $game")
  })

  val option:Int = readInt()

  option match{
    //chess
    case 1=> gameEngine(initChessBoard(),chessDrawer, chessController)
    //checkers
    case 2=>gameEngine(initCheckersBoard(), checkersDrawer, checkersController)
    //sudoku
    case 3 => gameEngine(initSudokuBoard(), sudokuDrawer, sudokuController)
    //8-Queens
    case 4 => gameEngine(initEightQueensBoard(), eightQueensDrawer, eightQueensController)
    //tic-tac-toe
    case 5 => gameEngine(initTicTacToeBoard(), ticTacToeDrawer, ticTacToeController)
    //connect-4
    case 6 => gameEngine(initConnectFourBoard(), connectFourDrawer,connectFourController)

  }
}

@main
def main(): Unit = {
  init(Array("Chess", "Checkers", "Sudoku", "8-Queens", "Tic-Tac-Toe", "Connect-4"))
}