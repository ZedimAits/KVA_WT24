package ScalaGo

import ScalaGo.*
import ScalaGo.Color.*
import ScalaGo.FieldContent.*

import scala.io.StdIn

object REPL:

  /**
    * This function runs and evaluates game states and transitions
    * until the global variable gs reaches the passed end state
    *
    * @param endState run until this state is reached
    */
  def repl(endState: GameState): Unit =
    val size = scala.io.StdIn.readLine("Size of Board: ")
    try
      ScalaGo.buildBoard(size)
    catch
      case e: Exception =>
        println(e.getMessage)
        repl(endState)
    
    while gs != endState do {
      val input = scala.io.StdIn.readLine("Set position / pass: ")

      var set = false
      for i <- gs.transitions.indices do
        if (gs.transitions(i)._1(input))
          gs = gs.transitions(i)._2
          gs.startAction()
          set = true

      if (!set) println("Ungültige Eingabe")
    }


  /**
    * Return a String depicting the content of the field
    *
    * @param c the FieldContent to depict
    * @return String depicting the FieldContent
    */
  def getFieldContentString(c: FieldContent): String =
    c match
      case Stone(White) => "○"
      case Stone(Black) => "●"
      case Empty(White) => "\u001b[31m◌\u001b[0m"
      case Empty(Black) => "\u001b[31m◍\u001b[0m"
      case _ => ""

  /**
    * Return a String displaying the current state of the board. If the previous board is supplied it is used to
    * determine differences between the boards to highlight them.
    *
    * @param b board to return as string
    * @param prev previous board to highlight differences
    * @return String representation of the current board
    */
  def getFieldString(b: Goboard, prev: Goboard): String =
    var boardBuilder = new StringBuilder()
    val n = b.length - 1

    def append(string: String, empty: String, endLine: Boolean = false, row: Int = -1): Unit = {
      boardBuilder.append(if string == "" then empty else string)
      boardBuilder.append(
        if endLine then
          " " + row.toString + "\n" + s"${if row != n then ("│"+" "*4)*n + "│\n" else ""}"
        else
          "────"
      )
    }

    for i <- 0 to n do
      for j <- 0 to n do
        var str = getFieldContentString(b(i)(j))
        if getFieldContentString(prev(i)(j)) != str && str != "" then str = "\u001b[36;1m"+str+"\u001b[0m"
        (i,j) match
          case (0,0) => append(str,"┌")
          case (x,y) if x == n && y == n => append(str, "┘", true, i)
          case (0,x) if x == n => append(str,"┐", true, i)
          case (x,0) if x == n => append(str, "└")
          case (x,_) if x == n => append(str,"┴")
          case (_,x) if x == n => append(str,"┤", true, i)
          case (0,_) => append(str,"┬")
          case (_,0) => append(str,"├")
          case _ => append(str,"┼")

    for i <- 0 to n do boardBuilder.append(i.toString + " "*4)
    return boardBuilder.toString()

  /**
    * Print the board to the standard output
    *
    * @param b current board
    * @param pB previous board, may be used to detect and higlight differences
    */
  def printBoard(b: Goboard, pB: Goboard): Unit =
    println(getFieldString(b, pB))

  /**
   * Get Color-String
   *
   * @param c color to convert
   * @return returns the color as a string
   */
  def getColorString(c: Color): String =
    c match
      case White => "White"
      case Black => "Black"
      case _ => "NaC"

  /**
    * Print which player's turn it is to standard output
    */
  def printCurrentPlayer(): Unit =
    println(getColorString(gs.player)+" is on the row")

  /**
    * Print the players' score to standard output
    *
    * @param stonesWhite number of white stones on the board
    * @param fieldsWhite number of fields enclosed by white stones
    * @param stonesBlack number of black stones on the board
    * @param fieldsBlack number of fields enclosed by black stones
    */
  def printFinalScore(stonesWhite: Int, fieldsWhite: Int, stonesBlack: Int, fieldsBlack: Int): Unit =
    println(
      "╔═══════════╗\n"+
      "║\u001b[32;1mFinal Score\u001b[0m║\n"+
      "╚═══════════╝\n"+
      s"White stones on board: $stonesWhite\n"+
      s"White enclosed fields: $fieldsWhite\n"+
      "White total: \u001b[33;1m"+(stonesWhite+fieldsWhite).toString+"\u001b[0m\n"+
      "════════════════════════\n"+
      s"Black stones on board: $stonesBlack\n"+
      s"Black enclosed fields: $fieldsBlack\n"+
      "Black total: \u001b[33;1m"+(stonesBlack+fieldsBlack).toString+"\u001b[0m\n"


    )

