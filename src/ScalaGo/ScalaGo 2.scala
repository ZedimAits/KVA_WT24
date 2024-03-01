package ScalaGo

import SimpleList.*

object ScalaGo:
  

  /**
    * Type for specifying a stone's or field's position
    */
  type Position = (Int, Int)

  /**
    * Color of a player or stone
    */
  enum Color:
    case Black, White, Nobody

  import Color.*

  /**
    * FieldContent models the different fields the game board can have.
    */
  enum FieldContent (color: Color):

    /**
     * A stone of a specific color.
     *
     * @param color color of the stone
     */
    case Stone (color: Color) extends FieldContent (color: Color)

    /**
     * An empty field.
     * Has another color than Nobody if a stone was removed from this field in the last round; Referred to as RemovedStone in this case
     *
     * @param color color the stone belonged if a stone was removed from this field in the last round; Nobody if there was no stone
     */
    case Empty (color: Color) extends FieldContent (color)

  import ScalaGo.FieldContent.*

  /**
    * The game state controls the progression of the game.
    *
    * The transitions array contains tuples of functions and target states.
    * If the function of a tuple evaluates to true the target state is activated.
    *
    * @param player who's turn it is
    * @param transitions array of a tuple of transition functions and next GameState
    * @param startAction function to execute when entering the state
    */
  case class GameState(player: Color, transitions: Array[(String => Boolean, GameState)], startAction: () => Unit)

  /**
    * Chain of stones
    *
    * @param color color of the stones
    * @param position positions of the stones
    * @param liberties positions of the chain's liberties
    */
  case class Chain(color: Color, position: SimpleList[Position], liberties: SimpleList[Position])

  /**
    * Alias type for the game board
    */
  type Goboard = Array[Array[FieldContent]]

  var board: Goboard = null // The current board
  var prevBoard: Goboard = null // The previous board (used for detecting repeating positions)

  var gs: GameState = null // Starting GameState, this global variable is used throughout the implementation
  var N: Int = 0 //global variable for board size
  var CHAINS: SimpleList[Chain] = new SimpleList[Chain]() //global variable for chains on the board


  def main(args: Array[String]): Unit =
    val (start, end) = buildStartToEndStates()
    gs = start
    REPL.repl(end)

  /**
    * Generate all necessary states with transitions for the game
    *
    * @return a tuple of the start and the end states
    */
  def buildStartToEndStates(): (GameState, GameState) =
    //state1: Schwarz ist am Zug
    //state2: Weiß ist am Zug
    //state3: Weiß ist am Zug (Schwarz hat gepasst)
    //state4: Schwarz ist am Zug (Weiß hat gepasst)
    //state5: EndState
    var state1 = GameState(Black, new Array[(String => Boolean, GameState)](2), () => {outputBoardAndPlayer()})
    var state2 = GameState(White, new Array[(String => Boolean, GameState)](2), () => {outputBoardAndPlayer()})
    var state3 = GameState(White, new Array[(String => Boolean, GameState)](2), () => {outputBoardAndPlayer()})
    var state4 = GameState(Black, new Array[(String => Boolean, GameState)](2), () => {outputBoardAndPlayer()})
    var state5 = GameState(Nobody, Array(), () => {calculateScore()})

    state1.transitions(0) = (placeStoneTransition, state2)
    state1.transitions(1) = (passTransition, state3)
    state2.transitions(0) = (placeStoneTransition, state1)
    state2.transitions(1) = (passTransition, state4)
    state3.transitions(0) = (placeStoneTransition, state1)
    state3.transitions(1) = (passTransition, state5)
    state4.transitions(0) = (placeStoneTransition, state2)
    state4.transitions(1) = (passTransition, state5)

    (state1,state5)

  /**
    * Output the current board and player and afterwards remove old RemovedStone markers
    */
  def outputBoardAndPlayer(): Unit =
    REPL.printBoard(board,prevBoard)
    println(gs.player.toString + " is on the row")

  /**
    * Remove all RemovedStone markers from the board
    *
    * @param b board to remove from
    */
  def removeOldRemovedStones(b: Goboard): Unit =
    for i <- b.indices; j <- b.indices do 
      val content = getFieldContent(i,j,b)
      if(content == Empty(Black) || content == Empty(White)) b(i)(j) = Empty(Nobody)

  /**
    * Remove stones of each chain's positions where the chain has no liberties
    *
    * @param chains list of chains
    * @param c      only remove stones of this color
    * @param b      board to remove the stones from
    */
  def removeStonesOfZeroChains(chains: SimpleList[Chain], c: Color, b: Goboard): Unit = {
    var positions: Array[Position] = Array()
    var cur = chains.next
    var prev = chains
    while cur != null do {
      if (cur.entry.isDefined && cur.entry.get.color == c && slLength(cur.entry.get.liberties) == 0)
        loopFunc(cur.entry.get.position, (x: Position) => {
          positions :+= x
        })
        prev.next = cur.next
        cur = cur.next
      else
        prev = cur
        cur = cur.next
    }
    for i <- positions do b(i._1)(i._2) = Empty(c)
  }


  /**
    * Remove all stones of chains with zero liberties starting with the opposing players color and then remove remaining
    * chains with zero liberties of the own color
    *
    * @param b board to remove the stones from
    */
  def killChains(b: Goboard): Unit = {
    val c1 = gs.player
    val c2 = getOppositeColor(c1)
    removeStonesOfZeroChains(CHAINS, c1, b)
    removeStonesOfZeroChains(CHAINS, c2, b)
  }


  /**
    * Tests if two boards are identical with regard to only Empty and Stones.
    *
    * @param b1 board one
    * @param b2 board two
    * @return true if both boards represent equal positions, false otherwise
    */
  def equalBoardPositions(b1: Goboard, b2: Goboard): Boolean = {
    for i <- b1.indices; j <- b1.head.indices do
      if (b1(i)(j) != b2(i)(j)) return false
    return true
  }
  /**
    * Create a real copy of the board
    *
    * @param b board to copy
    * @return an identical board at a different memory location
    */
  def copyBoard(b: Goboard): Goboard = {
    var o = Array.fill(b.length)(Array.fill(b.head.length)(Empty(Nobody)))
    for i <- b.indices; j <- b.head.indices do o(i)(j) = b(i)(j)
    o
  }
  /**
   * Get the opposite color
   *
   * @param c color provided
   * @return opposite color
   */
  def getOppositeColor(c: Color): Color = if c == Black then White else Black

  /**
    * Set a position within a board. Thows an OutOfBoardException if the position is not within the board.
    *
    * @param b   the board to place the field in
    * @param pos the position
    * @param fc  the new content of the field at position
    */
  def setPosition(b: Goboard, pos: Position, fc: FieldContent): Unit = {
    if(!validPos(pos)) throw new Exception(pos.toString() + " is not within the board")
    else if(b(pos._1)(pos._2) == Stone(Black) || b(pos._1)(pos._2) == Stone(White)) throw new Exception(pos.toString() + " is already taken")
    else if(slLength(getLiberties(pos,b)) == 0 && slLength(getSurroundingColor(pos,gs.player, b)) == 0) throw new Exception("Suicide!")
    else
      var l: Array[Array[Position]] = Array()
      loopFunc(CHAINS,(x: Chain)=>{
        if(x.color == gs.player){
          var newArr: Array[Position] = Array()
          loopFunc(x.liberties, (y: Position)=>{newArr :+= y})
          l :+= newArr
        }
      })
      for i <- l do
        if i.contains(pos) && i.length == 1 then throw new Exception("Suicide!")

      prevBoard = copyBoard(b)
      b(pos._1)(pos._2) = fc
      if(pos._1 == 2 && pos._2 == 2) {
        println(b(2)(2))
        println(prevBoard(2)(2))
      }

      addToChain(pos,gs.player,CHAINS, b)
      removeOldRemovedStones(b)
      killChains(b)
  }

  /**
   * Adds the Stone to the suitable chain
   * or connects chains if necessary
   * and updates liberties of each chain
   *
   * @param position position of added Stone
   * @param color color of added Stone
   * @param chains chain List in which Stone should be added
   * @param b board of the added Stone
   */
  def addToChain(position: Position, color: Color, chains: SimpleList[Chain],b: Goboard): Unit = {
    val surr = getSurroundingColor(position, color, b)
    if (slLength(surr) == 0) {
      val firstChainPos = new SimpleList[Position]()
      slAppend(firstChainPos, position)
      var firstChain: Chain = new Chain(color, firstChainPos, new SimpleList[Position]())
      slAppend(chains, firstChain)
    }
    else if (slLength(surr) == 1) {
      val el = surr.next.entry.get
      loopFunc(chains, (x: Chain) => {
        if (slContains(x.position, el._2))
          slAppend(x.position, position)
      })
    }
    else {
      var chainPositions: Array[SimpleList[Position]] = Array()
      loopFunc(surr, (x: (FieldContent, Position)) => {
        loopFunc(chains, (y: Chain) => {
          if slContains(y.position, x._2) && !chainPositions.contains(y.position) then chainPositions :+= y.position
        })
      })
      if chainPositions.length == 1 then slAppend(chainPositions.head, position)
      else {
        val newPos = new SimpleList[Position]()
        slAppend(newPos, position)
        for x <- chainPositions do
          loopFunc(x, (y: Position) => {
            slAppend(newPos, y)
          })

        var cur = chains.next
        var prev = chains
        while cur != null do {
          if (cur.entry.isDefined && chainPositions.contains(cur.entry.get.position))
            prev.next = cur.next
            cur = cur.next
          else
            prev = cur
            cur = cur.next
        }

        slAppend(chains, new Chain(color, newPos, new SimpleList[Position]()))
      }
    }

    updateLiberties(chains, b)
  }

  /**
   * Get surrounding empty fields based on a board-position
   *
   *
   * @param position the position from which the liberties are calculated
   * @param b board on which the position is based on
   * @return list of liberties
   */
  def getLiberties(position: Position, b: Goboard): SimpleList[Position] = {
    val surr = getSurrounding(position, b)
    var o = new SimpleList[Position]()
    loopFunc(surr, (x: (FieldContent, Position)) => {
      if x._1.isInstanceOf[Empty] then slAppend(o, x._2)
    })
    return o
  }

  /**
   * Get surrounding fields with a specific color
   *
   * @param position the position from which the surrounding fields are calculated
   * @param color color of surrounding fields
   * @param b board on which the position is based on
   * @return list of liberties
   */
  def getSurroundingColor(position: Position,color: Color, b: Goboard): SimpleList[(FieldContent,Position)] = {
    val surr = getSurrounding(position, b)
    var o = new SimpleList[(FieldContent, Position)]()
    loopFunc(surr, (x: (FieldContent, Position)) => {
      if (x._1 == Stone(color)) slAppend(o, x)
    })
    return o
  }

  /**
   * Get surrounding fields in borders of the board
   *
   * @param position the position from which the surrounding fields are calculated
   * @param b        board on which the position is based on
   * @return list of liberties
   */
  def getSurrounding(position: Position, b: Goboard): SimpleList[(FieldContent,Position)] = {
    var o = new SimpleList[(FieldContent, Position)]()
    val dirs = Array((0, 1), (0, -1), (1, 0), (-1, 0))
    for i <- dirs do
      val newPos = addPos(position, i)
      if validPos(newPos) then slAppend(o, (getFieldContent(newPos, b), newPos))
    return o
  }


  /**
   * Checks if a position is whithin the board based on the global variable N
   *
   * @param pos position being checked
   * @return
   */
  def validPos(pos: Position): Boolean = pos._1 >= 0 && pos._2 >= 0 && pos._2 < N && pos._1 < N

  /**
   * Adds two positions
   *
   * @param pos position to add 
   * @param dir position to add
   * @return added positions
   */
  def addPos(pos: Position, dir: Position): Position = (pos._1 + dir._1, pos._2 + dir._2)

  /**
    * Calculate the score according to the area covered by the players
    *
    */
  def calculateScore(): Unit =
    val stonesWhite = (for i <- 0 until N; j <- 0 until N if board(i)(j) == Stone(White) yield 1).sum
    val fieldsWhite = calculateArea(board,White)
    val stonesBlack = (for i <- 0 until N; j <- 0 until N if board(i)(j) == Stone(Black) yield 1).sum
    val fieldsBlack = calculateArea(board,Black)
    REPL.printFinalScore(stonesWhite,fieldsWhite,stonesBlack,fieldsBlack)

  /**
    * Generate a board of size*size dimensions.
    *  Throws an error if the size-string is not valid
    *
    * @param size the horizontal and vertical dimension of the board
    * @return the board initialized with Empty() fields
    */
  def buildBoard(size: String): Unit = {
    var sizeInt = 0
    try
      sizeInt = size.toInt
    catch
      case e => throw new Exception("'" + size + "' is not a number")
    if (sizeInt % 2 == 0 || sizeInt < 5 || sizeInt > 19) throw new Exception(size + " is not valid bord size. Try an odd number from 5 to 19")
    else
      N = sizeInt
      val emptyBoard = Array.tabulate(size.toInt)(x => Array.fill(size.toInt)(Empty(Nobody)))

      board = copyBoard(emptyBoard)
      prevBoard = copyBoard(emptyBoard)

    outputBoardAndPlayer()
  }


  /**
    * Parse a player's input to detect a stone placement command and place a stone
    *
    * @param command input from the player
    * @return true if the command could be understood, false otherwise
    */
  def placeStoneTransition(command: String): Boolean = {
    val split = command.split(" ")
    if split.length == 2 && split.forall(_.forall(_.isDigit)) then
      val IntSplit = split.map(_.toInt)
      try
        setPosition(board, (IntSplit(1), IntSplit(0)), Stone(gs.player))
      catch
        case e: Exception =>
          println(e.getMessage)
          return false
      return true
    else return false
  }

  /**
    * Parse a player's input to detect the 'pass' command
    *
    * @param command input from the player
    * @return true if the command could be understood, false otherwise
    */
  def passTransition(command: String): Boolean = command.equals("pass")


  /**
    * Get the contents of a field by row,column indices.
    *
    * This is a convenience function that directly calls getFieldConten(p: Position, b: Goboard)
    *
    * @param row    row index
    * @param column column index
    * @param b      the board
    * @return the FieldContent which may be OutOfBounds
    */
  def getFieldContent(row: Int, column: Int, b: Goboard): FieldContent = getFieldContent((row, column), b)

  /**
    * Get the contents of a field by Position.
    *
    * @param p the position
    * @param b the board
    * @return the FieldContent which may be OutOfBounds
    */
  def getFieldContent(p: Position, b: Goboard): FieldContent = b(p._1)(p._2)
  
  /**
    * Find all positions that are liberties along a chain
    *
    * @param chains chain to search for liberties positions
    * @param b board to use
    */
  def updateLiberties(chains: SimpleList[Chain], b: Goboard): Unit = {
    //loop through chains
    loopFunc(chains,(chain: Chain)=>{
      var newLib = new SimpleList[Position]()
      //loop through all postions
      loopFunc(chain.position, (x: Position) => {
        //get liberties of each Pos
        val lib = getLiberties(x, b)
        loopFunc(lib, (y: Position) => {
          //add to newLib, if not already in
          if !slContains(newLib, y) then slAppend(newLib, y)
        })
      })
      if slLength(newLib) == 0 then chain.liberties.next = null
      else chain.liberties.next = newLib
    })
  }

  /**
   * calculate the area that is covered by a color
   * doesnt work with "dead" stones
   *
   * @param b the board the area is calculated
   * @param c the color of the area
   */
  def calculateArea(b: Goboard, c: Color): Int = {
    val emptyFields: Array[Position] = (for i <- 0 until N; j <- 0 until N if b(i)(j).isInstanceOf[Empty] yield (i,j)).toArray

    val emptyBoard = Array.tabulate(N)(x=>Array.fill(N)(Empty(Nobody)))

    var emptyChain = new SimpleList[Chain]()

    for i <- emptyFields do
      emptyBoard(i._1)(i._2) = Stone(Black)
      addToChain(i,Black,emptyChain,emptyBoard)

    var area = 0
    loopFunc(emptyChain,(x: Chain)=>{
      var oneColor = true
      loopFunc(x.liberties, (y: Position)=>{
        val fc = b(y._1)(y._2).asInstanceOf[Stone]
        if(fc.color != c) oneColor = false
      })
      if oneColor then area += slLength(x.position)
    })

    return area
  }