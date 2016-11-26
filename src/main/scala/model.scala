package model
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set

class Cell ( val isOn: Boolean ) {
  override def toString: String = if ( isOn ) "*" else "-"
}

object Cell {
  def dead:  Cell = new Cell ( false )
  def alive: Cell = new Cell ( true  )
}

class Grid ( initialSize: Int ) {

  def this() = this ( 10 )

  val grid: ArrayBuffer [ ArrayBuffer [ Cell ] ] =
    new ArrayBuffer ( initialSize )

  for ( row <- 1 to initialSize ) {
    val newRow: ArrayBuffer [ Cell ] = new ArrayBuffer ( initialSize )
    for ( col <- 1 to initialSize ) newRow += Cell.dead
    grid += newRow
  }

  def rows: Int = grid.size
  def cols: Int = grid ( 0 ).size

  override def toString: String = grid.foldLeft ( "" ) (
      ( l, r ) => l ++ ( r.foldLeft ( "" ) ( _ ++ _.toString ) ) ++ "\n" )

  def get ( row: Int, col: Int ): Cell = grid ( row ) ( col )

  def put ( row: Int, col: Int, cell: Cell ): Unit =
    grid ( row ).update ( col, cell )

  def set ( row: Int, col: Int ): Unit = put ( row, col, Cell.alive )

  def unset ( row: Int, col: Int ): Unit = put ( row, col, Cell.dead )

  def toggle ( row: Int, col: Int ): Unit = put ( row, col,
    if ( get ( row, col ).isOn ) Cell.dead else Cell.alive )

  def aliveNeighbors ( row: Int, col: Int ): Int =
    ( Grid.neighborCoords ( row , col ) filter {
      case ( r, c ) =>
        r >= 0 && r < rows && c >= 0 && c < cols && get ( r, c ).isOn
    } ).length

  def update: Unit = {

    val toSet   : Set [ ( Int, Int ) ] = Set.empty
    val toUnset : Set [ ( Int, Int ) ] = Set.empty

    for ( row <- 0 until rows ; col <- 0 until cols ) {
      val cell: Cell = get ( row, col )
      val liveNeighbors: Int = aliveNeighbors ( row, col )
      if ( cell.isOn && ( liveNeighbors < 2 || liveNeighbors > 3 ) )
        toUnset += ( ( row, col ) )
      else if ( ( !cell.isOn ) && liveNeighbors == 3 )
        toSet   += ( ( row, col ) )
      else { }
    }

    toSet   foreach { case ( r, c ) => set   ( r, c ) }
    toUnset foreach { case ( r, c ) => unset ( r, c ) }

    expand ( borderCheck )
  }

  /**
   * Determine whether or not the grid needs to grow, and if so, in which
   * directions
   */
  def borderCheck: Set [ Direction ] = {

    val dirs: Set [ Direction ] = Set.empty

    if ( ( grid ( 0 )
      filter { c: Cell => c.isOn } ).length > 0 ) {
        dirs += North
    }

    if ( ( grid map ( _ ( cols - 1 ) )
      filter { c: Cell => c.isOn } ).length > 0 ) {
        dirs += East
    }

    if ( ( grid ( rows - 1 )
      filter { c: Cell => c.isOn } ).length > 0 ) {
        dirs += South
    }

    if ( ( grid map ( _ ( 0 ) )
      filter { c: Cell => c.isOn } ).length > 0 ) {
        dirs += West
    }

    dirs
  }

  /**
   * Expand the grid in all required directions
   */
  def expand ( dirs: Set [ Direction ] ): Unit = {

    if ( dirs ( North ) ) {
      grid prepend ( ArrayBuffer.fill ( cols ) ( Cell.dead ) )
    }

    if ( dirs ( East  ) ) {
      grid map ( _ append  Cell.dead )
    }

    if ( dirs ( South ) ) {
      grid append  ( ArrayBuffer.fill ( cols ) ( Cell.dead ) )
    }

    if ( dirs ( West  ) ) {
      grid map ( _ prepend Cell.dead )
    }
  }
}

object Grid {
  def neighborCoords ( row: Int, col: Int ): List [ ( Int, Int ) ] = List (
      ( row - 1, col - 1 )
    , ( row - 1, col     )
    , ( row - 1, col + 1 )
    , ( row    , col - 1 )
    , ( row    , col + 1 )
    , ( row + 1, col - 1 )
    , ( row + 1, col     )
    , ( row + 1, col + 1 ) )

  val gliderSouthEast: List [ ( Int, Int ) ] = List (
      ( -1 ,  0 )
    , (  0 ,  1 )
    , (  1 , -1 )
    , (  1 ,  0 )
    , (  1 ,  1 ) )
}

sealed abstract class Direction
case object North extends Direction
case object East  extends Direction
case object South extends Direction
case object West  extends Direction
