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

  var size: Int = initialSize

  def this() = this ( 10 )

  val grid: ArrayBuffer [ ArrayBuffer [ Cell ] ] =
    new ArrayBuffer ( size )

  for ( rows <- 1 to size ) {
    val newRow: ArrayBuffer [ Cell ] = new ArrayBuffer ( size )
    for ( columns <- 1 to size ) newRow += Cell.dead
    grid += newRow
  }

  override def toString: String = grid.foldLeft ( "" ) (
      ( l, r ) => l ++ ( r.foldLeft ( "" ) ( _ ++ _.toString ) ) ++ "\n" )

  def get ( row: Int, col: Int ): Cell = grid ( row ) ( col )

  def put ( row: Int, col: Int, cell: Cell ): Unit =
    grid ( row ).update ( col, cell )

  def set ( row: Int, col: Int ): Unit = put ( row, col, Cell.alive )

  def unset ( row: Int, col: Int ): Unit = put ( row, col, Cell.dead )

  def aliveNeighbors ( row: Int, col: Int ): Int =
    ( Grid.neighborCoords ( row , col ) filter {
      case ( r, c ) =>
        r >= 0 && r < size && c >= 0 && c < size && get ( r, c ).isOn
    } ).length

  def update: Unit = {

    val toSet   : Set [ ( Int, Int ) ] = Set.empty
    val toUnset : Set [ ( Int, Int ) ] = Set.empty

    for ( row <- 0 until size ; col <- 0 until size ) {
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
