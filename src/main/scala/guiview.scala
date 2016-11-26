package guiview

import model._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set

import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.event.MouseListener
import java.awt.event.MouseEvent

import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JLabel
import javax.swing.JButton
import javax.swing.JScrollPane

import java.awt.Color
import java.awt.GridLayout
import java.awt.BorderLayout
import java.awt.FlowLayout
import java.awt.Dimension

object UI extends JFrame {

  val game: Grid = new Grid

  setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE )
  
  val rootPanel: JPanel = new JPanel ( new BorderLayout )

  val controlPanel: JPanel = new JPanel ( new FlowLayout )

  var gridPanel: JPanel = new JPanel

  val labels: ArrayBuffer [ ArrayBuffer [ GridLabel ] ] = ArrayBuffer.empty

  val stepButton: JButton = new JButton
  stepButton setText "->"
  stepButton addActionListener ( new ActionListener {
    override def actionPerformed ( e: ActionEvent ): Unit = updateDisplay
  } )

  rootPanel add ( controlPanel , BorderLayout.SOUTH )
  controlPanel add stepButton
  refreshDisplay
  add ( rootPanel )
  setVisible ( true )
  
  def updateDisplay: Unit = {
    toToggle foreach { case ( r, c ) => game.toggle ( r, c ) }
    toToggle.clear
    game.update
    refreshDisplay
  }

  def refreshDisplay: Unit = {
    labels.clear
    rootPanel remove gridPanel
    gridPanel = new JPanel ( new GridLayout ( game.rows, game.cols, 1, 1 ) )
    for ( row <- 0 until game.rows ) {
      val labelsRow: ArrayBuffer [ GridLabel ] = ArrayBuffer.empty
      for ( col <- 0 until game.cols ) {
        val l = new GridLabel ( row, col )
        gridPanel add l
        labelsRow += l
      }
      labels += labelsRow
    }
    gridPanel setPreferredSize (
      new Dimension ( game.rows * 20, game.cols * 20 ) )
    rootPanel add ( new JScrollPane ( gridPanel ), BorderLayout.CENTER )
    revalidate
  }

  val toToggle: Set [ ( Int, Int ) ] = Set.empty

  def main(args: Array[String]): Unit = UI
}

sealed class GridLabel ( row: Int, col: Int ) extends JLabel {
  var curColor: Color = if ( UI.game.get ( row, col ).isOn ) Color.WHITE
    else Color.BLACK

  def flipColor: Unit = {
    curColor = if ( curColor == Color.WHITE ) Color.BLACK else Color.WHITE
    setBackground ( curColor )
    UI.revalidate
  }

  setOpaque ( true )

  setBackground ( curColor )

  addMouseListener ( new MouseListener {

    override def mouseClicked  ( e: MouseEvent ): Unit = {
      flipColor
      if ( UI.toToggle ( ( row, col ) ) )
        UI.toToggle -= ( ( row, col ) )
      else
        UI.toToggle += ( ( row, col ) )
    }

    override def mouseEntered  ( e: MouseEvent ): Unit = Unit
    override def mouseExited   ( e: MouseEvent ): Unit = Unit
    override def mousePressed  ( e: MouseEvent ): Unit = Unit
    override def mouseReleased ( e: MouseEvent ): Unit = Unit

  } )
}

