package guiview

import model._

import scala.collection.mutable.ArrayBuffer

import java.awt.event.ActionListener
import java.awt.event.ActionEvent

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
  for ( ( r, c ) <- Grid.gliderSouthEast ) game.set ( r + 2, c + 2 )

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
    game.update
    refreshDisplay
    revalidate
  }

  def refreshDisplay: Unit = {
    labels.clear
    rootPanel remove gridPanel
    gridPanel = new JPanel ( new GridLayout ( game.rows, game.cols, 1, 1 ) )
    for ( row <- 0 until game.rows ) {
      val labelsRow: ArrayBuffer [ GridLabel ] = ArrayBuffer.empty
      for ( col <- 0 until game.cols ) {
        val l = new GridLabel ( game.get ( row, col ) )
        gridPanel add l
        labelsRow += l
      }
      labels += labelsRow
    }
    rootPanel add ( gridPanel, BorderLayout.CENTER )
  }

  def main(args: Array[String]): Unit = UI
}

sealed class GridLabel ( cell: Cell ) extends JLabel {
  setOpaque ( true )
  setBackground ( if ( cell.isOn ) Color.WHITE else Color.BLACK )
}
