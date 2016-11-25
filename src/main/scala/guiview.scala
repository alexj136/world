package guiview

import model._

import scala.collection.mutable.ArrayBuffer

import java.awt.event.ActionListener
import java.awt.event.ActionEvent

import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JLabel
import javax.swing.JButton

import java.awt.Color
import java.awt.GridLayout
import java.awt.BorderLayout
import java.awt.FlowLayout

object UI extends JFrame {

  val game: Grid = new Grid
  for ( ( r, c ) <- Grid.gliderSouthEast ) game.set ( r + 2, c + 2 )

  setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE )
  
  val rootPanel: JPanel = new JPanel ( new BorderLayout )
  val gridPanel: JPanel =
    new JPanel ( new GridLayout ( game.size , game.size, 1, 1 ) )
  val controlPanel: JPanel = new JPanel ( new FlowLayout )

  val labels: ArrayBuffer [ ArrayBuffer [ JLabel ] ] = ArrayBuffer.empty

  for ( row <- 0 until game.size ) {
    val labelsRow: ArrayBuffer [ JLabel ] = ArrayBuffer.empty
    for ( col <- 0 until game.size ) {
      val l = new JLabel
      l setOpaque true
      gridPanel add l
      labelsRow += l
    }
    labels += labelsRow
  }

  rootPanel add ( gridPanel, BorderLayout.CENTER )
  rootPanel add ( controlPanel , BorderLayout.SOUTH )
  val stepButton: JButton = new JButton
  stepButton setText "->"
  controlPanel add stepButton

  stepButton addActionListener ( new ActionListener {
    override def actionPerformed ( e: ActionEvent ): Unit = updateDisplay
  } )

  add ( rootPanel )
  setVisible ( true )
  
  def updateDisplay: Unit = {
    game.update
    refreshDisplay
  }

  def refreshDisplay: Unit = {
    for ( row <- 0 until game.size ; col <- 0 until game.size )
      labels ( row ) ( col ).setBackground (
        if ( game.get ( row, col ).isOn )
          Color.WHITE
        else
          Color.BLACK
      )
  }

  def main(args: Array[String]): Unit = UI.refreshDisplay
}
