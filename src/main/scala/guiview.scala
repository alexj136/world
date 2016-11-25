package guiview
import model._
import scala.swing._
import scala.swing.event._
import scala.collection.mutable.ArrayBuffer
import java.awt.Color

object UI extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Game of Life"

    val game: Grid = new Grid
    for ( ( r, c ) <- Grid.gliderSouthEast ) game.set ( r + 2, c + 2 )

    val gridPanel = new GridPanel ( game.size, game.size ) {

      val labels: ArrayBuffer[ ( Int, Int, Label ) ] = ArrayBuffer.empty

      def refresh = for ( ( row, col, label ) <- labels ) {
        label.text = game.get( row, col ).toString
      }

      for ( row <- 0 until game.size ; col <- 0 until game.size ) {
        val label = new Label {
          text = game.get( row, col ).toString
          background = Color.black
        }
        labels += ( ( row, col, label ) )
        contents += label
      }
    }


    val stepButton = new Button { text = "Step" }

    val controlPanel = new FlowPanel {
      contents += stepButton
    }

    val mainPanel = new BorderPanel {
      layout(gridPanel) = BorderPanel.Position.Center
      layout(controlPanel) = BorderPanel.Position.South
    }

    listenTo(stepButton)

    reactions += {
      case ButtonClicked ( b ) => {
        game.update
        gridPanel.refresh
      }
    }

    contents = mainPanel
  }
}
