package cn.edu.neu.chiewen.cknn

import cn.edu.neu.chiewen.cknn.demo.{DemoData, VoronoiPanel}

import scala.swing.GridBagPanel._
import scala.swing._
import scala.swing.event.ButtonClicked

/**
 * Demo for submission to ICDE 2015
 * Created by chiewen on 7/8/14.
 */
object Demo extends SimpleSwingApplication {
  lazy val ui = new GridBagPanel {
    val cons = new Constraints
    val shouldFill = true
    if (shouldFill) {
      cons.fill = Fill.Horizontal
    }

    def newField(i: Int) = new TextField {
      text = i.toString
      columns = 1
      horizontalAlignment = Alignment.Right

    }

    def newLabel(label: String) = new Label {
      text = label
      border = Swing.EmptyBorder(5, 5, 5, 5)
      horizontalAlignment = Alignment.Right
    }

    def resetConstraint(c: Constraints, x: Int = 0, y: Int = 0) = {
      c.fill = Fill.Horizontal
      c.weightx = 0.5
      c.gridx = x
      c.gridy = y
      c
    }

    val btnReset = new Button("Reset")
    val lblN = newLabel("n=")
    val txtN = newField(50)
    val lblK = newLabel("k=")
    val txtK = newField(5)
    val lblRho = newLabel("rho=")
    val txtRho = newField(1)
    txtRho.text = "1.6"

    val chkAuto = new CheckBox {
      text = "Safe Region"
      selected = false
    }

    val panel = new VoronoiPanel

    listenTo(btnReset, chkAuto)
    reactions += {
      case ButtonClicked(b) => if (b == btnReset) resetDemoData() else DemoData.auto = chkAuto.selected
    }

    layout(btnReset) = resetConstraint(cons, 0)
    layout(lblN) = resetConstraint(cons, 1)
    layout(txtN) = resetConstraint(cons, 2)
    layout(lblK) = resetConstraint(cons, 3)
    layout(txtK) = resetConstraint(cons, 4)
    layout(lblRho) = resetConstraint(cons, 5)
    layout(txtRho) = resetConstraint(cons, 6)
    layout(chkAuto) = resetConstraint(cons, 7)

    cons.fill = Fill.Horizontal
    cons.ipady = VoronoiPanel.HEIGHT //make this component tall
    cons.ipadx = VoronoiPanel.WIDTH
    cons.weightx = 0.0
    cons.gridwidth = 8
    cons.gridx = 0
    cons.gridy = 1
    layout(panel) = cons


    def resetDemoData() {
      DemoData.reset(txtN.text.toInt, txtK.text.toInt, txtRho.text.toDouble,
        panel.size.getWidth.toInt,
        panel.size.getHeight.toInt)
      panel.reset()
    }
  }

  def top = new MainFrame {
    title = "Influential Neighbor Set Demo"
    contents = ui

    ui.btnReset.doClick()

    override def dispose(): Unit = {
      DemoData.system.shutdown()
      super.dispose()
    }
  }
}