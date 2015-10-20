package cn.edu.neu.chiewen.roadDemo

import javax.swing.BorderFactory
import javax.swing.table.DefaultTableModel

import cn.edu.neu.chiewen.cknn.demo.{DemoData, VoronoiPanel}
import cn.edu.neu.chiewen.roadDemo.moving.{KnnRefreshEvent, MovingController}
import cn.edu.neu.chiewen.roadDemo.ui._
import com.oreilly.swinghks._

import scala.swing.GridBagPanel._
import scala.swing.Swing._
import scala.swing._
import scala.swing.event.{ButtonClicked, ValueChanged}

/**
 * Created by Chiewen on 2015/9/15.
 */
object INSQ_Demo extends SimpleSwingApplication {
  lazy val ui = new GridBagPanel {
    val cons = new Constraints

    def newField(i: String) = new TextField {
      text = i
      columns = 3
      horizontalAlignment = Alignment.Right
    }

    def newLabel(label: String) = new Label {
      text = label
      border = Swing.EmptyBorder(5, 5, 5, 5)
      horizontalAlignment = Alignment.Right
    }

    def newCheckBox(t: String, show: Boolean = true, position: Alignment.Value = Alignment.Right) = new CheckBox {
      text = t
      this.horizontalTextPosition = position
      selected = show
    }


    def resetConstraint(c: Constraints, x: Int = 0, y: Int = 0, gw: Int = 1, ix: Int = 0, iy: Int = 0, fill: Fill.Value = Fill.Horizontal) = {
      c.fill = fill
      c.gridx = x
      c.gridy = y
      c.gridwidth = gw
      if (ix != 0) c.ipadx = ix
      if (iy != 0) c.ipady = iy
      c
    }

    def setCenterPanel(): Unit = {
      if (rbRoad.selected && !centerPanel.contents.contains(panel_road)) {
        centerPanel.contents.clear()
        centerPanel.contents += panel_road
        panel_road.requestFocus()
        revalidate()
      }
      else if (rbPlane.selected && !centerPanel.contents.contains(panel_plane)) {
        centerPanel.contents.clear()
        centerPanel.contents += panel_plane
        revalidate()
      }
    }

    val btnReset = new Button("Reset")
    val btnSave = new Button("Save")
    val btnRead = new Button("Read")
    val lblN = newLabel("n=")
    val txtN = newField("50")
    val lblK = newLabel("k=")
    val txtK = newField("5")
    val lblRho = newLabel("rho=")
    val txtRho = newField("1.6")

    val chkAuto = newCheckBox("", show = false, position = Alignment.Left)
    val chkShowVoronoi = newCheckBox("", show = false, position = Alignment.Left)

    val rbSite = new RadioButton("Site")
    val rbAdd = new RadioButton("Node")
    val rbTrajectory = new RadioButton("Trajectory")
    val rbDemo = new RadioButton("Demo")
    val rbRoadOperationGroup = new ButtonGroup(rbSite, rbAdd, rbDemo, rbTrajectory)

    val rbSatellite = new RadioButton("Satellite")
    val rbOrdinary = new RadioButton("Ordinary") {
      selected = true
    }
    val rbMapGroup = new ButtonGroup(rbSatellite, rbOrdinary)

    val rbRoad = new RadioButton("Road Network") {
      selected = false
    }
    val rbPlane = new RadioButton("2D Plane") {
      selected = true
    }
    val rbShowGroup = new ButtonGroup(rbRoad, rbPlane)

    val headers = Array("No.", "ID", "x", "y")
    val tableModel = new DefaultTableModel(new Array[Array[AnyRef]](0), Array[AnyRef]("No.", "ID", "x", "y"))
    val tableModel1 = new DefaultTableModel(new Array[Array[AnyRef]](0), Array[AnyRef]("No.", "ID", "x", "y"))
    val tblKnn = new Table(MovingController.dataKnn.asInstanceOf[Array[Array[Any]]], headers) {
      model = tableModel
    }
    val tblVoronoi = new Table(MovingController.dataVoronoi.asInstanceOf[Array[Array[Any]]], headers) {
      model = tableModel1
    }

    val chkRoad = newCheckBox("Road", show = false)
    val chkNode = newCheckBox("Node", show = false)
    val chkKnn = newCheckBox("kNN")
    val chkNeighbors = newCheckBox("INS")

    val slSpeed = new Slider() {
      min = 0
      max = MovingController.stepTimeMax - 5
      value = MovingController.stepTimeMax - MovingController.stepTime - 5
      orientation = Orientation.Horizontal
    }

    val panel_road = new RoadVoronoiPanel
    val panel_plane = new VoronoiPanel
    val centerPanel = new BoxPanel(Orientation.Vertical) {
      contents += panel_plane
    }


    def refreshKnnTable(): Unit = {
      val model = tblKnn.peer.getModel.asInstanceOf[DefaultTableModel]
      for (i <- 0 until model.getRowCount) model.removeRow(0)
      for (i <- MovingController.dataKnn.indices) model.addRow(MovingController.dataKnn(i).asInstanceOf[Array[AnyRef]])
      tblKnn.repaint()
      val model1 = tblVoronoi.peer.getModel.asInstanceOf[DefaultTableModel]
      for (i <- 0 until model1.getRowCount) model1.removeRow(0)
      for (i <- MovingController.dataVoronoi.indices) model1.addRow(MovingController.dataVoronoi(i).asInstanceOf[Array[AnyRef]])
      tblVoronoi.repaint()
    }

    val topPanel = new FlowPanel(FlowPanel.Alignment.Center)(new Label(" "))

    val leftPanel = new GridBagPanel {
      val cons = new Constraints()

      def resetConstraint(c: Constraints, x: Int = 0, y: Int = 0, gw: Int = 1,
                          ix: Int = 0, iy: Int = 0, fill: Fill.Value = Fill.Horizontal) = {
        c.fill = fill
        c.gridx = x
        c.gridy = y
        c.gridwidth = gw
        if (ix != 0) c.ipadx = ix
        if (iy != 0) c.ipady = iy
        c
      }

      layout(new BoxPanel(Orientation.Vertical) {
        border = BorderFactory.createTitledBorder("Global Setting")
        contents += new BoxPanel(Orientation.Vertical) {
          contents += new FlowPanel(FlowPanel.Alignment.Left)(new Label("Map type:    "), rbSatellite, new Label("           "), rbOrdinary)
          contents += new FlowPanel(FlowPanel.Alignment.Left)(new Label("Demo mode: "), rbRoad, new Label("  "), rbPlane)
          contents += new FlowPanel(FlowPanel.Alignment.Left)(lblK, txtK, new Label("      "), btnSave, new Label("  "),
            btnRead)
        }
      }) = resetConstraint(cons, 0, 0, 1)

      layout(new BoxPanel(Orientation.Vertical) {
        border = BorderFactory.createTitledBorder("Road Network")
        contents += new FlowPanel(FlowPanel.Alignment.Left)(new Label("Operation:"), rbAdd, new Label("         "), rbSite)
        contents += new FlowPanel(FlowPanel.Alignment.Left)(new Label("                   "), rbTrajectory, new Label(""), rbDemo)
        contents += new FlowPanel(FlowPanel.Alignment.Left)(new Label("Show:      "), chkNode, new Label(""),
          chkRoad, new Label(""), chkKnn, new Label(""), chkNeighbors)
        contents += new FlowPanel(FlowPanel.Alignment.Left)(new Label("Moving Speed:"), slSpeed)
      }) = resetConstraint(cons, 0, 1, 1)

      layout(new BoxPanel(Orientation.Vertical) {
        border = BorderFactory.createTitledBorder("2D Plane")
        contents += new FlowPanel(FlowPanel.Alignment.Left)(new Label("Node Parameter:"), lblN, txtN, new Label(" "), lblRho, txtRho)
        contents += new FlowPanel(FlowPanel.Alignment.Left)(new Label("Show Voronoi Cells:"), chkShowVoronoi)
        contents += new FlowPanel(FlowPanel.Alignment.Left)(new Label("Show Safe Region:  "), chkAuto, new Label("   "), btnReset)
      }) = resetConstraint(cons, 0, 2, 1)

      layout(new BoxPanel(Orientation.Vertical) {
        border = BorderFactory.createTitledBorder("Demo data")
        contents += new Label("Nearest neighbors:")
        contents += new ScrollPane(tblKnn) {
          preferredSize = (180, 150)
        }
        contents += new Label("Influential sets:")
        contents += new ScrollPane(tblVoronoi) {
          preferredSize = (180, 230)
        }
      }) = resetConstraint(cons, 0, 3, 1)
    }

    layout(topPanel) = resetConstraint(cons, 0, 0, 2)
    layout(leftPanel) = resetConstraint(cons, 0, 1, 1, fill = Fill.Vertical)
    layout(centerPanel) = resetConstraint(cons, 1, 1, 1, fill = Fill.Vertical)
    layout(Component.wrap(new StatusBar())) = resetConstraint(cons, 0, 2, 2, iy = 6)
    setCenterPanel()

    def resetDemoData() {
      DemoData.reset(txtN.text.toInt, txtK.text.toInt, txtRho.text.toDouble,
        VoronoiPanel.WIDTH, VoronoiPanel.HEIGHT)
      panel_plane.reset()
    }

    def refreshActionStateRadioButtons(): Unit = {
      rbAdd.selected = false
      rbDemo.selected = false
      rbSite.selected = false
      rbTrajectory.selected = false

      panel_road.actionState match {
        case panel_road.ActionState.Add => rbAdd.selected = true
        case panel_road.ActionState.Demo => rbDemo.selected = true
        case panel_road.ActionState.Site => rbSite.selected = true
        case panel_road.ActionState.Trajectory => rbTrajectory.selected = true
      }
    }

    listenTo(btnReset, btnSave, btnRead, chkAuto, chkKnn, chkNeighbors, chkNode, chkRoad,
      rbAdd, rbDemo, rbSite, rbTrajectory, MovingController, panel_road, slSpeed, rbRoad, rbPlane, chkShowVoronoi)

    def processThenFocusPanel(fun: => Unit): Unit = {
      fun
      panel_road.requestFocus()
    }

    reactions += {
      case ButtonClicked(b) if b == btnReset => processThenFocusPanel {
        resetDemoData()
      }
      case ButtonClicked(b) if b == btnSave => processThenFocusPanel {
        RoadDemoData.writeRoadNetwork()
        DemoData.writeVoronoi()
      }
      case ButtonClicked(b) if b == btnRead => processThenFocusPanel {
        RoadDemoData.readRoadNetwork()
      }
      case ButtonClicked(b) if b == rbAdd => processThenFocusPanel {
        panel_road.actionState = panel_road.ActionState.Add
      }
      case ButtonClicked(b) if b == chkAuto =>
        DemoData.auto = chkAuto.selected
      case ButtonClicked(b) if b == rbSite => processThenFocusPanel {
        panel_road.actionState = panel_road.ActionState.Site
      }
      case ButtonClicked(b) if b == rbTrajectory => processThenFocusPanel {
        panel_road.actionState = panel_road.ActionState.Trajectory
        MovingController.resetTrajectory()
      }
      case ButtonClicked(b) if b == rbDemo => processThenFocusPanel {
        panel_road.actionState = panel_road.ActionState.Demo
      }
      case ButtonClicked(b) if b == chkShowVoronoi =>
        DemoData.showVoronoi = chkShowVoronoi.selected
        panel_plane.repaint()
      case ButtonClicked(b) if b == rbRoad || b == rbPlane =>
        setCenterPanel()
        repaint()
      case ButtonClicked(b) if b == chkKnn || b == chkNeighbors || b == chkNode || b == chkRoad =>
        PanelDrawingState.setShow(chkRoad.selected, chkNode.selected, chkKnn.selected, chkNeighbors.selected)
        panel_road.repaint()
      case ValueChanged(b) if b == slSpeed =>
        MovingController.stepTime = MovingController.stepTimeMax - slSpeed.value + 5
      case KnnRefreshEvent => processThenFocusPanel {
        refreshKnnTable()
      }
      case ActionStateChangeEvent => processThenFocusPanel {
        refreshActionStateRadioButtons()
      }
    }

    refreshActionStateRadioButtons()
    PanelDrawingState.setShow(chkRoad.selected, chkNode.selected, chkKnn.selected, chkNeighbors.selected)

    RoadDemoData.readRoadNetwork()
    resetDemoData()
  }

  def top = new MainFrame {
    //val font1 = new Font("Menlo", Font.PLAIN, 22)
    //List("Button.font", "TextField.font", "Label.font", "CheckBox.font", "RadioButton.font") foreach (UIManager.put(_, font1))

    title = "Influential Neighbor Set Demo"
    contents = ui
    open()

    override def dispose(): Unit = {
      //DemoData.system.shutdown()
      super.dispose()
    }
  }
}
