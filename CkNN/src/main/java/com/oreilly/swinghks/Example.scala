package com.oreilly.swinghks

import swing._
import swing.event.{TableRowsSelected, TableEvent, TableColumnsSelected, ButtonClicked}

case class TableColumnHeaderSelected(override val source:Table, column: Int) extends TableEvent(source)

object Example extends SimpleSwingApplication {
  setSystemLookAndFeel()

  def top = new MainFrame {
    title = "Scala Swing Table Selection Example"
    contents = ui
  }

  val headers = Array.tabulate(10) {"Col-" + _}.toSeq
  val rowData = Array.tabulate[Any](10, 10) {"" + _ + ":" + _}

  lazy val ui = new BoxPanel(Orientation.Vertical) {
    val output = new TextArea(6, 40) { editable = false }
    val table  = new Table(rowData, headers) {
      selection.elementMode = Table.ElementMode.Cell
      //selection.intervalMode = Table.IntervalMode.Single

      val header = {
        import java.awt.event.{MouseEvent, MouseAdapter}

        val makeHeaderEvent = TableColumnHeaderSelected(this, _:Int)
        val tableHeader = peer.getTableHeader
        tableHeader.addMouseListener(new MouseAdapter() {
          override def mouseClicked(e: MouseEvent) {
            selection.publish(makeHeaderEvent(tableHeader.columnAtPoint(e.getPoint)))
          }
        })
        tableHeader
      }
    }

    listenTo(table.selection)

    reactions += {
      case TableRowsSelected(source, range, false) =>
        outputSelection(source, "Rows selected, changes: %s" format range)
      case TableColumnsSelected(source, range, false) =>
        outputSelection(source, "Columns selected, changes: %s" format range)
      case TableColumnHeaderSelected(source, column) =>
        outputSelection(source, "Column header %s selected" format column)
      case e => println("%s => %s" format(e.getClass.getSimpleName, e.toString))
    }

    contents += new ScrollPane(table)
    contents += new ScrollPane(output)

    def outputSelection(table: Table, msg: String) {
      val rowId = table.selection.rows.leadIndex
      val colId = table.selection.columns.leadIndex
      val rows = table.selection.rows.mkString(", ")
      val cols = table.selection.columns.mkString(", ")
      output.append("%s\n  Lead: %s, %s; Rows: %s; Columns: %s\n" format (msg, rowId, colId, rows, cols))
    }
  }

  def setSystemLookAndFeel() {
    import javax.swing.UIManager
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  }
}