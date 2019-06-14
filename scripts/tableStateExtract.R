if (upd() == "wine.info") {
  # print(paste("table state upd()=", upd(), "row.state=", row.state, "row=", row, "pl=", pl, "pg=", pg))
  # print(paste("cellar$row=", cellar$row, "crow=", crow()))
  # print(paste("selected row=", input$browse.table_rows_selected))
  # print("selecting page")
  selectPage(dataTableProxy("browse.table", session), pg)
  # print("done page")
  # print(paste("selecting row", crow()))
  selectRows(dataTableProxy("browse.table", session), crow())
  # selectRows(dataTableProxy("browse.table", session), 22)
  # print("done row")
  row.state <- as.integer(input$browse.table_state[2])
  # page length
  pl <- as.integer(input$browse.table_state[3])
  row <- pl*(row.state %/% pl) + (row.state %% pl) + 1
  pg <- crow() %/% pl
  # print(paste("table state upd()=", upd(), "row.state=", row.state, "row=", row, "pl=", pl, "pg=", pg))
  # print(paste("cellar$row=", cellar$row, "crow=", crow()))
  # print(paste("selected row=", input$browse.table_rows_selected))
  upd("done.wine.info")
}
else if (upd() == "default") {
  # print(paste("table state upd()=", upd(), "row.state=", row.state, "row=", row, "pl=", pl))
  # print(paste("cellar$row=", cellar$row))
  cellar$wine <- cellar$selected[row,]
  cellar$row = row
  selectRows(dataTableProxy("browse.table", session), row)
  update.wine.info()
}
else if (upd() == "filter.list")  {
  # print(paste("table state upd()=", upd(), "row.state=", row.state, "row=", row, "pl=", pl))
  # print(paste("cellar$row=", cellar$row))
  upd("default")
}
else {
  # print("done.wine.info")
  # print(paste("table state upd()=", upd(), "row.state=", row.state, "row=", row, "pl=", pl, "pg=", pg))
  # print(paste("cellar$row=", cellar$row, "crow=", crow()))
  # print(paste("selected row=", input$browse.table_rows_selected))
  upd("default")
}