update.add.wine.inputs <- function(cellar) {
  # print("update.add.wine.inputs")
  updateSelectInput(session, inputId = "add.bottles", selected = 12)
  updateSelectInput(session, inputId = "add.vintage", selected = 2018)
  updateSelectInput(session, inputId = "add.producer", choices = db.fetch.producers())
  updateSelectInput(session, inputId = "add.wine", choices = db.fetch.names())
  updateSelectInput(session, inputId = "add.varietal", choices = db.fetch.varietals())
  updateSelectInput(session, inputId = "add.origin", choices = db.fetch.origins())
  updateSelectInput(session, inputId = "add.appellation", choices = db.fetch.appellations())
  updateDateInput(session, inputId = "add.date.purchased", value = today())
  updateSelectInput(session, inputId = "add.size", selected = "750 ml")
  updateTextInput(session, inputId = "add.location", placeholder = "Enter location(s)")
}

init.add.wines <- function() {
  # print("init.add.wines")
  update.add.wine.inputs(cellar)
}

confirm.add.wine <-  modalDialog(
  "Add this wine?",
  footer = tagList(
    actionButton(inputId = "confirm.add.wine", label = "Yes"),
    modalButton("No")
  )
)

confirm.location <- modalDialog(
  div(tags$b("Do you want to leave storage location blank?", style = "color: red;")),
  footer = tagList(
    actionButton(inputId = "confirm.location", label = "Yes"),
    modalButton("No")
  )
)

observeEvent(input$save.add.wine, {
  showModal(confirm.add.wine)
})

observeEvent(input$reset.add.wine, {
  # print("reset.add.wine")
  update.add.wine.inputs(cellar)
})


prepare.added.wine <- function() {
  # print(paste("prepare.added.wine", input$add.date.purchased))
  d <- mdy(input$add.date.purchased, quiet = T)
  if (is.na(d)) {
    d <- as.character(input$add.date.purchased)
  } else {
    d <- as.character(d)
  }
  list(n = input$add.bottles,
       vintage = input$add.vintage,
       producer = input$add.producer,
       name = input$add.wine,
       varietal = input$add.varietal,
       origin = input$add.origin,
       appellation = input$add.appellation,
       purchased = d,
       size = input$add.size,
       location = input$add.location)
 }

observeEvent(input$confirm.add.wine, {
  removeModal()
  if (input$add.location == "") {
    showModal(confirm.location)
  }
  else {
    w <- prepare.added.wine()
    db.add.wine(w)
    update.add.wine.inputs(cellar)
  }
})

observeEvent(input$confirm.location, {
  removeModal()
  w <- prepare.added.wine()
  db.add.wine(w)
  update.add.wine.inputs(cellar)
})




