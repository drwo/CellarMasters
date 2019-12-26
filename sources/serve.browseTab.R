
# the cellar master logged in for this session
# initialized upon login using fetch.cm()
cm <- reactiveValues(
  login.name = NULL,
  passphrase = NULL,
  name = NULL,
  surname = NULL,
  id = NULL,
  logged.in = F
)

# the cellar associated with the cellarmaster cm
cellar <- reactiveValues(
  init = FALSE,
  # the cellarmaster referenced here for convenience
  cm = NULL,
  # the cm's complete cellar represented as a tibble
  # initialized upon valid login using db.fetch.cellar()
  # must be kept up to date as wines are modified, added or deleted
  all = NULL,
  # the functio that detrmines the value of view below
  view.f = NULL,
  # the current list, or subset, of "all.wines" from which "selected"  or other operations below are being made
  view = NULL,
  # the subset of "view" that is currently selected in the browse tab
  # must be updated as selections are made from user interface
  selected = NULL,
  # the specific wine that is currently being modified or otherwise manipulated
  wine = NULL,
  # the row corresponding to the position of "wine" in either "all", "view" or "selected" depending on context
  row = 0
)

# initializes cm after valid login
init.cm <- function(login.name, passphrase) {
  user <- db.fetch.cm(login.name, passphrase)
  cm$login.name = login.name
  cm$passphrase = passphrase
  cm$name = user$first.name
  cm$surname = user$surname
  cm$id = user$ID
  cm$logged.in = T
  cm
}

# initializes the cm's cellar after login
init.cellar <- function(cm) {
  cellar$cm <- cm
  cellar$all <- db.fetch.cellar(cm)
  cellar$view.f <- open.stock.view
  cellar$view <- open.stock.view(cellar$all)
  cellar$selected <- cellar$view
  cellar$wine <- cellar$selected[1,]
  cellar$row = 1
  cellar$init = T
  cellar
}

init.browsing <- function() {
  cellar <- init.cellar(cm)
  output$browse.table <- render.browse.table()
  update.select.inputs(cellar)
  update.view.inputs(cellar)
  update.wine.info()
  update.details.info()
}

open.stock.view <- function(cellar.all) {
  cellar.all %>%
    filter(Num > 0 & Location != "Schneider's") %>%
    arrange(Producer, Vint, Wine)
}

schneider.view <- function(cellar.all) {
  loc <- grep("Schneider", cellar.all$Location, value = T)
  cellar.all %>%
    filter(Location %in% loc) %>%
    arrange(Producer, Vint, Wine)
}

out.of.stock.view <- function(cellar.all) {
  cellar.all %>%
    filter(Num == 0) %>%
    arrange(Producer, Vint, Wine)
}

# return the browseTab view of the current selection
browse.table <- function(cellar) {
  cellar$selected %>%
    select(Num, Vint, Producer, Wine, Tasted, Location, Rating) %>%
    arrange(Producer)
}

# render "browse.table"
render.browse.table <- reactive({
  renderDT(browse.table(cellar), 
           selection = list(mode = 'single', selected = c(1), target = 'row'),
           options = list(pageLength = 10, 
                          lengthMenu = c('3', '5', '10', '25', 'All'),
                          stateSave = TRUE
           ))
})

# return a list of all the producers represented in the given cellar
# if add.hdr is true insert a header at the top of the list for purposes such as selectInput 
cm.producers <- function(cellar, add.hdr = F, hdr = "") {
  x <- cellar$all %>%
    select(Producer) %>%
    distinct() %>%
    arrange(Producer)
  if (add.hdr) {
    x <- x %>%
      add_row(Producer = hdr, .before = 1)
  }
  x
}

cm.origins <- function(cellar, add.hdr = F, hdr = "All") {
  x <- cellar$all %>%
    select(Origin) %>%
    distinct() %>%
    arrange(Origin)
  if (add.hdr) {
    x <- x %>%
      add_row(Origin = hdr, .before = 1)
  }
  x
}

cm.appellations <- function(cellar, add.hdr = F, hdr = "") {
  x <- cellar$all %>%
    select(Appellation) %>%
    distinct() %>%
    arrange(Appellation)
  if (add.hdr) {
    x <- x %>%
      add_row(Appellation = hdr, .before = 1)
  }
  x
}

cm.varietals <- function(cellar, add.hdr = F, hdr = "") {
  x <- cellar$all %>%
    select(Varietal) %>%
    distinct() %>%
    arrange(Varietal)
  if (add.hdr) {
    x <- x %>%
      add_row(Varietal = hdr, .before = 1)
  }
  x
}

cm.wines <- function(cellar, add.hdr = F, hdr = NULL) {
  x <- cellar$all %>%
    select(Wine) %>%
    distinct() %>%
    arrange(Wine)
  if (add.hdr) {
    x <- x %>%
      add_row(Wine = hdr, .before = 1)
  }
  x
}
update.select.inputs <- function(cellar) {
  updateSelectInput(session, "producers", "Producers", cm.producers(cellar, add.hdr = T, hdr = "All"))
  updateSelectInput(session, "origins", "Origins", cm.origins(cellar, add.hdr = T, hdr = "All"))
  updateSelectInput(session, "appellations", "Appellations", cm.appellations(cellar, add.hdr = T, hdr = "All"))
  updateSelectInput(session, "varietals", "Varietals", cm.varietals(cellar, add.hdr = T, hdr = "All"))
}

update.view.inputs <- function(cellar) {
  updateSelectInput(session, "select.view", "Views", c("Open Stock", "At Schneider's", "Out of Stock"))
}

apply.list.selections <- function(producer = "All", origin = "All", appellation = "All", varietal = "All") {
  
  filter.column <- function(list, col.name, value) {
    if (length(value) == 0 | value == "All" | is.null(value))
      list
    else
      list %>% filter(.[col.name] == value)
  }
  
  selected <- cellar$view %>% 
    filter.column("Origin", origin) %>%
    filter.column("Producer", producer) %>%
    filter.column("Appellation", appellation) %>%
    filter.column("Varietal", varietal)
  cellar$selected <- selected
}

update.wine.info <- function() {
  output$selected.wine <- renderText({paste(cellar$wine$Vint,
                                            cellar$wine$Producer, paste0("'", cellar$wine$Wine, "'"),
                                            "\tVarietal:", cellar$wine$Varietal,
                                            "\tSize:", cellar$wine$Size)})
  updateTextAreaInput(session, "Notes", value = cellar$wine$Notes)
  updateNumericInput(session, "Num", value = cellar$wine$Num)
  # print(paste("update.wine.info Tasted=", cellar$wine$Tasted))
  # print(paste("update.wine.info Tasted=", cellar$wine$Tasted, "as.Date=", as.Date(cellar$wine$Tasted)))
  # updateDateInput(session, "Tasted", value = ymd(cellar$wine$Tasted))
  if (length(cellar$wine$Tasted) & !is.na(cellar$wine$Tasted)) {
    # print(paste("updateTextInput Tasted=", cellar$wine$Tasted, "class=", class(cellar$wine$Tasted)))
    cellar$wine$Tasted <- ymd(cellar$wine$Tasted)
    updateTextInput(session, "Tasted", value = format(cellar$wine$Tasted, format = "%b %d, %Y"))
  }
  # print("update complete")
  else {
    updateTextInput(session, "Tasted", value = "")
  }
  updateTextInput(session, "Location", value = cellar$wine$Location)
  if (length(cellar$wine$Rating) & !is.na(cellar$wine$Rating)) {
    updateTextInput(session, "Rating", value = cellar$wine$Rating)
  }
  else {
    updateTextInput(session, "Rating", value = "")
  }
  output$details <- renderText({paste("Origin:", cellar$wine$Origin,
                                      "Appellation:", cellar$wine$Appellation,
                                      # "Classification:", cellar$wine$Appellation,
                                      "Source:", cellar$wine$Source)})
 
 }

update.details.info <- function() {
  line1 <- paste(cellar$wine$Vint,
                 cellar$wine$Producer, paste0("'", cellar$wine$Wine, "'"),
                 "\tVarietal:", cellar$wine$Varietal,
                 "\tSize:", cellar$wine$Size)
  line2 <- paste("\n\nOrigin:", cellar$wine$Origin, "\tAppellation:", cellar$wine$Appellation)
  line3 <- paste("\n\nPurchased:", cellar$wine$Purchased, "\tPrice:", paste0("$", format(cellar$wine$Price, small.mark = ".", digits = 2)), "\tSource:", cellar$wine$Source)
  txt <- paste(line1, line2, line3)
  output$detailed.wine <- renderText({txt})
}

re.row.selected <- reactive({
  # print("row.selected")
  update.wine.info()
  update.details.info()}, 
  label = "re.row.selected")

update.view.summary <- function() {
  output$view.summary <- renderText({paste("Viewing", nrow(cellar$selected), "wines totaling", sum(cellar$selected$Num), "bottles")})
  output$details.summary <- renderText({paste("Viewing", nrow(cellar$selected), "wines totaling", sum(cellar$selected$Num), "bottles")})
}


re.view.changed <- reactive(
  {
    # print("view.changed")
    update.view.summary()
  }, 
  label = "re.view.changed")

observeEvent(input$browse.table_rows_selected, {
  cellar$row <- as.integer(input$browse.table_rows_selected[1])
  cellar$wine <- cellar$selected[cellar$row,]
  re.row.selected()
})


# this is intended to process page changes 
observeEvent(input$browse.table_state, {
   # row.state is the 0-based index of the first row on the currently displayed page
  row.state <- as.integer(input$browse.table_state[2])
  # page length
  pl <- as.integer(input$browse.table_state[3])
  row <- pl*(row.state %/% pl) + (row.state %% pl) + 1
  pg <- row %/% pl
})

observeEvent(input$filter.list, {
  apply.list.selections(producer = input$producers,
                        origin = input$origins,
                        appellation = input$appellations,
                        varietal = input$varietals)
  cellar$wine <- cellar$selected[1,]
  cellar$row = 1
  output$browse.table <- render.browse.table()
  re.row.selected()
  re.view.changed()
})

observeEvent(input$cancel.filter.list, {
  cellar$selected <- cellar$view
  update.select.inputs(cellar)
  cellar$wine <- cellar$selected[1,]
  cellar$row = 1
  output$browse.table <- render.browse.table()
})

observeEvent(input$select.view, {
  if (input$select.view != "" & !is.na(input$select.view)) {
    if (input$select.view == "At Schneider's") {
      cellar$view <- schneider.view(cellar$all)
      cellar$view.f <- schneider.view
    } else if (input$select.view == "Out of Stock"){
      cellar$view <- out.of.stock.view(cellar$all)
      cellar$view.f <- out.of.stock.view
    } else {
      # default is Open Stock
      cellar$view <- open.stock.view(cellar$all)
      cellar$view.f <- open.stock.view
    }
    cellar$selected <- cellar$view
    cellar$wine <- cellar$selected[1,]
    cellar$row = 1
    output$browse.table <- render.browse.table()
    re.row.selected()
    re.view.changed()
  }
})

observeEvent(input$update.wine.info, {
  
  badDateModal <- function(failed = FALSE) {
    modalDialog(
      renderText({"Invalid date entry. Use format: Sep 24, 2017. Or leave blank"})
    )
  }
  
  # print("input$update.wine.info")
  new.notes <- input$Notes
  new.num <- input$Num
  new.location <- input$Location
  if (input$Tasted == "") {
    new.tasted <- mdy(input$Tasted, quiet = T)
  }
  else {
    # print(paste("for new.tasted =", input$Tasted))
    new.tasted <- as.character(mdy(input$Tasted, quiet = T))
    # print(paste("new.tasted =", new.tasted))
    if (is.na(new.tasted)) {
      showModal(badDateModal())
    }
  }
  new.rating <- input$Rating
  # print("updating notes db")
  db.update.wine.info(cellar$wine$ID, new.num, new.notes, new.tasted, new.location, new.rating)
  # print(paste("updating row notes", cellar$row))
  cellar$selected[cellar$row, "Notes"] <- new.notes
  # print(paste("updating num btls", cellar$row))
  cellar$selected[cellar$row, "Num"] <- new.num
  # print(paste("updating Tasted new.tasted=", new.tasted))
  cellar$selected[cellar$row, "Tasted"] <- new.tasted
  cellar$selected[cellar$row, "Location"] <- new.location
  cellar$selected[cellar$row, "Rating"] <- new.rating
  output$browse.table <- render.browse.table()
  # print(paste("cellar$cm =", cellar$cm))
  print(paste("cellar$cm$id =", cellar$cm$id))
  cellar$all <- db.fetch.cellar(cellar$cm)
  cellar$view <- cellar$view.f(cellar$all)
  # print(paste("done update, cellar$row=", cellar$row))
})

observeEvent(input$cancel.update.wine.info, {
  re.row.selected()
})

observeEvent(input$browse.navbar.page, {
  if (input$browse.navbar.page == "notes.tab") {
    # print("browsing")
  }
  else if (input$browse.navbar.page == "details.tab") {
    # print("details")
  }
})