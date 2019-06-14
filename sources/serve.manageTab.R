locations <- function(cellar.master) {
  db.fetch.cellar(cellar.master) %>%
    filter(Num > 0) %>%
    select(Location, Num)
}

# tabulate the number of bottles per location
tab.locations <- function(wine.locations) {
  
  # tidy uo the string that has the location information for a wine
  # eliminate white space and apostrophes
  tidy.locations <- function(locs) {
    locs <- gsub(" ", "", locs)
    locs <- gsub("'", "", locs)
    locs
  }
  
  tibble.locations <- function(locs, num.btls) {
    # str_extract_all returns a list of which we need only the first element
    n <- str_extract_all(locs, "\\d+")[[1]]
    # if length is zero that means, by convention, there is only one location that has all
    # the bottles given by num.btls
    if (length(n) == 0)
      n <- c(num.btls)
    l <- str_extract_all(locs, "[[:alpha:]]+")[[1]]
    tibble(loc = l, num = n) %>%
      mutate(num = as.integer(num))
  }
  
  tab.location <- function(a.loc) {
    # a string showing the possibly several locations where a wine is located
    # along with the number, of the form nXX, at each location
    locs <- a.loc[1]
    # the total number for the several locations
    num.btls <- a.loc[2]
    # remove all white space and apostrophes
    locs <- tidy.locations(locs)
    # locs is now a string of number-locations of the form nXX separated by commas
    # convert locs to a tibble of 2 columns, one being locations the other the correpsonding number
    # pass the total number of bottles in case there is only one location
    locs <- tibble.locations(locs, num.btls) 
    locs
  }
  
  # init the.tabulaton in order to bind rows
  the.tabulation <- tibble(loc = "", num =0)
  for (i in 1:nrow(wine.locations)) {
    x <- tab.location(wine.locations[i,])
    the.tabulation <- the.tabulation %>%
      bind_rows(x)
  }
  # delete the dummy row, group by location and summarize
  the.tabulation %>%
    filter(loc != "") %>%
    group_by(loc) %>%
    summarize(num = as.integer(sum(num))) %>%
    rename(Location = loc, Num = num)
}

init.manage.wines <- function() {
  tl <- locations(cm)
  total.num <- sum(tl$Num)
  tl <- tab.locations(tl)
  tabulated.num <- sum(tl$Num)
  output$crosstab <- renderText({paste("Total #:", total.num,"    ", "Tabulated #:", tabulated.num)})
  output$tab.locations <- renderTable({tl})  
}

update.attribute.inputs <- function() {
  updateTextInput(session, "new.producer", value = "")
  updateTextInput(session, "new.origin", value = "")
  updateTextInput(session, "new.appellation", value = "")
  updateTextInput(session, "new.name", value = "")
  updateTextInput(session, "new.varietal", value = "")
}

observeEvent(input$confirm.add.attributes, {
  # print("adding attributes")
  db.insert.new.attribute("producer", input$new.producer)
  db.insert.new.attribute("origin", input$new.origin)
  db.insert.new.attribute("appellation", input$new.appellation)
  db.insert.new.attribute("variety", input$new.varietal)
  db.insert.new.wine.name(input$new.name)
  removeModal()
  update.attribute.inputs()
})

confirm.add.attributes <-  modalDialog(
  "Add these attributes?",
  footer = tagList(
    actionButton(inputId = "confirm.add.attributes", label = "Yes"),
    modalButton("No")
  )
)

observeEvent(input$add.attributes, {
  showModal(confirm.add.attributes)
})

observeEvent(input$reset.attributes, {
  # print("reset attributes")
  update.attribute.inputs()
})