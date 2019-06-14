tabulateTab <- tabPanel(title = "Locations",
                        fluidRow(column(width = 4, verbatimTextOutput("crosstab"))),
                        fluidRow(column(width = 12, tableOutput("tab.locations"))))

attributesTab <- tabPanel(title = "Attributes",
                          fluidRow(column(width = 4, selectInput("producer.list", "Current Producers:", choices = db.fetch.producers())),
                                   column(width = 4, textInput("new.producer", "Add Producer:"))),
                          fluidRow(column(width = 4, selectInput("origin.list", "Current Origns:", choices = db.fetch.origins())),
                                   column(width = 4, textInput("new.origin", "Add Origin:"))),
                          fluidRow(column(width = 4, selectInput("appellations.list", "Current Appellations:", choices = db.fetch.appellations())),
                                   column(width = 4, textInput("new.appellation", "Add Appellation:"))),
                          fluidRow(column(width = 4, selectInput("names.list", "Current Names:", choices = db.fetch.names())),
                                   column(width = 4, textInput("new.name", "Add name:"))),
                          fluidRow(column(width = 4, selectInput("varietal.list", "Current Varietals:", choices = db.fetch.varietals())),
                                   column(width = 4, textInput("new.varietal", "Add Varietal:"))),
                          fluidRow(column(width = 4, actionButton("reset.attributes", "Reset All")),
                                   column(width = 4, actionButton("add.attributes", "Add All"))))

manageTab <- tabPanel(title = "Manage Cellar",
                      navbarPage(title = NULL,
                                 tabulateTab,
                                 attributesTab),
                      value = "manage.tab"
                      )

