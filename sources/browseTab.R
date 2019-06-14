notesTab <- tabPanel(title = "Notes",
                     fluidRow(column(12, verbatimTextOutput(outputId = "view.summary"))),
                     fluidRow(column(12, verbatimTextOutput(outputId = "selected.wine"))),
                     fluidRow(
                              column(2, numericInput("Num", "Number", value = 0, min = 0, max = 36, step = 1)),
                              # column(3, dateInput("Tasted", "Date",
                              #                     # min = today(),
                              #                     format = "M d, yyyy",
                              #                     startview = "month" ), offset = 1),
                              column(3, textInput("Tasted", "Last Tasted")),
                              column(3, textInput("Location", "Location")),
                              column(3, textInput("Rating", "Rating"))),
                     fluidRow(column(12, textAreaInput(inputId = "Notes",
                                                       label = "Notes",
                                                       width = "200%",
                                                       rows =10,
                                                       resize = "both"))),
                     fluidRow(column(12, actionButton("update.wine.info", label = "Update Info"),
                                     actionButton("cancel.update.wine.info", label = "Cancel"))),
                     value = "notes.tab"
)
                     

wineDetailsTab <- tabPanel(title="Details",
                           fluidRow(column(12, verbatimTextOutput(outputId = "details.row.click"))),
                           value = "details.tab"
                           )

browseTab <- 
  tabPanel(title = "Browse Cellar",
           sidebarLayout(
             sidebarPanel(
               strong("Select View:"),
               selectInput("select.view", label = NULL, choices = NULL),
               hr(),
               strong("Filter View:"),
               selectInput("producers", "Producers", NULL),
               selectInput("origins", "Origins", NULL),
               selectInput("appellations", "Appellations", NULL),
               selectInput("varietals", "Varietals", NULL),
               actionButton("filter.list", label = "Apply"),
               actionButton("cancel.filter.list", label = "Cancel"),
               width = 3),
             mainPanel(
               fluidRow(column(12, DTOutput(outputId = "browse.table"))),
               fluidRow(column(12, navbarPage(title="Wine", notesTab, wineDetailsTab)))
             )
           ),
           value = "browse.tab"
  )

