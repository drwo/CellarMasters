shinyServer(function(input, output, session) {
  # print(test.run)
  
  loginModal <- function(failed = FALSE) {
    modalDialog(
      textInput("user.id", "Enter user ID and password",
                placeholder = "user ID"),
      passwordInput("password", label = "", placeholder = "password"),
       if (failed)
        div(tags$b("Invalid user ID or password", style = "color: red;")),
      footer = tagList(
        actionButton("exit", "Cancel"),
        actionButton("ok", "OK")
      )
    )
  }

  showModal(loginModal())

  observeEvent(input$exit, {stopApp()})

  source("./sources/serve.browseTab.R", local = T)
  source("./sources/serve.addWinesTab.R", local = T)
  source("./sources/serve.manageTab.R", local = T)
  
  observeEvent(input$ok, {
    if (db.valid.login(input$user.id, input$password)) {
      cm <- init.cm(input$user.id, input$password)
      init.browsing()
      removeModal()
    }
    else {
      showModal(loginModal(failed = TRUE))
    }
  })

  observeEvent(input$navbar.page, {
    if (input$navbar.page == "browse.tab") {
      # print("browse.tab")
      # keep this if statement. When the app starts up the first thing that happens in ui.R
      # is the navbar page is displayed for which the first tab dislpayed is the browse.tab
      # this check ensures that no wines are displayed until there is a valid log in
      if (cm$logged.in) {
        init.browsing()
      }
    } else if (input$navbar.page == "addWines.tab") {
      # print("addWines.tab")
      init.add.wines()
    }
    else {
      # must be manage tab
       init.manage.wines()
    }
  })
  
  # cm <- init.cm("drwo@woteki.com", "Wine!Warrior")
})

