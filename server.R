shinyServer(function(input, output, session) {
  # print(test.run)
  
  # loginModal <- function(failed = FALSE) {
  #   modalDialog(
  #     textInput("user.id", "Enter user ID and password",
  #               placeholder = "user ID"),
  #     passwordInput("password", label = "", placeholder = "password"),
  #      if (failed)
  #       div(tags$b("Invalid user ID or password", style = "color: red;")),
  # 
  #     footer = tagList(
  #       actionButton("exit", "Cancel"),
  #       actionButton("ok", "OK")
  #     )
  #   )
  # }
  # 
  # showModal(loginModal())
  # 
  # observeEvent(input$exit, {stopApp()})
  # 
  source("./sources/serve.browseTab.R", local = T)
  source("./sources/serve.addWinesTab.R", local = T)
  source("./sources/serve.manageTab.R", local = T)
  
  # observeEvent(input$ok, {
  #   if (test.run | db.valid.login(input$user.id, input$password)) {
  #     cm <- init.cm("drwo@woteki.com", "Wine!Warrior")
  #     init.browsing()
  #     removeModal()
  #   }
  #   else {
  #     showModal(loginModal(failed = TRUE))
  #   }
  # })

      # cm <- init.cm("drwo@woteki.com", "Wine!Warrior")
      # init.browsing()
  
  
  observeEvent(input$navbar.page, {
    if (!cm$logged.in) {
      init.cm("drwo@woteki.com", "Wine!Warrior")
    }
    if (input$navbar.page == "browse.tab") {
      # print("browse.tab")
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
  
})

