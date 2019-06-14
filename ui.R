# source("./sources/sql.R", local = T)
source("./sources/browseTab.R", local = T)
source("./sources/manageTab.R", local = T)
source("./sources/addWinesTab.R", local = T)


navbarPage(title="Cellar Masters",
           browseTab,
           addWinesTab,
           manageTab,
           id = "navbar.page")


