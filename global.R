test.run <- T
dev.run <- T

db.connect <- function() {
  if (dev.run) {
    db.name <- "CellarMastersDev"
  } 
  else if (test.run) {
    db.name = "CellarMastersTest"
  }
  else {
    db.name = "CellarMastersPro"
  }
  dbConnect(RMySQL::MySQL(), dbname = db.name, user="mysql", password="Java!Warrior", host="localhost")
}

db.fetch <- function(sql) {
  channel <- db.connect()
  t <- as_tibble(dbGetQuery(channel, sql))
  dbDisconnect(channel)
  t
}

db.execute <- function(sql) {
  channel <- db.connect()
  n <- dbExecute(channel, sql)
  dbDisconnect(channel)
  n
}

esc <- function(x) {
  c <- db.connect()
  x <- escape(x, con = c)
  dbDisconnect(c)
  x
}

db.fetch.cm <- function(login.name, passphrase) {
  con <- db.connect()
  cm <- tbl(con, "cellar_master") %>%
    filter(login.name == login.name & passphrase == passphrase) %>%
    collect()
  # print(paste("db.fetch.cm", cm))
  dbDisconnect(con)
  cm
}

db.fetch.table.pk <- function(table) {
  # fetch the current pk for the given table
  q <- sql(paste("select eo.pk from eo_pk_table as eo where eo.table = ", esc(table))) 
  as.integer(db.fetch(q))
}

db.increment.table.pk <-  function(table) {
  # increment the current pk for the given table
  q <- sql(paste("update eo_pk_table as eo set eo.pk = eo.pk + 1 where eo.table = ", esc(table)))
  db.execute(q)
}

db.next.table.pk <- function(table) {
  # fetch the next pk value to use for the given table
  db.fetch.table.pk(table) + 1
}

db.valid.login <- function(login.name, passphrase) {
  cm <- db.fetch.cm(login.name, passphrase)
  if (nrow(cm) == 0) {
    v <- F
  } else {
    v <- cm$id == 1
  }
  v
}

db.fetch.cellar <- function(cellar.master) {
  con <- db.connect()
  wines <- tbl(con, "wine") %>%
    left_join(tbl(con, "appellation"), by = "APPELLATION_ID") %>%
    left_join(tbl(con, "classification"), by = "CLASSIFICATION_ID") %>%
    left_join(tbl(con, "winename"), by = "NAME_ID") %>%
    left_join(tbl(con, "tastingnote"), by = "WINE_ID") %>%
    left_join(tbl(con, "origin"), by = "ORIGIN_ID") %>%
    left_join(tbl(con, "producer"), by = "PRODUCER_ID") %>%
    left_join(tbl(con, "varietal"), by = "VARIETY_ID") %>%
    collect()
  dbDisconnect(con)
  wines
}

db.update.wine.info <- function(wine.id, new.num, new.notes, new.tasted, new.location, new.rating) {
  upd <- paste("update tastingnote set notes = ", esc(new.notes), " where wine_id = ", wine.id)
  # print(upd)
  # print(paste("db.update.wine.info", new.tasted))
  upd <- sql(paste("update wine set num = ", esc(new.num),
                   ", location = ", esc(new.location),
                   ", rating = ", esc(new.rating),
                   " where wine_id = ", wine.id))
  # print(upd)
  db.execute(upd)
  upd <- sql(paste("update tastingnote set notes = ", esc(new.notes), 
                   ", tasted = ", esc(new.tasted),
                   " where wine_id = ", wine.id))
  db.execute(upd)
} 

db.add.wine <- function(wine) {
  # wine is a list with these fields: n, vintage, producer, name, varietal, origin, appellation, purchased, size, location, source & price
  # print(wine)
  pk <- db.next.table.pk("wine")
  
  wine_id <- pk;
  num <- wine$n
  vint <- wine$vintage
  producer_id <- paste("(select producer_id from producer as p where p.Producer =", esc(wine$producer), ")")
  name_id <- paste("(select name_id from winename as wn where wn.Wine =", esc(wine$name), ")")
  variety_id <- paste("(select variety_id from varietal as v where v.Varietal =", esc(wine$varietal), ")")
  origin_id <- paste("(select origin_id from origin as o where o.Origin =", esc(wine$origin), ")")
  appellation_id <- paste("(select appellation_id from appellation as a where a.Appellation =", esc(wine$appellation), ")")
  purchased <- esc(wine$purchased)
  size <- esc(wine$size)
  location <- esc(wine$location)
  source <- esc(wine$source)
  price <- esc(wine$price)
  
  q <- paste("insert into wine (WINE_ID, Num, Vint, PRODUCER_ID, NAME_ID, VARIETY_ID, ORIGIN_ID, APPELLATION_ID, Purchased, Size, Location, Source, Price)")
  values <- paste(wine_id, num, vint, producer_id, name_id, variety_id, origin_id, appellation_id, purchased, size, location, source, price, sep= ",")
  q <- paste(q, "VALUES (", values,  ")") %>%
    sql()
  
  if (db.execute(q)) {
    db.increment.table.pk("wine")
    db.execute(sql(paste("insert into tastingnote set wine_id =", pk, ", Notes = NULL, Tasted = NULL")))
  }
}


db.fetch.attribute.table <- function(table, attribute) {
  channel <- db.connect()
  t <- tbl(channel, table) %>%
    collect()
  dbDisconnect(channel)
  t
}
db.fetch.producers <- function() {
  db.fetch.attribute.table("producer", "Producer") %>%
    select(Producer) %>%
    arrange(Producer)
}

db.fetch.origins <- function() {
  db.fetch.attribute.table("origin", "Origin") %>%
    select(Origin) %>%
    arrange(Origin)
}

db.fetch.appellations <- function() {
  db.fetch.attribute.table("appellation", "Appellation") %>%
    select(Appellation) %>%
    arrange(Appellation)
}

db.fetch.varietals <- function() {
  db.fetch.attribute.table("varietal", "Varietal") %>%
    select(Varietal) %>%
    arrange(Varietal)
}

db.fetch.names <- function() {
  db.fetch.attribute.table("winename", "Wine") %>%
    select(Wine) %>%
    arrange(Wine)
}

db.duplicate.attribute <- function(table, value) {
  if (table == "winename") {
    field <- "Wine"
  } else {
    field <- table
  }
  query <- sql(paste("SELECT", field, "FROM", table, "WHERE", field, "= ", esc(value)))
  # print(query)
  nrow(db.fetch(query)) != 0
}

db.insert.new.attribute <- function(table, attribute) {
  if (db.duplicate.attribute(table, attribute) | attribute == "") {
    # print(paste("duplicate attribute", attribute))
    return(NULL)
  }
  # print("attempting insertion")
  pk <- db.next.table.pk(table)
  table.id <- paste0(table, "_id")
  columns <- paste0("(", table, ",", table.id, ")")
  attribute <- esc(attribute)
  values <- paste0("(", attribute, ",", pk, ")")
  q <- paste("insert into", table, columns, "VALUES", values) %>%
    sql()
  if (ok <- db.execute(q)) {
    db.increment.table.pk(table)
  }
  ok
}

db.insert.new.wine.name <- function(name) {
  if (db.duplicate.attribute("winename", name) | name == "") {
    # print(paste("duplicate wine name", name))
    return(NULL)
  }
  # print("attempting wine name insertion")
  pk <- db.next.table.pk("winename")
  # print(pk)
  q <- sql(paste("INSERT INTO winename (wine, name_id) VALUES (", esc(name), ",", pk, ")"))
  if (ok <- db.execute(q)) {
    # print("incrementing pk")
    db.increment.table.pk("winename")
  }
  ok
}

