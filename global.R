test.run <- T
dev.run <- F

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
  dbClearResult(dbSendStatement(channel, sql))
  dbDisconnect(channel)
}

esc <- function(x) {
  c <- db.connect()
  x <- escape(x, con = c)
  dbDisconnect(c)
  x
}

db.fetch.cm <- function(login.name, passphrase) {
  cm.select <-"select first_name as name, surname, cellar_master_id as id, wine_cellar_name as cellar from cellar_master"
  cm.where <- paste("where login_name = ", esc(login.name), " and passphrase = ", esc(passphrase))
  db.fetch(sql(paste(cm.select, cm.where)))
}

db.fetch.table.pk <- function(table) {
  # fetch the current pk for the given table
  q <- sql(paste("select eo.name as name, eo.pk as pk from eo_pk_table as eo where name = ", esc(table))) 
  as.integer(db.fetch(q)[1,2])
}

db.increment.table.pk <-  function(table) {
  # increment the current pk for the given table
  q <- paste("update eo_pk_table as eo set eo.pk = eo.pk + 1 where eo.name = ", esc(table))
  db.execute(q)
}

db.next.table.pk <- function(table) {
  # fetch the next pk value to use for the given table
  db.fetch.table.pk(table) + 1
}

db.fetch.pk.for.value <- function(table, value) {
  # fetch the pk for the given value in the given table
  
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
  wine.cellar.sql.select <- sql("SELECT w.wine_id AS `ID`,
                                w.LAST_PURCHASE_DATE AS `Purchased`,
                                w.LAST_PURCHASE_PRICE AS `Price`,
                                w.LAST_PURCHASED_FROM AS `Source`,
                                w.LOCATION AS `Location`,
                                w.NUM_BOTTLES AS `Num`,
                                w.RATING AS `Rating`,
                                w.VINTAGE as `Vint`,
                                w.BOTTLE_SIZE as `Size`,
                                appellation.APPELLATION AS `Appellation`,
                                producer.PRODUCER AS `Producer`,
                                origin.ORIGIN AS `Origin`,
                                classification.CLASSIFICATION AS `Classification`,
                                variety.VARIETY AS `Varietal`,
                                winename.NAME AS `Wine`,
                                tastingnote.LAST_TASTING AS `Tasted`,
                                tastingnote.NOTES as `Notes`")
  wine.cellar.sql.from <- sql("FROM wine AS w
                              LEFT JOIN appellation USING (appellation_id)
                              LEFT JOIN producer USING (producer_id)
                              LEFT JOIN origin USING (origin_id)
                              LEFT JOIN classification USING (classification_id)
                              LEFT JOIN variety USING (variety_id)
                              LEFT JOIN winename USING (name_id)
                              LEFT JOIN tastingnote USING (wine_id) ")
  wine.cellar.sql <- sql(paste(wine.cellar.sql.select, wine.cellar.sql.from, "WHERE w.cellar_master_id =", cellar.master$id))
  db.fetch(wine.cellar.sql)
}

db.update.wine.info <- function(wine.id, new.num, new.notes, new.tasted, new.location, new.rating) {
  upd <- paste("update tastingnote set notes = ", esc(new.notes), " where wine_id = ", wine.id)
  # print(upd)
  # print(paste("db.update.wine.info", new.tasted))
  upd <- sql(paste("update wine set num_bottles = ", esc(new.num),
                   ", location = ", esc(new.location),
                   ", rating = ", esc(new.rating),
                   " where wine_id = ", wine.id))
  db.execute(upd)
  upd <- sql(paste("update tastingnote set notes = ", esc(new.notes), 
                   ", last_tasting = ", esc(new.tasted),
                   " where wine_id = ", wine.id))
  db.execute(upd)
} 

db.fetch.table <- function(table, col.name, as.name) {
  col.id <- paste0(col.name, "_ID, ")
  query <- sql(paste("select ", col.id, col.name, " AS `", as.name, "` FROM ", table, sep = ""))
  db.fetch(query)
}

db.fetch.tbl <- function(table, col.name, as.name) {
  channel <- db.connect()
  t <- tbl(channel, table) %>%
    # use unquote operator !! with assignment :=
    rename(!!as.name := col.name) %>%
    collect() 
  dbDisconnect(channel)
  t
}

db.add.wine <- function(wine) {
  # wine is a list with these fields: n, vintage, producer, name, varietal, origin, appellation, purchased, size & location
  # print(wine)
  pk <- db.next.table.pk("wine")
  q <- paste("insert into wine set") %>%
    paste("wine_id =", pk, ",") %>%
    paste("cellar_master_id =", 1, ",") %>%
    paste("num_bottles = ", wine$n, ",") %>%
    paste("vintage = ", wine$vintage, ",") %>%
    paste("producer_id = (select producer_id from producer as p where p.producer =", esc(wine$producer), "),") %>%
    paste("name_id = (select name_id from winename as wn where wn.name =", esc(wine$name), "),") %>%
    paste("variety_id = (select variety_id from variety as v where v.variety =", esc(wine$varietal), "),") %>%
    paste("origin_id = (select origin_id from origin as o where o.origin =", esc(wine$origin), "),") %>%
    paste("appellation_id = (select appellation_id from appellation as a where a.appellation =", esc(wine$appellation), "),") %>%
    paste("last_purchase_date =", esc(wine$purchased), ",") %>%
    paste("bottle_size =", esc(wine$size), ",") %>%
    paste("location =", esc(wine$location)) %>%
    sql()
  if (db.execute(q)) {
    db.increment.table.pk("wine")
    db.execute(sql(paste("insert into tastingnote set wine_id =", pk, ", notes = NULL, last_tasting = NULL")))
  }
}



db.fetch.producers <- function() {
  db.fetch.tbl("producer", "PRODUCER", "Producer") %>%
    arrange(Producer)
}

db.fetch.origins <- function() {
  db.fetch.tbl("origin", "ORIGIN", "Origin") %>%
    arrange(Origin)
}

db.fetch.appellations <- function() {
  db.fetch.tbl("appellation", "APPELLATION", "Appellation") %>%
    arrange(Appellation)
}

db.fetch.varietals <- function() {
  db.fetch.tbl("variety", "VARIETY", "Varietal") %>%
    arrange(Varietal)
}

db.fetch.names <- function() {
  db.fetch.tbl("winename", "NAME", "Name") %>%
    arrange(Name)
}

db.duplicate.attribute <- function(table, value) {
  query <- sql(paste("SELECT", table, "FROM", table, "WHERE", table, "= ", esc(value)))
  nrow(db.fetch(query)) != 0
}

db.insert.new.attribute <- function(table, attribute) {
  if (db.duplicate.attribute(table, attribute) | attribute == "") {
    # print("returning")
    return(NULL)
  }
  # print("attempting insertion")
  pk <- db.next.table.pk(table)
  table.id <- paste0(table, "_id")
  columns <- paste(table, table.id, sep = ",")
  columns <- paste0("(", columns, ")")
  values <- paste(esc(attribute), pk, sep = ",")
  values <- paste0("(", values, ")")
  q <- sql(paste("insert into", table, columns, "VALUES", values))
  if (ok <- db.execute(q)) {
    db.increment.table.pk(table)
  }
  ok
}

db.insert.new.wine.name <- function(name) {
  query <- sql(paste("SELECT name FROM winename WHERE name =", esc(name)))
  if (nrow(db.fetch(query)) != 0 | name == "") {
    # print("duplicate or no name")
    return(NULL)
  }
  # print("attempting insertion")
  pk <- db.next.table.pk("winename")
  # print(pk)
  q <- sql(paste("INSERT INTO winename (name, name_id) VALUES (", esc(name), ",", pk, ")"))
  if (ok <- db.execute(q)) {
    # print("incrementing pk")
    db.increment.table.pk("winename")
  }
  ok
}
