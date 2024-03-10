install.packages("RSQLite")
library(RSQLite)
library(DBI)

data(package = "mlbench", PimaIndiansDiabetes2)

#nombrar bbdd

db <- "PimaIndians.db"

#crear conexión a la db

con <- dbConnect(SQLite(), dbname = db)
dbListTables(con)

#almacenar el dataset en la db

dbWriteTable(conn = con, name = "Pima", value = PimaIndiansDiabetes2)
dbListTables(con)

#realizando consultas sql en la bbdd
#obtener casos diabetes positivo

query1 <- "SELECT * FROM Pima WHERE diabetes = 'pos'"
resultados <- RSQLite::dbGetQuery(conn = con, statement = query1)

#check
table(resultados$diabetes)

#obtener glucosa > 160 y edad > 50
query2 <- "SELECT * FROM Pima WHERE glucose > 160 and age > 50"
resultados2 <- RSQLite::dbGetQuery(conn = con, statement = query2)

table(resultados2$diabetes)

#obtener solo la glucosa, el numero de embarazos y imc de los casos
#con diabetes positivo y glucosa > 180
query3 <- "SELECT glucose, pregnant, mass FROM Pima WHERE glucose > 180 and diabetes = 'pos'"
resultados3 <- RSQLite::dbGetQuery(conn = con, statement = query3)

summary(resultados3$glucose)

tbl(src = con, "Pima")


#utilizando dbplyr para hacer las mismas queries.
library(dplyr)
library(dbplyr)
q1 <- tbl(src = con, "Pima") %>% filter(diabetes == "pos")
show_query(q1)
resultadosq1 <- q1 %>% collect()

q2 <- tbl(src = con, "Pima") %>% filter(age > 50, glucose > 160)
show_query(q2)
resultadosq2 <- q2 %>% collect()

q3 <- tbl(src = con, "Pima") %>% filter(diabetes == "pos", glucose > 180) %>% 
  select(glucose, pregnant, mass)
show_query(q3)
resultadosq3 <- q3 %>% collect()

class(resultadosq3)



#caso practico sql e dbplyr
library(tidyverse)
library(DBI)

#nombrando la bbdd

db <- "tuberculosis.db"

#crear conexion

con <- dbConnect(SQLite(), dbname = db)

dbWriteTable(conn = con, name = "population", value = population)
dbWriteTable(conn = con, name = "who", value = who)

tbl(con, "population")
tbl(con, "who")

#construir nuestra sentencia SQL
select <- "SELECT who.country, who.year, who.new_sp_f4554, population.population"
from <- "FROM who"
left_join <- "LEFT JOIN population on who.country = population.country and who.year = population.year"
where <- "WHERE (who.country = 'Spain' or who.country = 'Mexico') and (who.year >= 2001 and who.year <= 2008)"

query <- paste(select, from, left_join, where)
query

resultados <- RSQLite::dbGetQuery(con, query)

class(resultados)
resultados

#usando dbplyr

q1 <- tbl(con, "who") %>% 
  filter(country %in% c("Spain", "Mexico"), year >= 2001, year <= 2008) %>% 
  select("country", "year", "new_sp_f4554") %>% 
  left_join(y = tbl(con, "population"), by = c("country", "year")) %>% 
  group_by(country, year) %>% 
  summarise("casos por millón" = new_sp_f4554 / population * 1000000)

show_query(q1)

resultados2 <- q1 %>% collect()
class(resultados2)
resultados2

