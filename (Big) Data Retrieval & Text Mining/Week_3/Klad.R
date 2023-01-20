# libs
library('RSQLite')


conn <- DBI::dbConnect(
        RSQLite::SQLite(),
        ":memory:")


library(nycflights13)

flights <- nycflights13::flights
planes <- nycflights13::planes

DBI::dbCreateTable(conn, 'flights', flights) 
dbReadTable(conn, "flights")
dbAppendTable(conn, 'flights', flights)

DBI::dbCreateTable(conn, 'planes', planes) 
dbReadTable(conn, "planes")
dbAppendTable(conn, 'planes', planes)
dbReadTable(conn, "planes")



sql <- 'select avg(dep_delay), month as avg_dep_delay from flights where dep_delay > 2 group by month'


sql2 <- 'select avg(f.dep_delay), p.year
          from flights f
          inner join planes p 
          on f.tailnum = p.tailnum
          group by p.year'


DBI::dbGetQuery(conn, sql2)
