years <- seq(from=1987, to=2008)
years_strings <- sapply(years,toString)
fns <- sapply(years_strings, paste, sep="", ".csv.bz2")

# install.packages("RSQLite")
library("RSQLite")
# install.packages("str_pad")
library("stringr")
my_path <- "~/"
setwd(my_path)

database_filename = "Big_v5.sqlite"
ptm <- proc.time()
for (i in seq(length(fns)))
{
  print(fns[[i]])
  my_bz <- bzfile(fns[[i]])
  my_csv <- read.csv(my_bz,header=TRUE)

  my_csv[is.na(my_csv)] <- 0.1234
  my_csv$DepTime <- substr(str_pad(my_csv$DepTime, 4, pad="0"), 1, 2)

  drv <- dbDriver("SQLite")
  db <- dbConnect(drv, dbname = database_filename)

  dbWriteTable(conn = db, name = "flight_info", 
             value = my_csv, row.names = FALSE, append = TRUE)
}
ptm - proc.time()
dbSendQuery(db, "delete from flight_info where DepDelay==0.1234")
dbSendQuery(db, "delete from flight_info where DepTime is '0.'")

# 1a)

file.info(database_filename)

# 2b)
ptm <- proc.time()
x <- fetch(dbSendQuery(db, "select UniqueCarrier, Origin, 
                       Dest, Month, DayOfWeek, 
                       DepTime,
                       SUM(CASE WHEN DepDelay > 60 THEN 1 ELSE 0 END) as DelayedCounts, 
                       Count(*) as TotalFlightCounts,
                       CAST(SUM(CASE WHEN DepDelay > 60 THEN 1.0 ELSE 0.0 END) AS FLOAT) / Count(*) as DelayFraction
                       from flight_info group by
                       UniqueCarrier, Origin, Dest, Month, DayOfWeek, DepTime
                       order by DelayFraction desc"),n=-1)
y <- fetch(dbSendQuery(db, "select UniqueCarrier, Origin, 
                       Dest, Month, DayOfWeek, 
                       DepTime,
                       SUM(CASE WHEN DepDelay > 90 THEN 1 ELSE 0 END) as DelayedCounts, 
                       Count(*) as TotalFlightCounts,
                       CAST(SUM(CASE WHEN DepDelay > 90 THEN 1.0 ELSE 0.0 END) AS FLOAT) / Count(*) as DelayFraction
                       from flight_info group by
                       UniqueCarrier, Origin, Dest, Month, DayOfWeek, DepTime
                       order by DelayFraction desc"),n=-1)

z <- fetch(dbSendQuery(db, "select UniqueCarrier, Origin, 
                       Dest, Month, DayOfWeek, 
                       DepTime,
                       SUM(CASE WHEN DepDelay > 180 THEN 1 ELSE 0 END) as DelayedCounts, 
                       Count(*) as TotalFlightCounts,
                       CAST(SUM(CASE WHEN DepDelay > 180 THEN 1.0 ELSE 0.0 END) AS FLOAT) / Count(*) as DelayFraction
                       from flight_info group by
                       UniqueCarrier, Origin, Dest, Month, DayOfWeek, DepTime
                       order by DelayFraction desc"),n=-1)
proc.time() - ptm

head(x,n=10)
head(y,n=10)
head(z,n=10)
# 2d)

dbSendQuery(db, "create index delay_index on flight_info
            (UniqueCarrier, Origin, Dest, Month, 
            DayOfWeek, DepTime)")
# dbSendQuery(db, "drop index delay_index")

ptm <- proc.time()
x <- fetch(dbSendQuery(db, "select UniqueCarrier, Origin, 
                       Dest, Month, DayOfWeek, 
                       DepTime,
                       SUM(CASE WHEN DepDelay > 60 THEN 1 ELSE 0 END) as DelayedCounts, 
                       Count(*) as TotalFlightCounts,
                       CAST(SUM(CASE WHEN DepDelay > 60 THEN 1.0 ELSE 0.0 END) AS FLOAT) / Count(*) as DelayFraction
                       from flight_info group by
                       UniqueCarrier, Origin, Dest, Month, DayOfWeek, DepTime
                       order by DelayFraction desc"),n=-1)

y <- fetch(dbSendQuery(db, "select UniqueCarrier, Origin, 
                       Dest, Month, DayOfWeek, 
                       DepTime,
                       SUM(CASE WHEN DepDelay > 90 THEN 1 ELSE 0 END) as DelayedCounts, 
                       Count(*) as TotalFlightCounts,
                       CAST(SUM(CASE WHEN DepDelay > 90 THEN 1.0 ELSE 0.0 END) AS FLOAT) / Count(*) as DelayFraction
                       from flight_info group by
                       UniqueCarrier, Origin, Dest, Month, DayOfWeek, DepTime
                       order by DelayFraction desc"),n=-1)

z <- fetch(dbSendQuery(db, "select UniqueCarrier, Origin, 
                       Dest, Month, DayOfWeek, 
                       DepTime,
                       SUM(CASE WHEN DepDelay > 180 THEN 1 ELSE 0 END) as DelayedCounts, 
                       Count(*) as TotalFlightCounts,
                       CAST(SUM(CASE WHEN DepDelay > 180 THEN 1.0 ELSE 0.0 END) AS FLOAT) / Count(*) as DelayFraction
                       from flight_info group by
                       UniqueCarrier, Origin, Dest, Month, DayOfWeek, DepTime
                       order by DelayFraction desc"),n=-1)
proc.time() - ptm

# 2e)

xb <- subset(x, TotalFlightCounts > 149)
yb <- subset(y, TotalFlightCounts > 149)
zb <- subset(z, TotalFlightCounts > 149)

head(xb,n=10)
head(yb,n=10)
head(zb,n=10)

# 3)
# install.packages("parallel")
library("parallel")

getDelays <- function(x,s) {
  # print(x)
  s <- toString(s)
  # print(s)
  drv <- dbDriver("SQLite")
  db <- dbConnect(drv, dbname = "Big_v4.sqlite")
  query <- sprintf("select UniqueCarrier, Origin, 
                       Dest, Month, DayOfWeek, 
                       DepTime,
                       SUM(CASE WHEN DepDelay > %i THEN 1 ELSE 0 END) as DelayedCounts, 
                       Count(*) as TotalFlightCounts,
                       CAST(SUM(CASE WHEN DepDelay > %i THEN 1.0 ELSE 0.0 END) AS FLOAT) / Count(*) as DelayFraction
                       from flight_info where Origin like '%s%%'  group by
                       UniqueCarrier, Origin, Dest, Month, DayOfWeek, DepTime
                       order by DelayFraction desc", x, x, s)
  tmp <- fetch(dbSendQuery(db, query),n=-1)
  return(tmp)
}

alphabet = c("A","B","C","D","E","F","G","H","I","J","K","L",
             "M","N","O","P","Q","R","S","T","U","V","W","X",
             "Y","Z")
times <- c(60, 90, 180)
tlc <- expand.grid(times,alphabet)
names(tlc) <- c("x","s")

ptm <- proc.time()
question_3 <- mcmapply(getDelays, tlc$x, tlc$s, mc.cores=4)
proc.time() - ptm
