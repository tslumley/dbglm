##
## Download data from figshare
## https://figshare.com/articles/NZ_vehicles_database/5971471
##
### Code for MonetDB

library(dbglm)
library(DBI)
library(MonetDBLite)
library(dplyr)
library(dbplyr)

ms <- MonetDBLite::src_monetdblite("~/VEHICLE")
monetdb.read.csv(ms$con, "Fleet30Nov2017.csv",tablename="vehicles",quote="",nrow.check=10000,best.effort=TRUE,lower.case.names=TRUE)
vehicles<-tbl(ms,"vehicles")
cars <- filter(vehicles, vehicle_type == "PASSENGER CAR/VAN") %>% 
	mutate(isred=ifelse(basic_colour=="RED",1,0)) %>% 
	filter(number_of_seats >1 & number_of_seats < 7) %>% filter(number_of_axles==2) %>%
	compute()

system.time({
model<-dbglm(isred~power_rating+number_of_seats+gross_vehicle_mass,tbl=cars)
})


### Code for SQLite
library(dbglm)
library(RSQLite)
library(dplyr)
library(dbplyr)


vehicles<-read.csv("Fleet30Nov2017.csv")
sqlite<-dbDriver("SQLite")
con<-dbConnect(sqlite,"nzcars.db")
RSQLite:::initExtension(con)
dbWriteTable(con,"vehicles",vehicles)
rm(vehicles)

sqlitevehicles<-tbl(con,"vehicles")


cars <- filter(sqlitevehicles, vehicle_type == "PASSENGER CAR/VAN") %>%
	mutate(isred=ifelse(basic_colour=="RED",1,0)) %>% 
	filter(number_of_seats >1 & number_of_seats < 7) %>% filter(number_of_axles==2) %>%
	compute()

system.time({
sqlitemodel<-dbglm(isred~power_rating+number_of_seats+gross_vehicle_mass,tbl=cars)
})

