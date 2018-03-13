##
## Download January to June 2016 data from
## https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2016-01.csv
## https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2016-02.csv
## https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2016-03.csv
## https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2016-04.csv
## https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2016-05.csv
## https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2016-06.csv
## and July to December data from
##   https://figshare.com/articles/yellow_tripdata_2016-07trim_csv/5965522
##
## 
## This is the directory you put them in
datadir<-"~/TAXITEST"
##


library(DBI)
library(MonetDBLite)
library(dplyr)
library(dbplyr)

ms <- MonetDBLite::src_monetdblite(paste(datadir,"db",sep="/"))

## read the January to June data
inputs<-list.files(datadir,"-0[1-6]",full.names=TRUE)
monetdb.read.csv(ms$con, inputs, tablename="yellow", header=TRUE, lower.case.names=TRUE)

## read the July to December data (the header on the original file is wrong)
nm<-tolower(strsplit("VendorID,tpep_pickup_datetime,tpep_dropoff_datetime,passenger_count,trip_distance,RatecodeID,store_and_fwd_flag,PULocationID,DOLocationID,payment_type,fare_amount,extra,mta_tax,tip_amount,tolls_amount,improvement_surcharge,total_amount,junk1,junk2",",")[[1]])
inputs<-list.files(datadir,"trim",full.names=TRUE)
 monetdb.read.csv(ms$con, inputs, tablename="yellowlate",col.names=nm,header=FALSE)

## Combine the two data sets

dbSendQuery(ms$con, "create table taxis as (select vendorid, tpep_pickup_datetime, tpep_dropoff_datetime, passenger_count, trip_distance, ratecodeid, store_and_fwd_flag,  payment_type, fare_amount, extra, mta_tax, tip_amount, tolls_amount, improvement_surcharge, total_amount from yellow union all select vendorid, tpep_pickup_datetime, tpep_dropoff_datetime, passenger_count, trip_distance, ratecodeid, store_and_fwd_flag, payment_type, fare_amount, extra, mta_tax, tip_amount, tolls_amount, improvement_surcharge, total_amount from yellowlate)")

## Create some additional variables

dbSendQuery(ms$con, "alter table taxis add column dropoff_timestamp TIMESTAMP")
dbSendQuery(ms$con, "update taxis set dropoff_timestamp = str_to_timestamp(tpep_dropoff_datetime,  '%Y-%m-%d %H:%M:%S')")
dbSendQuery(ms$con, "alter table taxis add column drop_hour FLOAT")
dbSendQuery(ms$con, "update taxis set drop_hour = extract( hour from dropoff_timestamp)")
dbSendQuery(ms$con, "alter table taxis add column drop_dow FLOAT")
dbSendQuery(ms$con, "update taxis set drop_dow = dayofweek(dropoff_timestamp)")

## Subset and create more variables

taxis <- tbl(ms, "taxis")
cc_taxis <- taxis %>% filter(payment_type==1) %>% filter(fare_amount>0) %>% filter(trip_distance < 50) %>% filter(ratecodeid!=99)%>% mutate(bad_tip=ifelse(tip_amount/(fare_amount+improvement_surcharge)<0.20,1.0,0.0))%>% mutate(night=ifelse(drop_hour<5| drop_hour>7,1.0,0)) %>% mutate(weekend=ifelse(drop_dow==1 | drop_dow==7 | (drop_dow==6 & drop_hour>7),1,0)) %>% compute()

## The actual model fitting

system.time({
model0<-dbglm(bad_tip~factor(weekend)*night+trip_distance+passenger_count+factor(ratecodeid),tbl=cc_taxis)	
})

system.time({
model1<-dbglm(bad_tip~factor(weekend)*factor(night)*passenger_count+trip_distance++factor(ratecodeid),tbl=cc_taxis,family=binomial)	
})


## Comparison with bigglm

library(biglm)

system.time({
model0biglm<-bigglm(bad_tip~weekend*night*passenger_count+trip_distance+factor(ratecodeid,levels=1:5),data=cc_taxis$src$con,family=binomial(),tablename="xryqxldcdd",chunksize=4e5)	
})

system.time({
model1biglm<-bigglm(bad_tip~weekend*night*passenger_count+trip_distance+factor(ratecodeid,levels=1:5),data=cc_taxis$src$con,family=binomial(),tablename="xryqxldcdd",chunksize=4e5)	
})

