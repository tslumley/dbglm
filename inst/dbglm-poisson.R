set.seed(2018-7-7)
df<-data.frame(x=rep(1:1000,10000),z=rnorm(1e7))
df$y<-with(df, rpois(1e7,exp(x/1000+z/1000+1)))

system.time(inmem <- glm(y~x+z, family=poisson(), data=df))

library(MonetDBLite)
library(DBI)
td<-tempdir()
con<-dbConnect(MonetDBLite(),td)
dbWriteTable(con,"simulated", df)
dbDisconnect(con,shutdown=TRUE)

rm(df)

library(dbglm)
library(dplyr)
ms<- MonetDBLite::src_monetdblite(td)

system.time(indb<-dbglm(y~x+z, family=poisson(), tbl=tbl(ms,"simulated")))

dbDisconnect(ms$con,shutdown=TRUE)
