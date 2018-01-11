
library(tidypredict)
library(dbplyr)
library(dplyr)
library(MonetDBLite)

   
dbsample<-function(con,...)  {UseMethod("dbsample")}
  

dbsample.tbl_monetdb <-function(tbl, n, N, variables, ... ){
	dbGetQuery(tbl$src$con, build_sql("select ",ident(variables)," from (",sql(sql_render(tbl)), ") as foo sample ", as.integer(n)))
}
 
 
 
dbsample.tbl_df <-function(tbl, n, N, variables, ... ){
	tbl[sample(N,n),]	
}
 
   
dbglm<-function(formula, family = binomial(), tbl, sd=FALSE,weights=.NotYetImplemented(), subset=.NotYetImplemented(), ...){   
	
 
  variables<-all.vars(formula)
  if (!(all(variables %in% colnames(tbl)))) stop("variables must be in data tbl")
  tbl2<-select(tbl,!!!syms(all.vars(formula)))

  N<- pull(summarise(tbl2, n()))
  n<-round(N^(5/9))
  sdf<-dbsample(tbl2, n, N,  variables,...)
  
  model0 <- glm(formula=formula,family=family, data=sdf, ...)
  
  if(sd){
  	  rval <- t(as.matrix(tbl2 %>% score_meansd(model0)))
  	  U <-rval[,1]*N
  	  beta0<-coef(model0)
  	  V0<- vcov(model0)
  	  inf<-solve(summary(model0)$cov.unscaled)
  	  seratio<- sqrt(diag(inf))/(rval[,2]*sqrt(n))
  	  V1<-V0*n/N
  	  V2<- outer(seratio,seratio)*V1
  	  beta1<-beta0+V1%*%U
  	  beta2<-beta0+V2%*%U

  	  list(beta0,beta1,beta2,V1,V2)

  } else {
  		U <- t(as.matrix(tbl2 %>% score_mean(model0)))*N
  		beta0<-coef(model0)
  		V0<- vcov(model0)
  		V1<-vcov(model0)*(n/N)
  		beta1<-beta0+V1%*%U
  		list(beta0,beta1,V0,V1)

  }
}


score_mean<- function(df, model,fitname="_fit_",residname="_resid_") {
	df <- df %>% tidypredict_to_column(model, vars=c(fitname,"",""))
	
  parsedmodel<- parse_model(model)
  labels <- parsedmodel %>%
    filter(labels == "labels") %>%
    as.character()
  
  labels <- labels[4:length(labels)]
  labels <- c("estimate", labels)
  all_terms <- parsedmodel %>%
    filter(.data$type == "term") %>%
    select(- .data$type, -.data$labels)
  
  selection <- which(labels != "NA")
  all_terms <- all_terms[, which(labels != "NA")]
  colnames(all_terms) <- labels[which(labels != "NA")]

  response<-attr(terms(model),"variables")[[2]]
  fit<-sym(fitname)
  
  f <- seq_len(nrow(all_terms)) %>%
    map(~{
      vars <- colnames(all_terms)
      vals <- as.character(all_terms[.x, ])
      
      resid <- expr((!!!response)-(!!!fit))
      
      reg <- vars[vals == "{{:}}" & !is.na(vals) & vars != "estimate"]
      reg <- expr(!! syms(reg))
      
      field <- vars[vals != "{{:}}" & !is.na(vals) & vars != "estimate"]
      val <-  vals[vals != "{{:}}" & !is.na(vals) & vars != "estimate"]
      ie <- map2(syms(field), val, function(x, y) expr((!!x) == (!!y)))
      ie <- map(ie, function(x) expr(ifelse(!!x, 1, 0)))
      set <- c(reg, ie, resid)
      reduce(set, function(l, r) expr((!!! l) * (!!! r)))
    } )
  
  offset <- filter(parsedmodel, labels == "offset")
  if (nrow(offset) > 0) {
    f <- c(f, sym(offset$vals))
  }
  
  df %>% 
  	mutate(!!!map2(paste0("_u",seq_along(coef(model))),f, function(x,y) expr(!!x:=!!y))) %>%
	summarise(!!!map(paste0("_u",seq_along(coef(model))), function(x) expr(mean(!!sym(x))))) %>%
    collect()
}

score_meansd<- function(df, model,fitname="_fit_",residname="_resid_") {
	df <- df %>% tidypredict_to_column(model, vars=c(fitname,"",""))
	
  parsedmodel<- parse_model(model)
  labels <- parsedmodel %>%
    filter(labels == "labels") %>%
    as.character()
  
  labels <- labels[4:length(labels)]
  labels <- c("estimate", labels)
  all_terms <- parsedmodel %>%
    filter(.data$type == "term") %>%
    select(- .data$type, -.data$labels)
  
  selection <- which(labels != "NA")
  all_terms <- all_terms[, which(labels != "NA")]
  colnames(all_terms) <- labels[which(labels != "NA")]

  response<-attr(terms(model),"variables")[[2]]
  fit<-sym(fitname)
  
  f <- seq_len(nrow(all_terms)) %>%
    map(~{
      vars <- colnames(all_terms)
      vals <- as.character(all_terms[.x, ])
      
      resid <- expr((!!!response)-(!!!fit))
      
      reg <- vars[vals == "{{:}}" & !is.na(vals) & vars != "estimate"]
      reg <- expr(!! syms(reg))
      
      field <- vars[vals != "{{:}}" & !is.na(vals) & vars != "estimate"]
      val <-  vals[vals != "{{:}}" & !is.na(vals) & vars != "estimate"]
      ie <- map2(syms(field), val, function(x, y) expr((!!x) == (!!y)))
      ie <- map(ie, function(x) expr(ifelse(!!x, 1, 0)))
      set <- c(reg, ie, resid)
      reduce(set, function(l, r) expr((!!! l) * (!!! r)))
    } )
  
  offset <- filter(parsedmodel, labels == "offset")
  if (nrow(offset) > 0) {
    f <- c(f, sym(offset$vals))
  }
  
  rval <- df %>% 
  	mutate(!!!map2(paste0("_u",seq_along(coef(model))), f, function(x,y) expr(!!x:=!!y))) %>%
	summarise(!!!flatten(map(paste0("_u", seq_along(coef(model))), 
			function(x) c(expr(mean(!!sym(x))),expr(sd(!!sym(x))))))) %>% 
			collect()
  
  matrix(as.matrix(rval),nrow=2)
}
