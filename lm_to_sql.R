##this function is used to transform a multiple linear regression formula (lm function) into SQL code.

lm_to_sql <- function(x)
{
  
  variable_name <- c("",gsub("[-^0-9]", "", names(unlist(x$xlevels))))
  mode_name <- c("",as.character(unlist(x$xlevels)))
  names <- data.frame(variable_name, mode_name, var_mode=c("(Intercept)",paste(variable_name,mode_name,sep="")[-1]))
  regression <- data.frame(var_mode=names(coefficients(x)), coef=as.numeric(coefficients(x)))
  merged <- merge(names,regression,all.x=TRUE)
  merged$type[2:nrow(merged)] <- "factor"
  merged$type[1] <- "intercept"
  
  numeric <- data.frame(variable_name = names(coefficients(x)), coef=as.numeric(coefficients(x)))
  numeric <- numeric[which(!(numeric[,1] %in% merged[,1])),]
  numeric$type <- "numeric"
  numeric$mode_name <- numeric$variable_name
  numeric <- numeric[,c(1,4,2,3)]
  
  all_merged <- rbind(merged[,-1], numeric)
  rownames(all_merged) <- NULL
  all_merged <- all_merged[-which(is.na(all_merged$coef)),]
  
  sql1 <- paste0(all_merged$coef[1]," + ")
  
  for (i in 2:nrow(all_merged)) {
    
    if (all_merged$type[i]=="factor") 
    {sql0 <- paste0("(case when ", all_merged$variable_name[i], " = ", 
                    all_merged$mode_name[i], " then ",all_merged$coef[i]," else 0 end)"," + ")}
    
    else if (all_merged$type[i]=="numeric") 
    {sql0<-paste0("(",all_merged$coef[i],")*","isnull(",all_merged$variable_name[i],",0)"," + ")}
    
    sql1 <- paste0(sql1,sql0)
  }
  
  sql <- substr(sql1,1,nchar(sql1)-1)
  
  return(sql)

}
