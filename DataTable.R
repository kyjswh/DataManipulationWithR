library(data.table)
library(hflights)


data(hflights)


hflights_dt <- data.table(hflights)


#Create a variable in-place
#Data.table does not allocate additional resources for the dataset (does not create extra copy)
#This saves a lot of overheads
address(hflights_dt)
system.time(hflights_dt[,DistanceKMs:=Distance/0.62137])
address(hflights_dt)

address(hflights)
system.time(hflights$DistanceKMs <- hflights$Distance/0.62137)
address(hflights)

head(hflights$DistanceKMs)
head(hflights$DistanceKMs)

#You can also vectorize assignment of multiple variables at once
#Save time and make the codes cleaner and more readable
hflights_dt[,c('DistanceKMs','DistanceFeets') :=
                                        list(Distance/0.62137, Distance*5280)]

carriers <- unique(hflights_dt$UniqueCarrier)

#Assign a list of variables to a vector of variable names
hflights_dt[,paste('carrier',carriers, sep='_') :=
                                        lapply(carriers, function(x) as.numeric(UniqueCarrier == x))]
str(hflights_dt[,grep('^carrier',names(hflights_dt)),with=FALSE])
