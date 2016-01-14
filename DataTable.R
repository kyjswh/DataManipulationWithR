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




#Save time and make the codes cleaner and more readable
hflights_dt[,c('DistanceKMs','DistanceFeets') :=
                                        list(Distance/0.62137, Distance*5280)]

carriers <- unique(hflights_dt$UniqueCarrier)



#Assign a list of variables to a vector of variable names
hflights_dt[,paste('carrier',carriers, sep='_') :=
                                        lapply(carriers, function(x) as.numeric(UniqueCarrier == x))]
str(hflights_dt[,grep('^carrier',names(hflights_dt)),with=FALSE])


#Merge Tables
wdays <- data.table(
    DayOfWeek = 1:7,
    DayOfWeekString = c('Sunday',
                        'Monday',
                        'Tuesday',
                        'Wednesday',
                        'Thursday',
                        'Friday',
                        'Saturday')
)


system.time(merge(hflights, wdays))
system.time(merge(hflights_dt, wdays, by='DayOfWeek'))
  )


#Reshape and Visualization
#
library(reshape2)
melted_flights <- melt(hflights)

melted_flights <- melt(hflights,
                            id.vars = 0,
                              measure.vars = c('ActualElapsedTime','AirTime'))

str(melted_flights)
head(melted_flights)


#Group by operations 
##.N has special meaning for number of rows in data.table
hflights_dt[,.(mean_dist=mean(DistanceKMs),sd_dist=sd(DistanceKMs)),by=Dest][order(mean_dist,decreasing = T)]
hflights_dt[,.N, by=.(Origin,Dest)][order(N,decreasing=T)]
 

#Boxplot
library(ggplot2)
ggplot(melted_flights,aes(x = variable, y = value)) + geom_boxplot()


#long to wide format and aggregate
melted_flights <- melt(hflights,
                        id.vars = 'Month',
                          measure.vars = c('ActualElapsedTime','AirTime'))
df <- dcast(melted_flights, Month~variable, fun.aggregate=mean,na.rm=TRUE)

ggplot(melt(df, id.vars = 'Month')) + 
      geom_line(aes(x = Month, y = value, color = variable),size=1.2) + 
        scale_x_continuous(breaks=1:12) + 
          theme_bw() + theme(legend.poapplsition='top')


#wide to long on the data.table
hflights_melted_db <- melt(hflights_dt, id.vars = c('Month','Origin','Dest'), measure.vars = c('TaxiIn','TaxiOut'))
hflights_melted_db[variable == 'TaxiIn',
                    .(mean_val=mean(value,na.rm=T),sd_val=sd(value,na.rm=T)), 
                          by=.(Month,Origin)][order(Origin,Month)]
