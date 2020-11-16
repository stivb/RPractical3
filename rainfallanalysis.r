library(readr)
library(lubridate)
library(dplyr)
rainfall <- read_csv("rainfall.csv")
#making turning the month year pairs into an actual date
rainfall$tempDate<-paste("01",rainfall$Month,rainfall$Year,sep="-")
rainfall$trueDate<-as.Date(rainfall$tempDate,format="%d-%b-%Y")
pdf("myplot2.pdf")
#making a line plot
plot(recent$trueDate,recent$Rainfallmm,type="l",xlab="Time by month and year", ylab="Rainfall in millimetres")
#creating a month number column (so months are 1,2,3,4 etc rather than Jan,Feb,Mar,Apr)
rainfall$numMonth<-month(rainfall$trueDate)
#using dplyr to group by month and get the mean for that month
monthly<-rainfall %>% group_by(numMonth) %>% summarise(avg_rainfall = mean(Rainfallmm, na.rm=TRUE))
#now plotting it - I get the list of months in order by just choosing 
#unique from the whole "Month" (ie. Jan,Feb,March) column
barplot(height=monthly$avg_rainfall, names=unique(rainfall$Month), col="#69b3a2")
#creating a "decade" column
#basically subtracting 1910 from the year, dividing it by 10, converting to integer
#then multiplying that integer by 10, then adding the whole thing by 1910
#then putting an "s" at the end of it
rainfall$decade<-paste(as.integer((rainfall$Year-1910)/10)*10+1910,"s",sep="")
#this is a delightful construction (which I only just found out about)
#it means - aggregate the rainfall by decade and then find the average
#rainfall per month for that decade
#it turns it all into a 2 column tibble with decade in one column and avg rainfall in the other
bydecade<-aggregate(Rainfallmm ~ decade, data = rainfall, mean)
barplot(height=bydecade$Rainfallmm, names=bydecade$decade, col="#69b3a2")
dev.off()
