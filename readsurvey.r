library(readr)
introsurvey <- read_csv("7COM1079introsurvey.csv")
df = subset(introsurvey, select = -c(Email,Name) )
for ( col in 1:ncol(df)){
  colnames(df)[col] <-  tail(strsplit(colnames(df)[col],split=" ")[[1]],1)
}
View(df)


printColumnInfo <- function(clm, lbl){   
  vals<-na.omit(unlist(clm))
  message (sprintf("Mean of %s: %f", lbl,mean(vals)))
  message (sprintf("Table of %s:", lbl))
  print(table(vals))
}

doHistogram<-function(clm,lbl)
{
  vals<-na.omit(unlist(clm))
  barplot(table(vals),main=lbl,
          xlab="Rating",
          ylab="Number of students")
}

printColumnInfo(df[,4],"GIT")
doHistogram(df[,4],"GIT")

printColumnInfo(df[,5],"Spreadsheets")
printColumnInfo(df[,6],"Statistics")
printColumnInfo(df[,7],"R")
printColumnInfo(df[,8],"Kanban ")

print ("Table of voters")
print (table(unlist(df[,9])))

print ("Table of rainfall")
print (table(unlist(df[,10])))

print ("Table of basketball")
print (table(unlist(df[,11])))

print ("Table of pValue")
print (table(unlist(df[,13])))

print("Correlation betweeen knowledge of spreadsheets and knowledge of R")
cor(as.numeric(unlist(df[,5])),as.numeric(unlist(df[,7])))
plot(as.numeric(unlist(df[,5])),as.numeric(unlist(df[,7])))




