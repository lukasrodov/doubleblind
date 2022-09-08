rm(list = ls())
setwd("C:/Users/Jonas/Desktop/degustace/degustace 2")

#using czech because of the commentaries
Sys.setlocale(locale = "Czech")
library("readxl")

#in the whole code we will have sorted every table by names alphabetically because we use different sources

#loading the data with the results
X <- read_excel("Results.xlsx", sheet = 1)

#renaming and translating the column names to English
X$Date <- X$Zeitstempel
X$Name <- X$Jméno
X$Age <- X$`Vìk (12 bylo?)`

#order the result table
X <- X[order(X$Name),]

#loading the data with the tasting schedule
Kbeer <- read_excel("tasting_schedule.xlsx", sheet = 1)
Kbeer <- Kbeer[order(Kbeer$...1),]

#now the names are useless
Kbeer <- Kbeer[,-1]

#creating a list with the names
nameslist <- X$Name

#creating an empty data frame in which we will place the votes of the people after sorting
QuantTab <- data.frame(matrix(NA, nrow=13, ncol=12, dimnames = list(x= nameslist, y=c("Date",
                                                                                    "Name", 
                                                                                    "Age", 
                                                                                    "Primator11",
                                                                                    "Plzen", 
                                                                                    "Svijany", 
                                                                                    "IPA", 
                                                                                    "SchlickRudohor",
                                                                                    "Bernard", 
                                                                                    "Budvar33", 
                                                                                    "Budvarklasik",
                                                                                    "Kozel"))))

#creating a duplicate of the quantitative tabel because of the second vote people got on different beers
QuanTabDuplicate <- QuantTab

#creating an empty data frame in which we will place the comments of the people after sorting
QualTab <- QuantTab

#creating a list with all the beers 
#QuantTabList 
Beerlist<- c("Primator11",
                  "Plzen", 
                  "Svijany", 
                  "IPA", 
                  "SchlickRudohor",
                  "Bernard", 
                  "Budvar33", 
                  "Budvarklasik",
                  "Kozel")




#sorting the quantitative values from the data table to the result table
#we group the results in the quantitative table
#while we group we always check (because everybody tasted 1 random beer twice) 
#if we already have an value for a beer and if yes put it in a second table
#creating a a table for the different commentaries, quantitative and qualitative
Comment <- data.frame(Name=X$Name)
Comment$Beer <- 0
Comment$Opinion1 <- 0
Comment$Vote1 <- 0
Comment$Opinion2 <- 0
Comment$Vote2 <- 0
k <- 0
while(k<nrow(QuantTab)
){
  k <- k+1
  a <- 3
  while(a<ncol(X)-12){
    a <- a+2
    storage <- X[[c(a,k)]]
    place <- which(Beerlist[]==Kbeer[[c((a-3)/2,k)]])
    place <- as.integer(place)
#    QuantTab[c(k),c(place+3)] <-  storage
    
    if( is.na(QuantTab[c(k),c(place+3)])){
      QuantTab[c(k),c(place+3)] <-  storage
      QuanTabDuplicate[c(k),c(place+3)] <- storage
    }
    else{                                          
    QuanTabDuplicate[c(k),c(place+3)] <- storage
    Comment[c(k),c(2)] <- Beerlist[c(place)]
    Comment[c(k),c(4)] <- storage
    
    Comment[c(k),c(6)] <- QuantTab[c(k),c(place+3)]   
    
     }
  }
}


boxplot(QuantTab[,c(4:12)],main="ZnÃ¡mky bez Ãºpravy")
#summary(QuantTab[c(-1,-7),c(4:12)])


#sorting the qualitative values from the data table to the result table and at the same time we put the second opinion on the beer twice tasted in a seperate table

k <- 0
while(k<nrow(QuantTab)
){
  k <- k+1
  a <- 3
  while(a<ncol(X)-12){
    a <- a+2
    storage <- X[[c(a+1,k)]]
    place <- which(Beerlist[]==Kbeer[[c((a-3)/2,k)]])
    place <- as.integer(place)
#    if( is.na(QualTab[c(k),c(place+3)])){print("dick")}
#    QualTab[c(k),c(place+3)] <- storage
    
    if( is.na(QualTab[c(k),c(place+3)])){
      QualTab[c(k),c(place+3)] <-  storage
     
    }
    else{                                          
      Comment[c(k),c(3)] <- storage

      Comment[c(k),c(5)] <- QualTab[c(k),c(place+3)]
    }
    
    
    
    
  }
}



#creating an average in the beers people tasted twice

#library("data.table")
#QuantMean <- rbindlist(list(QuantTab,QuanTabDuplicate))[,lapply(.SD,mean), list(Name)]

QuantMean <- (QuantTab + QuanTabDuplicate)/2


#creating an weight according how consistent the people tasted
#first calculate how different their tasting of the two same beers were
Weight <- data.frame(QuantMean[,c(4:12)])-data.frame(QuantTab[,c(4:12)])
#for the ones which rated the same R so the difference would be 0, R gives an NA
Weight[is.na(Weight)] <- 0
#now we want the sum of all errors (an trick to get a list and not have a table with 1 value per person at a different place)
Weight <- rowSums(Weight)
#absolute values
Weight <- abs(Weight)
#now we decide how much weight should be given to the perspective people
Wsum <- sum(Weight)
Weight <- Wsum/Weight
Weight[is.infinite(Weight)] <- 36
wsum <- sum(Weight)
wF <- Weight/wsum
beer <- QuantMean[,c(4:12)]
wbeer <- beer*wF
wF100 <- wF*100
wF100 <- round(wF100)
library("dplyr")
counter <- 0
while(counter<nrow(QuantTab)){
  counter <- counter+1
  mulpl <- wF100[c(counter)]
  beernew <- beer %>% slice(rep(counter, each = mulpl))
  beer <- rbind(beer,beernew)
}



boxplot(beer)




#agnes which reveals how close to each other the participants are in their tastes
cbeer <- beer[!duplicated(beer), ]
D <- dist(cbeer)
hcf <- hclust(D)
plot(hcf)
#agnes with beers
tscbeer <- t(cbeer)
D <- dist(tscbeer)
hcf <- hclust(D)
plot(hcf)

#evaluation quiz
#first we make a data frame with the right results
QuizAnswers <- c("Ano",
                 "Ano", 
                 "Ne", 
                 "Ne", 
                 "Ne",
                 "Ano",
                 "Ano")

QuizAnswers <- as.data.frame(QuizAnswers)
#now we create a placeholder for the given result in a certain moment
Result <- c("Result") 
#dataframe with the assigned points to each one
Quiz <- data.frame(Name=X$Name)
Quiz$Results <- 0
a <- 1
counter <- 0
counter2 <- 0
Q <- subset(X[,c(2,25:31)])
while(a<ncol(Q)){
  a <- a+1
  k <- 0
  while(k<nrow(Q)){
    k <- k+1
    counter <- counter+1
    storage <- 0
    storage <-  ifelse(as.name(as.character(Q[c(k),c(a)]))==as.name(as.character(QuizAnswers[c(a),c(1)])),1,0)
    storage <- as.numeric(storage)
    Quiz[c(k),c(2)] <- as.numeric(Quiz[c(k),c(2)])+storage
  }
  
  counter2 <- counter2+1
}


Results <- Comment
Results$Points <- Quiz$Results

plot(Results)

