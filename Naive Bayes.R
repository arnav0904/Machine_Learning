rm(list = ls(all.names = TRUE))
getwd()
setwd("C:/Users/Arnav/Desktop/Semester 3/ML/R")

install.packages("mlbench")
library(mlbench)
data(HouseVotes84) # This dataset is contained in mlbench library


#barplots for specific issue
plot(as.factor(HouseVotes84[,2]))
title(main = 'Votes cast for issue 1', xlab = "Vote", ylab = "Num reps")


#by party
HouseVotes84$Class
Repub <- HouseVotes84$Class == "republican"
Democrat <- HouseVotes84$Class == "democrat"
Repub
plot(as.factor(HouseVotes84[Repub,2]))
title(main = 'Republic Votes cast for issue 1', xlab = "Vote", ylab = "Num reps")
plot(as.factor(HouseVotes84[Democrat,2]))
title(main = 'Democrat Votes cast for issue 1', xlab = "Vote", ylab = "Num reps")

#Imputing NA Values

#function to return number of NAs by vote and class (democrat and republican)
na_by_col_class <- function(col,cls){return(sum(is.na(HouseVotes84[,col]) & HouseVotes84$Class==cls))}
na_by_col_class


#function to compute conditional probability that a member will caste "yes"
#vote for a particular issue. 
p_y_col_class <- function(col,cls){
  sum_y <- sum(HouseVotes84[,col]=="y" & HouseVotes84$Class==cls,na.rm = TRUE)
  sum_n <- sum(HouseVotes84[,col]=="n" & HouseVotes84$Class==cls,na.rm = TRUE)
return(sum_y/(sum_y+sum_n))
  }


#check the probability of yes vote by a democrat in issue 5
p_y_col_class(5, "democrat")


#Impute the missing value
#If the republican congressman didn't vote, then we are allocationg 'y' or 'n' based on their
#party voted 'y' or 'n'
for (i in 2:ncol(HouseVotes84)) {
  if(sum(is.na(HouseVotes84[,i])>0)) {
    c1 <- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=="democrat",arr.ind = TRUE)
    c2 <- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=="republican",arr.ind = TRUE)
    HouseVotes84[c1,i] <-
      ifelse(runif(na_by_col_class(i,"democrat"))<p_y_col_class(i,"democrat"),"y","n")
    HouseVotes84[c2,i] <-
      ifelse(runif(na_by_col_class(i,"republican"))<p_y_col_class(i,"republican"),"y","n")}
}

#divide into test and training test
#create new col "train" and assign 1 or 0 in 80/20 proportion via random uniform dist
HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84))<0.80,1,0)

#get col num of train
trainColNum <- grep("train", names(HouseVotes84))

#separate training and test sets and remove training column before modeling
trainHouseVotes84 <- HouseVotes84[HouseVotes84$train==1, -trainColNum]
testHouseVotes84 <- HouseVotes84[HouseVotes84$train==0, -trainColNum]

#package e1071
install.packages("e1071")
library(e1071)
nb_model <- naiveBayes(Class~.,data = trainHouseVotes84)
nb_model
summary(nb_model)
str(nb_model)


#Lets test the model
nb_test_predict <- predict(nb_model, testHouseVotes84[,-1])



#fraction of correct predictions
mean(nb_test_predict==testHouseVotes84$Class)


#Confusion matrix
table(pred=nb_test_predict, true=testHouseVotes84$Class)




































