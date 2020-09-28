getwd()
setwd("C:/Users/Arnav/Desktop/Semester 3/Machine Learning/R")
getwd()
readLines(con = "German-Credit_1.csv", n = 5)
df1 <- read.csv(file = "German-Credit_1.csv", header = TRUE)
head(df1)
install.packages("XLConnect")
library(XLConnect)
xl_sheet_df_part1 <- readWorksheetFromFile("German-Credit_1.csv", sheet = 1)
dim(xl_sheet_df_part1)
df1 <- read.table(file = "German-Credit_1.csv", header = TRUE)
head(df1)
colnames(df1)
df2 <- read.csv(file = "German-Credit_2.csv", header = TRUE)
head(df2)
colnames(df2)
df3 <- merge(x= df1, y = df2, by = "OBS", all = T)
summary(df1)
str(df1)

num_Attr <- c("DURATION", "AMOUNT", "INSTALL_RATE", "AGE", "NUM_CREDITS", "NUM_DEPENDENTS")

cat_Attr <- setdiff(x= colnames(df3), y = num_Attr)
df3$OBS <- as.character(df3$OBS)

df3$RESPONSE <- as.factor(as.character(df3$RESPONSE))


df_cat <- subset(df3,select = cat_Attr)
df3[,cat_Attr] <- data.frame(apply(df_cat, 2, function(x) as.factor(as.character(x))))

str(df3)
summary(df3)
colSums(is.na(x = df3))
sum(is.na(df3))

df4 <- na.omit(df3)
dim(df4)
dim(df3)
sum(is.na(df4))
install.packages("DMwR")
library(DMwR)
manyNAs(df3, 0.1)

df3_imputed <- centralImputation(data = df3) #CentralImputation
sum(is.na(df3_imputed))


df3_imputed1 <- knnImputation(data = df3, k = 5) #Knn Imputation
sum(is.na(df3_imputed1))


install.packages("infotheo")
library(infotheo)
x <- c(5,6,7,8,8,8,8,8,11,20,21,22)
length(x)

x0 <- discretize(x, disc = "equalfreq", nbins = 4)
table(x0)
x0 <- discretize(x, disc = "equalwidth", nbins = 4)
table(x0)

Amtbin <- discretize(df3_imputed$AMOUNT, disc = "equalfreq", nbins = 4)
table(Amtbin)


Amtbin <- discretize(df3_imputed$AMOUNT, disc = "equalwidth", nbins = 4)
table(Amtbin)

install.packages("dummies")
library(dummies)

df_ex <- datasets::warpbreaks
table(df_ex$tension)

dummy_ex <- dummy(df_ex$tension)
head(dummy_ex)


df_cat <- subset(df3_imputed, select = cat_Attr)
df_cat_dummies <- data.frame(apply(df_cat,2, function(x) dummy(x)))
dim(df_cat_dummies)


install.packages("vegan")
library(vegan)

df_num <- df3_imputed[,num_Attr]
df_num2 <- decostand(x = df_num, method = "range")
summary(df_num2)

df_num3 <- decostand(x = df_num, method = "standardize")
summary(df_num3)

df_final <- cbind(df_num3,df_cat)
head(df_final)

rows <- seq(1,1000,1)
set.seed(123)
trainRows <- sample(rows,600)

train_data <- df_final[trainRows,]
test_data <- df_final[-c(trainRows),]

dim(train_data)
lm_model <- lm(AMOUNT~DURATION, data = train_data)
summary(lm_model)















