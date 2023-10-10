library(dplyr)

#DATASET WITHOUT COORDINATES

houseneigh <- read.csv("houseneighh.csv", header=TRUE, sep=";")

#We remove observations of Neighborhoods 'Greens', 'GrnHill' and 'Landmrk' because they are too poor to take them into considerations
houseneigh <- arrange(houseneigh,houseneigh$Neighborhood)
houseneigh <- houseneigh[-c(939,940,941,942,943,944,945,946,947,948,1041),]

###########################################################################################################################################################################################

#We create a train set (80% of the dataset) and a test set (20% of the dataset)

sample <- sample(c(TRUE, FALSE), nrow(houseneigh), replace=TRUE, prob=c(0.8,0.2))
train <- houseneigh[sample, ]
test <- houseneigh[!sample, ]

write.table(train, "train.csv", 
            sep = ";",             
            row.names = FALSE,     
            dec = ".",             
            na = "",
            quote = TRUE)

write.table(test, "test.csv", 
            sep = ";",             
            row.names = FALSE,     
            dec = ".",             
            na = "",
            quote = TRUE)

##############################################################################################################

train <- read.csv("train.csv", header=TRUE, sep=";")

train$price <- round(log10(train$price),3)
train$Garage.Area <- round(sqrt(train$Garage.Area)*0.3,3)
train$Total.Bsmt.SF <- round(sqrt(train$Total.Bsmt.SF)*0.3,3)
train$X1st.Flr.SF <- round(sqrt(train$X1st.Flr.SF)*0.3,3)
train$AgeofHouse <- ceiling((train$AgeofHouse)/5)
train$Mas.Vnr.Area <- round(sqrt(train$Mas.Vnr.Area)*0.3,3)
train$Wood.Deck.SF <- round(sqrt(train$Wood.Deck.SF)*0.3,3)

write.table(train, "traintransf.csv", 
            sep = ";",             
            row.names = FALSE,     
            dec = ".",             
            na = "",
            quote = TRUE)


test <- read.csv("test.csv", header=TRUE, sep=";")

test$price <- round(log10(test$price),3)
test$Garage.Area <- round(sqrt(test$Garage.Area)*0.3,3)
test$Total.Bsmt.SF <- round(sqrt(test$Total.Bsmt.SF)*0.3,3)
test$X1st.Flr.SF <- round(sqrt(test$X1st.Flr.SF)*0.3,3)
test$AgeofHouse <- ceiling((test$AgeofHouse)/5)
test$Mas.Vnr.Area <- round(sqrt(test$Mas.Vnr.Area)*0.3,3)
test$Wood.Deck.SF <- round(sqrt(test$Wood.Deck.SF)*0.3,3)

write.table(test, "testtransf.csv", 
            sep = ";",             
            row.names = FALSE,     
            dec = ".",             
            na = "",
            quote = TRUE)

##############################################################################################################

train <- read.csv("traintransf.csv", header=TRUE, sep=";")

train$Neighborhood <- as.factor(train$Neighborhood)
train$Neighborhood <- as.integer(train$Neighborhood)
index <- as.vector(train$Neighborhood)

write.table(index, "index_Neigh.csv", 
            sep = ";",             
            row.names = FALSE,     
            dec = ".",             
            na = "",
            quote = TRUE)


############################################################################################
############################################################################################


#DATASET WITH COORDINATES

houseneigh <- read.csv("housefinlatlong.csv", header=TRUE, sep=";")

#We remove observations of Neighborhoods 'Greens', 'GrnHill' and 'Landmrk' because they are too poor to take them into considerations
houseneigh <- arrange(houseneigh,houseneigh$Neighborhood)
houseneigh <- houseneigh[-c(938,939,940,941,942,943,944,945,946,1039),]

###########################################################################################################################################################################################

#We create a train set (80% of the dataset) and a test set (20% of the dataset)

sample <- sample(c(TRUE, FALSE), nrow(houseneigh), replace=TRUE, prob=c(0.8,0.2))
train <- houseneigh[sample, ]
test <- houseneigh[!sample, ]

write.table(train, "traincoor.csv", 
            sep = ";",             
            row.names = FALSE,     
            dec = ".",             
            na = "",
            quote = TRUE)

write.table(test, "testcoor.csv", 
            sep = ";",             
            row.names = FALSE,     
            dec = ".",             
            na = "",
            quote = TRUE)

##############################################################################################################

train <- read.csv("traincoor.csv", header=TRUE, sep=";")

train$price <- round(log10(train$price),3)
train$Garage.Area <- round(sqrt(train$Garage.Area)*0.3,3)
train$Total.Bsmt.SF <- round(sqrt(train$Total.Bsmt.SF)*0.3,3)
train$X1st.Flr.SF <- round(sqrt(train$X1st.Flr.SF)*0.3,3)
train$AgeofHouse <- ceiling((train$AgeofHouse)/5)
train$Mas.Vnr.Area <- round(sqrt(train$Mas.Vnr.Area)*0.3,3)
train$Wood.Deck.SF <- round(sqrt(train$Wood.Deck.SF)*0.3,3)

write.table(train, "traintransfcoor.csv", 
            sep = ";",             
            row.names = FALSE,     
            dec = ".",             
            na = "",
            quote = TRUE)


test <- read.csv("testcoor.csv", header=TRUE, sep=";")

test$price <- round(log10(test$price),3)
test$Garage.Area <- round(sqrt(test$Garage.Area)*0.3,3)
test$Total.Bsmt.SF <- round(sqrt(test$Total.Bsmt.SF)*0.3,3)
test$X1st.Flr.SF <- round(sqrt(test$X1st.Flr.SF)*0.3,3)
test$AgeofHouse <- ceiling((test$AgeofHouse)/5)
test$Mas.Vnr.Area <- round(sqrt(test$Mas.Vnr.Area)*0.3,3)
test$Wood.Deck.SF <- round(sqrt(test$Wood.Deck.SF)*0.3,3)

write.table(test, "testtransfcoor.csv", 
            sep = ";",             
            row.names = FALSE,     
            dec = ".",             
            na = "",
            quote = TRUE)

##############################################################################################################

train <- read.csv("traintransfcoor.csv", header=TRUE, sep=";")

train$Neighborhood <- as.factor(train$Neighborhood)
train$Neighborhood <- as.integer(train$Neighborhood)
index <- as.vector(train$Neighborhood)

write.table(index, "index_Neighh.csv", 
            sep = ";",             
            row.names = FALSE,     
            dec = ".",             
            na = "",
            quote = TRUE)

##############################################################################################################################

test <- read.csv("testtransfcoor.csv", header=TRUE, sep=";")

test$Neighborhood <- as.factor(test$Neighborhood)
test$Neighborhood <- as.integer(test$Neighborhood)
index <- as.vector(test$Neighborhood)
index[5:562] <- index[5:562]+1 

write.table(index, "indexneigh_test.csv", 
            sep = ";",             
            row.names = FALSE,     
            dec = ".",             
            na = "",
            quote = TRUE)





