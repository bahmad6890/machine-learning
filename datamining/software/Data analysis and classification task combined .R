setwd("D:/Rproject/data minig task")
getwd()

library(dplyr) # Data Manipulation
#install.packages("randomForest")
library(Amelia) # Missing Data: Missings Map
library(ggplot2) # Visualization
library(scales) # Visualization
library(caTools) # Prediction: Splitting Data
library(car) # Prediction: Checking Multicollinearity
library(ROCR) # Prediction: ROC Curve
library(e1071) # Prediction: SVM, Naive Bayes, Parameter Tuning
library(rpart) # Prediction: Decision Tree
library(rpart.plot) # Prediction: Decision Tree
library(randomForest) # Prediction: Random Forest
library(caret) # Prediction: k-Fold Cross Validation

titanic_train_dataset = read.csv("titanicdataset1.csv")
titanic_test_dataset = read.csv('titanicdataset2.csv')


# Combining data
titanic_full_dataset <- bind_rows(titanic_train_dataset, titanic_test_dataset)
# Checking the structure of the data
str(titanic_full_dataset)
head(titanic_full_dataset)
summary(titanic_full_dataset)
mean(titanic_full_dataset$Fare)
max(titanic_full_dataset$Fare)
mode(titanic_full_dataset$Fare)

barplot2(table(titanic_full_dataset$Pclass), main="Barplot of Passenger Class",xlab="Class", ylab="Number of Passengers ", col = "red")
barplot2(table(titanic_full_dataset$Sex) , main="Barplot of Sex", xlab="Sex", ylab="Number of Passengers ", col = "red")
barplot2(table(titanic_full_dataset$Age) , main="Barplot of Age", xlab="Age", ylab="Number of Passengers ", col = "red")

# ,c("Cherbourg","Queenstown","Southampton")
barplot2(table(titanic_full_dataset$Embarked) , main="Barplot of Port of Embarkment" , xlab="Port of Embarkment", ylab="Number of Passengers ", col = "red")  
# Handling Missing Data

## Checking Missing Data

# Checking missing values (missing values or empty values)
colSums(is.na(titanic_full_dataset)|titanic_full_dataset=='')


missmap(titanic_full_dataset, main="Titanic Data - Missings Map",
        col=c("red", "blue"), legend=FALSE)


## Missing Fare Data Imputation

# Extract the row which contains the missing Fare
filter(titanic_full_dataset, is.na(Fare)==TRUE|Fare=='')

ggplot(filter(titanic_full_dataset, Embarked=="S"  & Pclass==3 ), aes(Fare)) +                       
  geom_vline(aes(xintercept=median(Fare, na.rm=T)), colour='darkblue', linetype='dashed', size=2) +
  geom_density(fill="blue", alpha=0.5) +
  geom_vline( size=2, colour='red',aes(xintercept=mean(Fare, na.rm=T)) , linetype='dashed') +
  theme_bw() +
  ggtitle("Fare distribution of 3rd class passengers \n embarked from Southampton port") +
  theme(plot.title = element_text(hjust = 0.5))




# Impute the missing Fare value by the median fare of third class passengers embarked from Southampton port
titanic_full_dataset$Fare[is.na(titanic_full_dataset$Fare)==TRUE] = median(filter(titanic_full_dataset, Pclass==3 & Embarked=="S")$Fare, na.rm=TRUE)

# Checking missing values
colSums(is.na(titanic_full_dataset)|titanic_full_dataset=='')

## Missing Embarked Data Imputation

# Extract the rows which contain the missing Embarked values
filter(titanic_full_dataset, is.na(Embarked)==TRUE|Embarked=='')

# Frequency of ports of embarkation of first class passengers
table(filter(titanic_full_dataset, Pclass==1)$Embarked)

ggplot(filter(titanic_full_dataset, is.na(Embarked)==FALSE & Embarked!='' & Pclass==1), 
       aes(Embarked, Fare)) +     
  geom_boxplot(aes(colour = Embarked)) +
  geom_hline(aes(yintercept=80), colour='red', linetype='dashed', size=2) +
  ggtitle("Fare distribution of first class passengers") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Impute the missing Embarked values by the Cherbourg port
titanic_full_dataset$Embarked[titanic_full_dataset$Embarked==""] = "C"

# Checking missing values
colSums(is.na(titanic_full_dataset)|titanic_full_dataset=='')

## Missing Age Data Imputation

ggplot(titanic_full_dataset,aes(Pclass,Age)) +                                                  
  geom_boxplot(aes(fill=factor(Pclass)),alpha=0.5) +
  ggtitle("Age distribution based on Pclass")


# Imputation of Age based on Pclass
impute.age <- function(age,class){
  vector <- age
  for (i in 1:length(age)){
    if (is.na(age[i])){
      if (class[i] == 1){
        vector[i] <- round(mean(filter(titanic_full_dataset,Pclass==1)$Age, na.rm=TRUE),0)
      }else if (class[i] == 2){
        vector[i] <- round(mean(filter(titanic_full_dataset,Pclass==2)$Age, na.rm=TRUE),0)
      }else{
        vector[i] <- round(mean(filter(titanic_full_dataset,Pclass==3)$Age, na.rm=TRUE),0)
      }
    }else{
      vector[i]<-age[i]
    }
  }
  return(vector)
}
imputed.age <- impute.age(titanic_full_dataset$Age,titanic_full_dataset$Pclass)
titanic_full_dataset$Age <- imputed.age


# Checking missing values
colSums(is.na(titanic_full_dataset)|titanic_full_dataset=='')

# Feature Engineering

## Passenger Title


head(titanic_full_dataset$Name)

# Grab passenger title from passenger name
titanic_full_dataset$Title <- gsub("^.*, (.*?)\\..*$", "\\1", titanic_full_dataset$Name)

# Frequency of each title by sex
table(titanic_full_dataset$Sex, titanic_full_dataset$Title)

# First, I reassign few categories 
titanic_full_dataset$Title[titanic_full_dataset$Title == 'Mlle' | titanic_full_dataset$Title == 'Ms'] <- 'Miss' 
titanic_full_dataset$Title[titanic_full_dataset$Title == 'Mme']  <- 'Mrs' 

# Then, I create a new category with low frequency of titles
Other <- c('Dona', 'Dr', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Jonkheer', 'Major', 'Rev', 'Sir')
titanic_full_dataset$Title[titanic_full_dataset$Title %in% Other]  <- 'Other'

# Let's see if it worked
table(titanic_full_dataset$Sex, titanic_full_dataset$Title)

## Family Size

FamilySize <- titanic_full_dataset$SibSp + titanic_full_dataset$Parch + 1

table(FamilySize)

# Create a family size feature with three categories
titanic_full_dataset$FamilySize <- sapply(1:nrow(titanic_full_dataset), function(x) 
  ifelse(FamilySize[x]==1, "Single", 
         ifelse(FamilySize[x]>4, "Large", "Small")))

table(titanic_full_dataset$FamilySize)


# Exploratory Data Analysis

## Encoding the categorical features as factors

titanic_full_dataset$Survived = factor(titanic_full_dataset$Survived)
titanic_full_dataset$Pclass = factor(titanic_full_dataset$Pclass)
titanic_full_dataset$Sex = factor(titanic_full_dataset$Sex)
titanic_full_dataset$Embarked = factor(titanic_full_dataset$Embarked)
titanic_full_dataset$Title = factor(titanic_full_dataset$Title)
titanic_full_dataset$FamilySize = factor(titanic_full_dataset$FamilySize, levels=c("Single","Small","Large"))

#Checking the structure of the data
str(titanic_full_dataset)

## Exploratory Data Analysis on Pclass, Sex and Age

ggplot(filter(titanic_full_dataset, is.na(Survived)==FALSE), aes(Pclass, fill=Survived)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=.8, position="dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(labels=percent, breaks=seq(0,0.6,0.05)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on Pclass") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(filter(titanic_full_dataset, is.na(Survived)==FALSE), aes(Sex, fill=Survived)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.9) +
  facet_wrap(~Pclass) + 
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(labels=percent, breaks=seq(0,0.4,0.05)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on Pclass and Sex") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(filter(titanic_full_dataset, is.na(Survived)==FALSE), aes(Pclass, Age)) + 
  geom_violin(aes(fill=Survived), alpha=0.9) +
  facet_wrap(~Survived) + 
  scale_fill_brewer(palette = "Set1", direction = -1) +
  ggtitle("Survival Rate based on Pclass and Age") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## Exploratory Data Analysis on Title and FamilySize

mosaicplot(~ Title + Survived, data=titanic_full_dataset, main='Survival Rate based on Title', shade=TRUE)



ggplot(filter(titanic_full_dataset, is.na(Survived)==FALSE), aes(Title)) + 
  geom_bar(aes(fill=Survived), alpha=0.9, position="fill") +
  facet_wrap(~Pclass) + 
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on Pclass and Title") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



mosaicplot(~ FamilySize + Survived, data=titanic_full_dataset, main='Survival Rate based on FamilySize', shade=TRUE)



ggplot(filter(ttitanic_full_dataset, is.na(Survived)==FALSE), aes(Title)) + 
  geom_bar(aes(fill=Survived), alpha=0.9, position="fill") +
  facet_wrap(~FamilySize) + 
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on FamilySize and Title") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(filter(titanic_full_dataset, is.na(Survived)==FALSE), aes(Embarked, Fare)) + 
  geom_boxplot(aes(fill=Survived), alpha=0.9) +
  facet_wrap(~Survived) + 
  scale_fill_brewer(palette="Set1") +
  ggtitle("Survival Rate based on Embarked and Fare") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))






#################Clasification Task ##########################
# Prediction

## Splitting the dataset into the Training set and Test set


# Splitting the dataset into the Training set and Test set
train_original <- titanic_full_dataset[1:891, c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Title","FamilySize")]
test_original <- titanic_full_dataset[892:1309, c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Title","FamilySize")]

## Splitting the training set into the Training set and Validation set


# Splitting the Training set into the Training set and Validation set
set.seed(789)
split = sample.split(train_original$Survived, SplitRatio = 0.8)
train = subset(train_original, split == TRUE)
test = subset(train_original, split == FALSE)


## Decision Tree


# Fitting Decision Tree Classification Model to the Training set
classifier = rpart(Survived ~ ., data = train, method = 'class')

# Tree Visualization
rpart.plot(classifier, extra=4, col= "red")


# Predicting the Validation set results
y_pred = predict(classifier, newdata = test[,-which(names(test)=="Survived")], type='class')

# Checking the prediction accuracy
table(test$Survived, y_pred) # Confusion matrix
error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

# Applying k-Fold Cross Validation
set.seed(789)
folds = createMultiFolds(train$Survived, k = 10, times = 5)
control <- trainControl(method = "repeatedcv", index = folds)
classifier_cv <- train(Survived ~ ., data = train, method = "rpart", trControl = control)

# Tree Visualization
rpart.plot(classifier_cv$finalModel, extra=4)

# Predicting the Validation set results
y_pred = predict(classifier_cv, newdata = test[,-which(names(test)=="Survived")])

# Checking the prediction accuracy
table(test$Survived, y_pred) # Confusion matrix
error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

