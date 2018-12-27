#1. Import the Titanic Dataset from the link Titanic Data Set.
myData <- read.csv("titanic3.csv")
#Perform the following:
  #a. Preprocess the passenger names to come up with a list of titles that represent families and represent using appropriate visualization graph.
final_Data <- myData[complete.cases(myData$pclass),] # remove the empty row
final_Data[which(final_Data$embarked ==''),'embarked'] = NA
final_Data[which(final_Data$cabin ==''),'cabin'] = NA
final_Data[which(final_Data$home.dest ==''),'home.dest'] = NA
final_Data[which(final_Data$boat ==''),'boat'] = NA

summary(final_Data)
## function that preprocesses the data (splits each row in the "name" column into a list and then picks the text between the comma nd the period)
eConvert <- function(final_Data){
  titles <- apply(final_Data,1,function(row){
    strsplit(strsplit(as.character(row['name']),', ')[[1]][2],'\\.')[[1]][1]
  })
  ## clean up the titles to use the most common ones and keep the ones commonly used
  retained_titles <- c('Dr','Master', 'Miss', 'Mr', 'Mrs', 'Rev')
  revised_titles <- list(Mlle = 'Miss', Mme = 'Mrs', Sir = 'Mr', Ms = 'Miss')
  for (i in names(revised_titles)){
    titles[titles == i] <- revised_titles[[i]]
  }
  ## change the rare titles to 'Seldom_titles'
  titles[!titles %in% retained_titles] = 'Seldom_titles'
  final_Data$Ptitle <- as.factor(titles)
}

eData <- eConvert(final_Data) ## assign the converted titles to eData
summary(as.factor(eData))     ## view the summary of the converted titles

library(ggplot2)
barplot(height = table(eData),horiz = FALSE,col = 'blue')

  #b. Represent the proportion of people survived from the family size using a graph.
final_Data$famSize <- final_Data$sibsp + final_Data$parch + 1                   ## Define a new column in the data set 'famSize'
summary(as.factor(final_Data$famSize))
par(mfrow = c(1,2))
ggplot(final_Data,aes(x= final_Data$famSize, fill = factor(final_Data$survived))) + 
  geom_bar(stat = 'count')  +   labs(x = 'Family Size')  + labs(y ='Survived')

## to have a better view of this analysis, we group family sizes and assign category
famCat = array(dim = length(final_Data$famSize))
famCat[final_Data$famSize == 1] = 'Small'
famCat[final_Data$famSize >= 2 & final_Data$famSize <= 4] = 'Medium'
famCat[final_Data$famSize > 4] = 'Big'

final_Data$famSize1 <- as.factor(famCat)
# plot grouped data
ggplot(final_Data,aes(x= final_Data$famSize1, fill = factor(final_Data$survived))) + 
  geom_bar(stat = 'count')  +   labs(x = 'Family Size')  + labs(y ='Survived')

  #c. Impute the missing values in Age variable using Mice Library, create two different graphs showing Age distribution before and after imputation.
library(mice)
set.seed(8)
df_Impute <- final_Data[,names(final_Data) %in% c('age','sibsp','parch','fare')]
meanImpute <- mice(data = df_Impute, method = "rf", m=5)
ageImpute <- complete(meanImpute)

par(mfrow=c(1,2))
hist(final_Data$age, main = "Before Imputation", col = "red")
hist(ageImpute$age, main = "After Imputation", col = "blue")




