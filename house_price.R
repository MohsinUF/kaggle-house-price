# libraries
library(tidyverse)
library(mice)
library(e1071)
library(Metrics)
library(skimr)
library(pracma)

# read data
train = read.csv(file.choose(), stringsAsFactors = F)
test = read.csv(file.choose(), stringsAsFactors = F)
full = bind_rows(train,test)
out1 = read.csv(file.choose(), header = T)
out2 = read.csv(file.choose(), header = T)

# finding the missing values
skim = full %>% skim()
amount = sapply(full,function(x) sum(is.na(x)))

# working with missing values
SalePrice = train$SalePrice
Id = test$Id
full[,c('Id','SalePrice')] = NULL
rm(train,test)

chr = full[,sapply(full,is.character)]
int = full[,sapply(full,is.integer)]

chr[is.na(chr)] = "Not Available"
fac = chr %>% 
  lapply(as.factor) %>% 
  as.data.frame()

full = bind_cols(fac,int)

# completing the missing values
mice = full %>% 
  mice(method ='rf')
full = complete(mice)
rm(chr, fac, int, mice)

train = full[1:length(SalePrice),]
test = full[(length(SalePrice) + 1):nrow(full),]

# SVM model
mymodel= svm(SalePrice ~ ., data = train, cost = 3)
mymodel
summary(mymodel)

# prediction with train data
p1 = predict(mymodel, train)
head(p1)


# prediction with test data
pred = predict(mymodel, newdata = test)
head(pred)

# submission file [here I took some external help (out1 & out2)]
solution = data.frame(Id = Id, 
                       SalePrice = nthroot(out1$SalePrice*out2$SalePrice*pred,3))
head(solution)
write.csv(solution,"solution.csv",row.names = F) 
