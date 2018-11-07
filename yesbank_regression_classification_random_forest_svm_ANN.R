#loading file
train_base<-read.csv("Yes_Bank_Train (1).csv", header = T, sep = ',',stringsAsFactors=F)
train<-train_base

test_Base<-read.csv("Yes_Bank_Test_int.csv", header = T, sep = ',',stringsAsFactors=F)
test<-test_Base
##################
str(train)
str(test)

##setting train cluster
train$credit_amount<-ifelse(train$credit_amount<1500, 3,ifelse(train$credit_amount>=1500 & train$credit_amount<4000 , 2, 1))
train$credit_amount<-factor(train$credit_amount)
## 2 is missing so we will predict using linear regression then do the conversion
summary(train$credit_amount)

uniq_values<-sapply(train, function(x) length(unique(x)))
# serial.number     account_info   duration_month   credit_history          purpose 
# 800                4               33                5               10 
# credit_amount  savings_account    employment_st              poi  personal_status 
# 3                5                5                4                4 
# gurantors   resident_since    property_type              age installment_type 
# 3                4                4               53                3 
# housing_type       credits_no         job_type          liables        telephone 
# 3                4                4                2                2 
# foreigner 
# 2 
continuos_varaible<-names(uniq_values[uniq_values>10])
categorical_varaible<-names(uniq_values[uniq_values<=10])

library(ggplot2)
library(dplyr)

####continuos_varaible
continuos_varaible
train$serial.number<-NULL
##########################################################
mutatecol<- function(colname,partition){
  clause<- paste(" ",colname ," = case_when( ")
  for (i in 1:length(partition))  
  { 
    if (i==1)
    { clause<-paste(clause , " ( ",colname ," <= ", partition[i]," )~ ",i," , ")
    
    } else if (i>1 
               # & i!=length(partition)
    )
    {
      clause<-paste(clause, " ( ", colname ," > ", partition[i-1]," & ",colname ," <= ",partition[i]," ) ~ ",i," , ")
    } 
  }
  clause<-paste(clause , " ( ", colname ," > ", partition[length(partition)]," )~ ",length(partition)+1," ,T~",length(partition)+2," ) ")
  # clause<-paste( " lazyeval::interp( ", clause , ")") 
  print(clause)  
  #Data.Cars3<-mutate_(Data.Cars3,clause)
}

#
QuartileNHist <- function(data) 
{
  print(quantile(data,seq(0, 1, 0.01)))
  print(ggplot(data.frame(data),aes(x=data))+geom_histogram() + scale_x_continuous(breaks = round(seq(min(data), max(data), by = (max(data)-min(data))/30 ),1)))
  
}

#age
summary(train$age)
hist(train$age)
QuartileNHist(train$age)
#candidate for binning
partition<-c(20,22,28,30,38,41,47,54,67)
mutatecol('age',partition)

#segmenting the data set
train<-mutate(train,  age  = case_when(   (  age  <=  20  )~  1  ,   (  age  >  20  &  age  <=  22  ) ~  2  ,   (  age  >  22  &  age  <=  28  ) ~  3  ,   (  age  >  28  &  age  <=  30  ) ~  4  ,   (  age  >  30  &  age  <=  38  ) ~  5  ,   (  age  >  38  &  age  <=  41  ) ~  6  ,   (  age  >  41  &  age  <=  47  ) ~  7  ,   (  age  >  47  &  age  <=  54  ) ~  8  ,   (  age  >  54  &  age  <=  67  ) ~  9  ,   (  age  >  67  )~  10  ,T~ 11  ) )
test<-mutate(test,  age  = case_when(   (  age  <=  20  )~  1  ,   (  age  >  20  &  age  <=  22  ) ~  2  ,   (  age  >  22  &  age  <=  28  ) ~  3  ,   (  age  >  28  &  age  <=  30  ) ~  4  ,   (  age  >  30  &  age  <=  38  ) ~  5  ,   (  age  >  38  &  age  <=  41  ) ~  6  ,   (  age  >  41  &  age  <=  47  ) ~  7  ,   (  age  >  47  &  age  <=  54  ) ~  8  ,   (  age  >  54  &  age  <=  67  ) ~  9  ,   (  age  >  67  )~  10  ,T~ 11  ) )


#duration_month
summary(train$duration_month)
hist(train$duration_month)
QuartileNHist(train$duration_month)
#candidate for binning
partition<-c(4,10.82,15,24,36,48)
mutatecol('duration_month',partition)
#segmenting the data set
train<-mutate(train,  duration_month  = case_when(   (  duration_month  <=  4  )~  1  ,   (  duration_month  >  4  &  duration_month  <=  10.82  ) ~  2  ,   (  duration_month  >  10.82  &  duration_month  <=  15  ) ~  3  ,   (  duration_month  >  15  &  duration_month  <=  24  ) ~  4  ,   (  duration_month  >  24  &  duration_month  <=  36  ) ~  5  ,   (  duration_month  >  36  &  duration_month  <=  48  ) ~  6  ,   (  duration_month  >  48  )~  7  ,T~ 8  ) )
test<-mutate(test,  duration_month  = case_when(   (  duration_month  <=  4  )~  1  ,   (  duration_month  >  4  &  duration_month  <=  10.82  ) ~  2  ,   (  duration_month  >  10.82  &  duration_month  <=  15  ) ~  3  ,   (  duration_month  >  15  &  duration_month  <=  24  ) ~  4  ,   (  duration_month  >  24  &  duration_month  <=  36  ) ~  5  ,   (  duration_month  >  36  &  duration_month  <=  48  ) ~  6  ,   (  duration_month  >  48  )~  7  ,T~ 8  ))

##now all colums are factors
train_fact<-as.data.frame(sapply(train,factor))
test_fact<-as.data.frame(sapply(test,factor))# serial number will become afactor but we will ignore taht for now

train_fact$credit_amount<-train_base$credit_amount
summary(train_fact$credit_amount)
class(train_fact)
summary(train_fact)
names(train_fact)
library(car)
library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)


#############################
########
# separate training and validattion data
set.seed(100)
trainindices= sample(1:nrow(train_fact), 0.7*nrow(train_fact))
train_final = train_fact[trainindices,]
validation_final = train_fact[-trainindices,]
#########


names(train_fact)
names(train_final)
names(validation_final)
names(test_fact)
nrow(test_fact)

################
library(randomForest)
#duration_month+poi+property_type+job_type+telephone
data.rf <- randomForest(credit_amount ~ duration_month+poi+job_type, data=train_final, proximity=FALSE,
                        sampsize=100,
                        ntree=100, mtry=2, do.trace=TRUE, na.action=na.omit)
data.rf

testPred <- predict(data.rf, newdata=validation_final[,-5])

results<-cor(round(testPred,2),validation_final[,5])^2
results#0.5174544, 5 0.5265683,0.5232535,0.5256445,0.5169596,
#100,2 0.5272446


#######################################################################

testPred <-predict(data.rf, test_fact[,-1])
reults_final<-as.data.frame(cbind('serial number'=test_fact$serial.number,'credit_amount'=round(testPred,2)))
reults_final<-reults_final[order(reults_final$"serial number"),]
write.csv(reults_final,file ='submit_rf_reg__100_2.csv',row.names = F)


#######################################################################
names(validation_final)
##test for accuracy
results_fact<-factor(ifelse(testPred<1500, 3,ifelse(testPred>=1500 & testPred<4000 , 2, 1)))
validattion_fact<-factor(ifelse(validation_final[,5]<1500, 3,ifelse(validation_final[,5]>=1500 & validation_final[,5]<4000 , 2, 1)))

summary(validattion_fact)
summary(validattion_fact)
confusionMatrix(results_fact, validattion_fact)
#   Accuracy : 0.5792   
###############################################################
names(test_fact)
Eval_RF<- predict(data.rf, test_fact[,-1])
results_fact<-factor(ifelse(Eval_RF<1500, 3,ifelse(Eval_RF>=1500 & Eval_RF<4000 , 2, 1)))

reults_final<-as.data.frame(cbind('serial number'=test_fact$serial.number,cluster_number=results_fact))
reults_final<-reults_final[order(reults_final$"serial number"),]
write.csv(reults_final,file ='submit_rf_reg_100_2.csv',row.names = F)



################################################################
Eval_RF<- predict(data.rf, test_fact[,-1])
reults_final<-as.data.frame(cbind('serial number'=test_fact$serial.number,
                                  cluster_number=Eval_RF))
reults_final<-reults_final[order(reults_final$"serial number"),]
write.csv(reults_final,file ='submit_rf.csv',row.names = F)


############################################################################
#Artificial Neural Network


#loading file
train_base<-read.csv("Yes_Bank_Train (1).csv", header = T, sep = ',',stringsAsFactors=F)
train<-train_base

test_Base<-read.csv("Yes_Bank_Test_int.csv", header = T, sep = ',',stringsAsFactors=F)
test<-test_Base
##################
str(train)
str(test)

##setting train cluster
train$credit_amount<-ifelse(train$credit_amount<1500, 3,ifelse(train$credit_amount>=1500 & train$credit_amount<4000 , 2, 1))
train$credit_amount<-factor(train$credit_amount)
## 2 is missing so we will predict using linear regression then do the conversion
summary(train$credit_amount)

uniq_values<-sapply(train, function(x) length(unique(x)))
# serial.number     account_info   duration_month   credit_history          purpose 
# 800                4               33                5               10 
# credit_amount  savings_account    employment_st              poi  personal_status 
# 3                5                5                4                4 
# gurantors   resident_since    property_type              age installment_type 
# 3                4                4               53                3 
# housing_type       credits_no         job_type          liables        telephone 
# 3                4                4                2                2 
# foreigner 
# 2 
continuos_varaible<-names(uniq_values[uniq_values>10])
categorical_varaible<-names(uniq_values[uniq_values<=10])

library(ggplot2)
library(dplyr)

####continuos_varaible
continuos_varaible
train$serial.number<-NULL
##########################################################
mutatecol<- function(colname,partition){
  clause<- paste(" ",colname ," = case_when( ")
  for (i in 1:length(partition))  
  { 
    if (i==1)
    { clause<-paste(clause , " ( ",colname ," <= ", partition[i]," )~ ",i," , ")
    
    } else if (i>1 
               # & i!=length(partition)
    )
    {
      clause<-paste(clause, " ( ", colname ," > ", partition[i-1]," & ",colname ," <= ",partition[i]," ) ~ ",i," , ")
    } 
  }
  clause<-paste(clause , " ( ", colname ," > ", partition[length(partition)]," )~ ",length(partition)+1," ,T~",length(partition)+2," ) ")
  # clause<-paste( " lazyeval::interp( ", clause , ")") 
  print(clause)  
  #Data.Cars3<-mutate_(Data.Cars3,clause)
}

#
QuartileNHist <- function(data) 
{
  print(quantile(data,seq(0, 1, 0.01)))
  print(ggplot(data.frame(data),aes(x=data))+geom_histogram() + scale_x_continuous(breaks = round(seq(min(data), max(data), by = (max(data)-min(data))/30 ),1)))
  
}

#age
summary(train$age)
hist(train$age)
QuartileNHist(train$age)
#candidate for binning
partition<-c(20,22,28,30,38,41,47,54,67)
mutatecol('age',partition)

#segmenting the data set
train<-mutate(train,  age  = case_when(   (  age  <=  20  )~  1  ,   (  age  >  20  &  age  <=  22  ) ~  2  ,   (  age  >  22  &  age  <=  28  ) ~  3  ,   (  age  >  28  &  age  <=  30  ) ~  4  ,   (  age  >  30  &  age  <=  38  ) ~  5  ,   (  age  >  38  &  age  <=  41  ) ~  6  ,   (  age  >  41  &  age  <=  47  ) ~  7  ,   (  age  >  47  &  age  <=  54  ) ~  8  ,   (  age  >  54  &  age  <=  67  ) ~  9  ,   (  age  >  67  )~  10  ,T~ 11  ) )
test<-mutate(test,  age  = case_when(   (  age  <=  20  )~  1  ,   (  age  >  20  &  age  <=  22  ) ~  2  ,   (  age  >  22  &  age  <=  28  ) ~  3  ,   (  age  >  28  &  age  <=  30  ) ~  4  ,   (  age  >  30  &  age  <=  38  ) ~  5  ,   (  age  >  38  &  age  <=  41  ) ~  6  ,   (  age  >  41  &  age  <=  47  ) ~  7  ,   (  age  >  47  &  age  <=  54  ) ~  8  ,   (  age  >  54  &  age  <=  67  ) ~  9  ,   (  age  >  67  )~  10  ,T~ 11  ) )


#duration_month
summary(train$duration_month)
hist(train$duration_month)
QuartileNHist(train$duration_month)
#candidate for binning
partition<-c(4,10.82,15,24,36,48)
mutatecol('duration_month',partition)
#segmenting the data set
train<-mutate(train,  duration_month  = case_when(   (  duration_month  <=  4  )~  1  ,   (  duration_month  >  4  &  duration_month  <=  10.82  ) ~  2  ,   (  duration_month  >  10.82  &  duration_month  <=  15  ) ~  3  ,   (  duration_month  >  15  &  duration_month  <=  24  ) ~  4  ,   (  duration_month  >  24  &  duration_month  <=  36  ) ~  5  ,   (  duration_month  >  36  &  duration_month  <=  48  ) ~  6  ,   (  duration_month  >  48  )~  7  ,T~ 8  ) )
test<-mutate(test,  duration_month  = case_when(   (  duration_month  <=  4  )~  1  ,   (  duration_month  >  4  &  duration_month  <=  10.82  ) ~  2  ,   (  duration_month  >  10.82  &  duration_month  <=  15  ) ~  3  ,   (  duration_month  >  15  &  duration_month  <=  24  ) ~  4  ,   (  duration_month  >  24  &  duration_month  <=  36  ) ~  5  ,   (  duration_month  >  36  &  duration_month  <=  48  ) ~  6  ,   (  duration_month  >  48  )~  7  ,T~ 8  ))

##now all colums are factors
train_fact<-as.data.frame(sapply(train,factor))
test_fact<-as.data.frame(sapply(test,factor))# serial number will become afactor but we will ignore taht for now
class(train_fact)
summary(train_fact)
names(train_fact)
library(car)
library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
######factor variabel conversion
factor.var<-as.data.frame(model.matrix(~.,data=train_fact[,-5]))
factor.var<-factor.var[,-1]
colnames(factor.var)
factor.var$credit_amount<-train_base$credit_amount

names(test_fact)


test.var<-as.data.frame(model.matrix(~.,data=test_fact[,-1]))
test.var<-test.var[,-1]
names(test.var)
test.var$serial.number<-test_fact[,1]


########
# separate training and validattion data
set.seed(100)
trainindices= sample(1:nrow(factor.var), 0.7*nrow(factor.var))
train_final = factor.var[trainindices,]
validation_final = factor.var[-trainindices,]
#########

c('duration_month', 'job_type','poi4')
names(train_final)
h2o.init()
train_final_h20<-as.h2o(train_final)
validation_final_h20<-as.h2o(validation_final)
#'duration_month5' , 'duration_month6' ,'duration_month7' ,  'job_typeA174' ,'poi4' 

yes_regresion <- h2o.deeplearning(x = c('duration_month5' , 'duration_month6' ,'duration_month7' ,  'job_typeA174' ,'poi4' ),
                                  y = 'credit_amount',
                                  training_frame = train_final_h20,
                                  validation_frame = validation_final_h20,
                                  distribution = "AUTO",
                                  activation = "RectifierWithDropout",
                                  hidden = c(100,100,100,100,100),
                                  hidden_dropout_ratio = c(0.1, 0.1, 0.1,.1,.1),
                                  l1 = 1e-5,
                                  epochs = 20)


yes_regresion
test.var_h20<-as.h2o(test.var)
creditPrediction <- h2o.predict(yes_regresion, test.var_h20)
creditPrediction<-as.data.frame(as.numeric(round(creditPrediction$predict,2)))
names(creditPrediction)
cbind(test.var$serial.number,creditPrediction$predict)
creditPrediction$predict
names(reults_final1)
reults_final1<-as.data.frame(cbind('serial number'=test.var$serial.number,'credit_amount'=round(creditPrediction$predict,2)))
names(reults_final1)
reults_final1<-reults_final1[order(as.numeric(reults_final1$"serial number")),]
write.csv(reults_final1,file ='submit_deep_learning.csv',row.names = F)

