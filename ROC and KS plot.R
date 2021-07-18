
#clear all variables
rm(list=ls())

## browse and select data set. in your case select breastcancer.csv
mydata <- read.csv(file = choose.files(),header = TRUE,sep = ",")

## if you don't want to browse and select us this 
mydata <- read.csv(file = '..your drive/...folder/breastcancer.csv,',header = TRUE,sep = ",")


## delete all rows having NA
mydata <- mydata[complete.cases(mydata), ]


## IN this dataset binary variable is Class, convert that as factor
mydata['Class'] <- factor(ifelse(mydata['Class'] == 'malignant', 1, 0), levels = c(0, 1))



# Now Prep Training and Test data.
library(caret)
n=which(colnames(mydata)=='Class')
set.seed(100)
trainDataIndex <- createDataPartition(mydata[,n], p=0.70, list = F)  # 70% training data

trainData <- mydata[trainDataIndex, ]
testData <- mydata[-trainDataIndex, ]



#There is approximately 2 times more benign samples. So lets downsample it using the downSample function from caret package.
#To do this you just need to provide the X and Y variables as arguments.
# Down Sample

'%!ni%' <- Negate('%in%')  # define 'not in' function
options(scipen=999)  # prevents printing scientific notation

set.seed(100)
n=which(colnames(mydata)=='Class')
down_train <- downSample(x = trainData[, colnames(trainData) %!ni% 'Class'],
                          y = trainData[,'Class'],yname = 'Class')
#Benign and malignant are now in the same ratio.

# Up Sample.
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %!ni% 'Class'],
                      y = trainData[,'Class'],yname = 'Class')

##----------------

#apply gml function get model.  we can change the variable and try
logitmod <- glm(Class ~ +Bare.nuclei + Cell.size + Cell.shape + Bl.cromatin, family = binomial(link = logit), data=down_train)


# apply your model and predict
pred    <- predict(logitmod, newdata = testData, type = "response")


#make all values either 1 or 0 and not in between
y_predict  <- as.numeric(ifelse( pred  > 0.5, 1, 0))

#make y_predict as factor and y_actual is already factor
y_predict  <- factor( y_predict , levels = c(0, 1))
y_actual   <- testData[,'Class']


#to calculate ks plot and Roc plot we need these two should be numeric, converting it here
actscore <- as.numeric(as.character(y_actual))
predscore <- as.numeric(as.character(y_predict))


# now you have the plot ready
InformationValue::ks_plot(actuals = actscore,predictedScores =  predscore)
InformationValue::plotROC(actuals = actscore, predictedScores = predscore)



#additional points if you want
summary(logitmod)
caret::confusionMatrix(y_predict, y_actual, positive="1", mode="everything")
