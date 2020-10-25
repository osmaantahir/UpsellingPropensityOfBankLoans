
# LIBRARIES
library(pacman) #Package Management
p_load(readr,install=TRUE,update=getOption("par_update")) # reading csv files
p_load(MASS,install=TRUE,update=getOption("par_update")) # 
p_load(caTools,install=TRUE,update=getOption("par_update")) #
p_load(DataExplorer,install=TRUE,update=getOption("par_update")) # exploratory data analysis
p_load(NbClust,install=TRUE,update=getOption("par_update")) # cluster recommendations
p_load(fpc,install=TRUE,update=getOption("par_update")) #
p_load(cluster,install=TRUE,update=getOption("par_update")) # Clustering functions
p_load(rpart,install=TRUE,update=getOption("par_update")) # CART functions
p_load(rpart.plot,install=TRUE,update=getOption("par_update")) # CART Visualization
p_load(tidyverse,install=TRUE,update=getOption("par_update")) # 
p_load(randomForest,install=TRUE,update=getOption("par_update")) # Random Forest functions
p_load(ROCR,install=TRUE,update=getOption("par_update")) # Model Performance
p_load(ineq,install=TRUE,update=getOption("par_update")) # Model Performance
p_load(InformationValue,install=TRUE,update=getOption("par_update")) # Concordance
p_load(data.table,install=TRUE,update=getOption("par_update")) # Data Table Functions
p_load(psych,install=TRUE,update=getOption("par_update")) # pairs.panel




# WORKING DIRECTORY ----
setwd("C:/DSBA_Course/Proper Learning/Module 4 [Machine Learning]/M4 W5/")

TheraBankData <- read.csv("TheraBank.csv",header = TRUE)
head(TheraBankData)

# LOADING DATA ----
TheraBankData <- setNames(TheraBankData,c("ID","Age","Exp","Income","Zip","FMembers","CCAvg","Education","Mortgage","PLoan","Securities","CDAmount","Online","CreditCard"))
str(TheraBankData)

# DATA DICTIONARY ----
# ID	        Customer ID
# Age	        Customer's age in years
# Exp       	Years of professional experience
# Income	    Annual income of the customer ($000)
# Zip   	    Home Address ZIP code.
# FMembers    Family size of the customer
# CCAvg	      Avg. spending on credit cards per month ($000)
# Education	  Education Level. 1: Undergrad; 2: Graduate; 3: Advanced/Professional
# Mortgage	  Value of house mortgage if any. ($000)
# PLoan       Loan	Did this customer accept the personal loan offered in the last campaign?
# Securities  Account	Does the customer have a securities account with the bank?
# CDAccount	  Does the customer have a certificate of deposit (CD) account with the bank?
# Online	    Does the customer use internet banking facilities?
# CreditCard	Does the customer use a credit card issued by the bank?

# PROBLEM ----
# This case is about a bank (Thera Bank) which has a growing customer base. 
# Majority of these customers are liability customers (depositors) with varying size of deposits. 
# The number of customers who are also borrowers (asset customers) is quite small, and the bank is interested 
# in expanding this base rapidly to bring in more loan business and in the process, earn more through the interest on loans. 
# In particular, the management wants to explore ways of converting its liability customers to personal loan customers 
# (while retaining them as depositors). A campaign that the bank ran last year for liability customers showed a 
# healthy conversion rate of over 9% success. This has encouraged the retail marketing department to devise campaigns with 
# better target marketing to increase the success ratio with a minimal budget. 
# The department wants to build a model that will help them identify the potential customers who have a higher probability 
# of purchasing the loan. This will increase the success ratio while at the same time reduce the cost of the campaign. 
# The dataset has data on 5000 customers. The data include customer demographic information (age, income, etc.), 
# the customer's relationship with the bank (mortgage, securities account, etc.), and the customer response to the 
# last personal loan campaign (Personal Loan). 
# Among these 5000 customers, only 480 (= 9.6%) accepted the personal loan that was offered to them in the earlier campaign.

# DESIRED SOLUTION ----
# 1.1 EDA - Basic data summary, Univariate, Bivariate analysis, graphs
# 2.1 Apply Clustering algorithm < type, rationale>
# 2.2 Clustering Output interpretation < dendrogram, number of clusters, remarks to make it meaningful to understand>
# 3.1 Applying CART <plot the tree>
# 3.2 Interpret the CART model output <pruning, remarks on pruning, plot the pruned tree>
# 3.3 Applying Random Forests<plot the tree>
# 3.4 Interpret the RF model output <with remarks, making it meaningful for everybody>
# 4.1 Confusion matrix interpretation
# 4.2 Interpretation of other Model Performance Measures <KS, AUC, GINI>
# 4.3 Remarks on Model validation exercise <Which model performed the best>

# EDA ----
dim(TheraBankData)
plot_intro(TheraBankData)
plot_str(TheraBankData)
head(TheraBankData)
tail(TheraBankData)
plot_missing(TheraBankData)
summary(TheraBankData)


# Missing Value Treatment
TheraBankData$FMembers = as.factor(replace(as.character(TheraBankData$FMembers), is.na(TheraBankData$FMembers),"2"))
split <- sample.split(TheraBankData$PLoan, SplitRatio = 0.7)
?sample.split()
# SPLITTING DATA ----
trainSample<- subset(TheraBankData, split == TRUE)
testSample<- subset(TheraBankData, split == FALSE)

trainSampleRF<- subset(TheraBankData, split == TRUE)
testSampleRF<- subset(TheraBankData, split == FALSE)

# Transoformation ----
TheraBankData$Education = as.factor(TheraBankData$Education)
TheraBankData$PLoan = as.factor(TheraBankData$PLoan)
TheraBankData$Securities = as.factor(TheraBankData$Securities)
TheraBankData$CDAmount = as.factor(TheraBankData$CDAmount)
TheraBankData$Online = as.factor(TheraBankData$Online)
TheraBankData$CreditCard = as.factor(TheraBankData$CreditCard)
TheraBankData$FMembers = as.factor(TheraBankData$FMembers)
summary(TheraBankData)

TheraBankData = TheraBankData[,-1]
TheraBankData = TheraBankData[,-4]
summary(TheraBankData)

# Univariate ----

par(mfrow=c(1,2),mai = c(0.5, 0.5, 0.5, 0.5))

#Age

indexI=1
h <- hist(TheraBankData[,indexI], main = paste0("Centrality of ",names(TheraBankData[indexI]),"\n [St.Dev.=",round(sd(TheraBankData[,indexI]),2),"]"), ylim = c(0,900), labels = FALSE, col = "dark grey",xlab = NULL, cex.main=0.75,cex=0.1,cex.labels=0.1,breaks = 10)
mode <- h$mids[h$counts == max(h$counts)]
abline(v = mean(TheraBankData[,indexI]),col = "royalblue",lwd = 2)
abline(v = median(TheraBankData[,indexI]),col = "red",lwd = 2)
abline(v = mode,col = "green",lwd = 2)
legend("topright", inset=.01,c("Mean","Median","Mode"), fill=c("royalblue","red","green"), horiz=TRUE, cex=0.8)
boxplot(TheraBankData[,indexI], horizontal = TRUE, main = paste0("Dispersion of ",names(TheraBankData[indexI]),"\n [St.Dev.=",round(sd(TheraBankData[,indexI]),2),"]"), col="grey",xlab=NULL,cex.main=0.75)
text(fivenum(TheraBankData[,indexI]), labels =fivenum(TheraBankData[,indexI]), y=1.3, col = "blue", cex = 0.6)
rug(TheraBankData[,indexI], side = 1, ticksize = 0.08, lwd = 0.9, col = "grey");


#Experience
indexI=2
h <- hist(TheraBankData[,indexI], main = paste0("Centrality of ",names(TheraBankData[indexI]),"\n [St.Dev.=",round(sd(TheraBankData[,indexI]),2),"]"), ylim = c(0,900), labels = FALSE, col = "dark grey",xlab = NULL, cex.main=0.75,cex=0.1,cex.labels=0.1,breaks = 10)
mode <- h$mids[h$counts == max(h$counts)]
abline(v = mean(TheraBankData[,indexI]),col = "royalblue",lwd = 2)
abline(v = median(TheraBankData[,indexI]),col = "red",lwd = 2)
abline(v = mode,col = "green",lwd = 2)
legend("topright", inset=.01,c("Mean","Median","Mode"), fill=c("royalblue","red","green"), horiz=TRUE, cex=0.8)
boxplot(TheraBankData[,indexI], horizontal = TRUE, main = paste0("Dispersion of ",names(TheraBankData[indexI]),"\n [St.Dev.=",round(sd(TheraBankData[,indexI]),2),"]"), col="grey",xlab=NULL,cex.main=0.75)
text(fivenum(TheraBankData[,indexI]), labels =fivenum(TheraBankData[,indexI]), y=1.3, col = "blue", cex = 0.6)
rug(TheraBankData[,indexI], side = 1, ticksize = 0.08, lwd = 0.9, col = "grey");

#Income
indexI=3
h <- hist(TheraBankData[,indexI], main = paste0("Centrality of ",names(TheraBankData[indexI]),"\n [St.Dev.=",round(sd(TheraBankData[,indexI]),2),"]"), ylim = c(0,1300), labels = FALSE, col = "dark grey",xlab = NULL, cex.main=0.75,cex=0.1,cex.labels=0.1,breaks = 10)
mode <- h$mids[h$counts == max(h$counts)]
abline(v = mean(TheraBankData[,indexI]),col = "royalblue",lwd = 2)
abline(v = median(TheraBankData[,indexI]),col = "red",lwd = 2)
abline(v = mode,col = "green",lwd = 2)
legend("topright", inset=.01,c("Mean","Median","Mode"), fill=c("royalblue","red","green"), horiz=TRUE, cex=0.8)
boxplot(TheraBankData[,indexI], horizontal = TRUE, main = paste0("Dispersion of ",names(TheraBankData[indexI]),"\n [St.Dev.=",round(sd(TheraBankData[,indexI]),2),"]"), col="grey",xlab=NULL,cex.main=0.75)
text(fivenum(TheraBankData[,indexI]), labels =fivenum(TheraBankData[,indexI]), y=1.3, col = "blue", cex = 0.6)
rug(TheraBankData[,indexI], side = 1, ticksize = 0.08, lwd = 0.9, col = "grey");

#Credit Card Average
indexI=5
h <- hist(TheraBankData[,indexI], main = paste0("Centrality of ",names(TheraBankData[indexI]),"\n [St.Dev.=",round(sd(TheraBankData[,indexI]),2),"]"), ylim = c(0,2500), labels = FALSE, col = "dark grey",xlab = NULL, cex.main=0.75,cex=0.1,cex.labels=0.1,breaks = 10)
mode <- h$mids[h$counts == max(h$counts)]
abline(v = mean(TheraBankData[,indexI]),col = "royalblue",lwd = 2)
abline(v = median(TheraBankData[,indexI]),col = "red",lwd = 2)
abline(v = mode,col = "green",lwd = 2)
legend("topright", inset=.01,c("Mean","Median","Mode"), fill=c("royalblue","red","green"), horiz=TRUE, cex=0.8)
boxplot(TheraBankData[,indexI], horizontal = TRUE, main = paste0("Dispersion of ",names(TheraBankData[indexI]),"\n [St.Dev.=",round(sd(TheraBankData[,indexI]),2),"]"), col="grey",xlab=NULL,cex.main=0.75)
text(fivenum(TheraBankData[,indexI]), labels =fivenum(TheraBankData[,indexI]), y=1.3, col = "blue", cex = 0.6)
rug(TheraBankData[,indexI], side = 1, ticksize = 0.08, lwd = 0.9, col = "grey");

# Mortgage
indexI=7
h <- hist(TheraBankData[,indexI], main = paste0("Centrality of ",names(TheraBankData[indexI]),"\n [St.Dev.=",round(sd(TheraBankData[,indexI]),2),"]"), ylim = c(0,4500), labels = FALSE, col = "dark grey",xlab = NULL, cex.main=0.75,cex=0.1,cex.labels=0.1,breaks = 10)
mode <- h$mids[h$counts == max(h$counts)]
abline(v = mean(TheraBankData[,indexI]),col = "royalblue",lwd = 2)
abline(v = median(TheraBankData[,indexI]),col = "red",lwd = 2)
abline(v = mode,col = "green",lwd = 2)
legend("topright", inset=.01,c("Mean","Median","Mode"), fill=c("royalblue","red","green"), horiz=TRUE, cex=0.8)
boxplot(TheraBankData[,indexI], horizontal = TRUE, main = paste0("Dispersion of ",names(TheraBankData[indexI]),"\n [St.Dev.=",round(sd(TheraBankData[,indexI]),2),"]"), col="grey",xlab=NULL,cex.main=0.75)
text(fivenum(TheraBankData[,indexI]), labels =fivenum(TheraBankData[,indexI]), y=1.3, col = "blue", cex = 0.6)
rug(TheraBankData[,indexI], side = 1, ticksize = 0.08, lwd = 0.9, col = "grey");

#Family Size
par(mfrow=c(1,1),mai = c(0.9, 0.6, 0.9, 0.6))

temp <- table(TheraBankData$FMembers)
barplot(temp, 
        main="Family Size",
        horiz = TRUE,
        col=c("beige","brown"),
        border=NA,
        xlim=c(0,5000),
        xlab="Count")


# Education
par(mfrow=c(1,1),mai = c(0.9, 0.6, 0.9, 0.6))

temp <- table(TheraBankData$Education)
barplot(temp, 
        main="Education",
        horiz = TRUE,
        col=c("beige","brown"),
        border=NA,
        xlim=c(0,5000),
        xlab="Count")


# Personal Loan
par(mfrow=c(1,1),mai = c(0.9, 0.6, 0.9, 0.6))

temp <- table(TheraBankData$PLoan)
barplot(temp, 
        main="Personal Loan",
        horiz = TRUE,
        col=c("beige","brown"),
        border=NA,
        xlim=c(0,5000),
        xlab="Count")

# Securities
par(mfrow=c(1,1),mai = c(0.9, 0.6, 0.9, 0.6))

temp <- table(TheraBankData$Securities)
barplot(temp, 
        main="Securities Account",
        horiz = TRUE,
        col=c("beige","brown"),
        border=NA,
        xlim=c(0,5000),
        xlab="Count")

# CD Amount
par(mfrow=c(1,1),mai = c(0.9, 0.6, 0.9, 0.6))

temp <- table(TheraBankData$CDAmount)
barplot(temp, 
        main="CD Account",
        horiz = TRUE,
        col=c("beige","brown"),
        border=NA,
        xlim=c(0,5000),
        xlab="Count")

# Online 
par(mfrow=c(1,1),mai = c(0.9, 0.6, 0.9, 0.6))

temp <- table(TheraBankData$Online)
barplot(temp, 
        main="Online",
        horiz = TRUE,
        col=c("beige","brown"),
        border=NA,
        xlim=c(0,5000),
        xlab="Count")

# Credit Card
par(mfrow=c(1,1),mai = c(0.9, 0.6, 0.9, 0.6))

temp <- table(TheraBankData$CreditCard)
barplot(temp, 
        main="Credit Card Account",
        horiz = TRUE,
        col=c("beige","brown"),
        border=NA,
        xlim=c(0,5000),
        xlab="Count")

# Bivariate ----
par(mfrow=c(1,1),mai = c(0.9, 0.8, 0.9, 0.8))
pairs.panels(TheraBankData)

plot(TheraBankData$PLoan,TheraBankData$Age, xlab="PLoan", ylab="Age",label=TRUE,main="Personal Loan vs Age")
plot(TheraBankData$PLoan,TheraBankData$Exp, xlab="PLoan", ylab="Exp",label=TRUE,main="Personal Loan vs Experience")
plot(TheraBankData$PLoan,TheraBankData$Income, pch=18,xlab="PLoan", ylab="Income",label=TRUE,main="Personal Loan vs Income")
plot(TheraBankData$PLoan,TheraBankData$FMembers, xlab="PLoan", ylab="Family",label=TRUE,main="Personal Loan vs Family Size")
plot(TheraBankData$PLoan,TheraBankData$CCAvg, pch=18,xlab="PLoan", ylab="CCAvg",label=TRUE,main="Personal Loan vs CCAvg")
plot(TheraBankData$PLoan,TheraBankData$Education, xlab="PLoan", ylab="Education",label=TRUE,main="Personal Loan vs Education")
plot(TheraBankData$PLoan,TheraBankData$Mortgage, xlab="PLoan", ylab="Mortgage",label=TRUE,main="Personal Loan vs Mortgage")
plot(TheraBankData$PLoan,TheraBankData$Securities, xlab="PLoan", ylab="Securities",label=TRUE,main="Personal Loan vs Securities")
plot(TheraBankData$PLoan,TheraBankData$CDAmount, xlab="PLoan", ylab="CD Account",label=TRUE,main="Personal Loan vs CD Account")
plot(TheraBankData$PLoan,TheraBankData$Online, xlab="PLoan", ylab="Online",label=TRUE,main="Personal Loan vs Online")
plot(TheraBankData$PLoan,TheraBankData$CreditCard, xlab="PLoan", ylab="Credit Card",label=TRUE,main="Personal Loan vs Credit Card")

# Clustering ----

CustomerSegment <- TheraBankData[,1:7]
str(CustomerSegment)
CustomerSegment$Education = as.numeric(CustomerSegment$Education)
CustomerSegment$FMembers = as.numeric(CustomerSegment$FMembers)

str(CustomerSegment)

CustomerSegment.Scaled <- scale(CustomerSegment)

head(round(CustomerSegment.Scaled,2))

## Identifying the optimal number of clusters form WSS

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(CustomerSegment.Scaled, nc=10)
abline(v=3,col="red")

## Kmeans

kmeans.clus = kmeans(x=CustomerSegment.Scaled, centers = 5, nstart = 5)
kmeans.clus

## plotting the clusters
plotcluster(CustomerSegment.Scaled, kmeans.clus$cluster)
clusplot(CustomerSegment.Scaled, kmeans.clus$cluster, color=TRUE, shade=TRUE, labels=0, lines=1)


## profiling the clusters
CustomerSegment$Clusters <- kmeans.clus$cluster
View(CustomerSegment)
aggr = aggregate(CustomerSegment,list(CustomerSegment$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(CustomerSegment$Clusters)),
                            aggr[,-1])

View(clus.profile)


TheraBankData$Cluster = CustomerSegment$Clusters

split <- sample.split(TheraBankData$PLoan, SplitRatio = 0.7)

# SPLITTING DATA ----
trainSample<- subset(TheraBankData, split == TRUE)
testSample<- subset(TheraBankData, split == FALSE)

trainSampleRF<- subset(TheraBankData, split == TRUE)
testSampleRF<- subset(TheraBankData, split == FALSE)

# Decision Tree ----
str(trainSample)

r.ctrl = rpart.control(minsplit=100, minbucket = 3, cp = 0, xval = 10)

CART_PRED <- rpart(formula = PLoan ~ ., data = trainSample, method = "class", control=r.ctrl)

CART_PRED
rpart.plot(CART_PRED)

#Cost Complexity Table
printcp(CART_PRED)
plotcp(CART_PRED)
abline(v=3,col="red")
CP_Threshold = 0.049

#pruning
pCart = prune(CART_PRED, cp= CP_Threshold ,"CP")
printcp(pCart)
pCart
rpart.plot(pCart)

#prediction
trainSample$prediction = predict(pCart, data=trainSample, type="class")
trainSample$score = predict(pCart, data=trainSample, type="prob")

tbl=table(trainSample$PLoan,trainSample$prediction)
print((tbl[1,2]+tbl[2,1])/3500)


testSample$prediction = pCart %>% predict(testSample, type="class")
testSample$score = pCart %>% predict(testSample, type="prob")

tbl=table(testSample$PLoan,testSample$prediction)
print((tbl[1,2]+tbl[2,1])/1500)


# Random Forest ----

seed=1000
set.seed(seed)
trainSampleRF$PLoan=as.factor(trainSampleRF$PLoan)
rndFor = randomForest(PLoan ~ ., data = trainSampleRF, 
                      ntree=501, mtry = 3, nodesize = 10,
                      importance=TRUE)


print(rndFor)

#Out of Bag Error 
rndFor$err.rate
plot(rndFor, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Train Sample")

#Importance of Variables
print(rndFor$importance)

#Tuning
str(trainSampleRF)
set.seed(seed)
tRndFor = tuneRF(x = trainSampleRF[,-8], 
                 y=trainSampleRF[,8],
                 mtryStart = 3, 
                 ntreeTry = 101, 
                 stepFactor = 1.5, 
                 improve = 0.0001, 
                 trace=TRUE, 
                 plot = TRUE,
                 doBest = TRUE,
                 nodesize = 10, 
                 importance=TRUE
)
importance(tRndFor)

## Scoring syntax
trainSampleRF$class = predict(tRndFor, trainSampleRF, type="class")
trainSampleRF$prob1 = predict(tRndFor, trainSampleRF, type="prob")[,"1"]
tbl=table(trainSampleRF$PLoan, trainSampleRF$class)
print((tbl[1,2]+tbl[2,1])/3500)

##Probability Threshold
qs=quantile(trainSampleRF$prob1,prob = seq(0,1,length=11))
print(qs)
print(qs[10])
threshold=0.5 

mean((trainSampleRF$PLoan[trainSampleRF$prob1>threshold])=="1")

## Test Sample

nrow(testSampleRF)
testSampleRF$predict.class = predict(tRndFor, testSampleRF, type="class")
testSampleRF$prob1 = predict(tRndFor, testSampleRF, type="prob")[,"1"]
head(testSampleRF)

tbl=table(testSampleRF$PLoan, testSampleRF$predict.class)
print((tbl[1,2]+tbl[2,1])/1500)

mean((testSampleRF$PLoan[testSampleRF$prob1>threshold])=="1")

# Model Performance Evaluation ----

# Confusion Matrix

tbl=table(testSample$PLoan, testSample$prediction)
print(tbl)
print((tbl[1,2]+tbl[2,1])/1500)

tbl=table(testSampleRF$PLoan, testSampleRF$predict.class)
print(tbl)
print((tbl[1,2]+tbl[2,1])/1500)

#Deciles for Ranking

#CART
testSample$prob1 = testSample$score[,2]
probs=seq(0,1,length=11)
qs=quantile(testSample$prob1, probs)
testSample$deciles=cut(as.numeric(testSample$prob1), unique(qs),include.lowest = TRUE,right=FALSE)
table(testSample$deciles)

#RF
probs=seq(0,1,length=11)
qs=quantile(testSampleRF$prob1, probs)
testSampleRF$deciles=cut(testSampleRF$prob1, unique(qs),include.lowest = TRUE,right=FALSE)
table(testSampleRF$deciles)


#Rank Order Table

#CART
testDT = data.table(testSample)
testDT$PLoan = as.numeric(testDT$PLoan)
rankTbl = testDT[, list(
  cnt = length(PLoan), 
  cnt_tar1 = sum(PLoan), 
  cnt_tar0 = sum(PLoan == 0)
), 
by=deciles][order(-deciles)]

rankTbl$rrate = round(rankTbl$cnt_tar1 / rankTbl$cnt,4)*100;
rankTbl$cum_resp = cumsum(rankTbl$cnt_tar1)
rankTbl$cum_non_resp = cumsum(rankTbl$cnt_tar0)
rankTbl$cum_rel_resp = round(rankTbl$cum_resp / sum(rankTbl$cnt_tar1),4)*100;
rankTbl$cum_rel_non_resp = round(rankTbl$cum_non_resp / sum(rankTbl$cnt_tar0),4)*100;
rankTbl$ks = abs(rankTbl$cum_rel_resp - rankTbl$cum_rel_non_resp);

print(rankTbl)

#RF
testRFDT = data.table(testSampleRF)
testRFDT$PLoan = as.numeric(testRFDT$PLoan)
rankRFTbl = testRFDT[, list(
  cnt = length(PLoan), 
  cnt_tar1 = sum(PLoan), 
  cnt_tar0 = sum(PLoan == 0)
), 
by=deciles][order(-deciles)]

rankRFTbl$rrate = round(rankRFTbl$cnt_tar1 / rankRFTbl$cnt,4)*100;
rankRFTbl$cum_resp = cumsum(rankRFTbl$cnt_tar1)
rankRFTbl$cum_non_resp = cumsum(rankRFTbl$cnt_tar0)
rankRFTbl$cum_rel_resp = round(rankRFTbl$cum_resp / sum(rankRFTbl$cnt_tar1),4)*100;
rankRFTbl$cum_rel_non_resp = round(rankRFTbl$cum_non_resp / sum(rankRFTbl$cnt_tar0),4)*100;
rankRFTbl$ks = abs(rankRFTbl$cum_rel_resp - rankRFTbl$cum_rel_non_resp);

print(rankRFTbl)


#Performance CART
predObjCART = prediction(testSample$prob1, testSample$PLoan)
perfCART = performance(predObjCART, "tpr", "fpr")
plot(perfCART)

#Performance RF
predObjRF = prediction(testSampleRF$prob1, testSampleRF$PLoan)
perfRF = performance(predObjRF, "tpr", "fpr")
plot(perfRF)

#KS
KSCART = max(perfCART@y.values[[1]]-perfCART@x.values[[1]])

KSRF = max(perfRF@y.values[[1]]-perfRF@x.values[[1]])

#AUC
aucCART = performance(predObjCART,"auc"); 
aucCART = as.numeric(aucCART@y.values)
aucRF = performance(predObjRF,"auc"); 
aucRF = as.numeric(aucRF@y.values)

#GINI
giniCART = ineq(testSample$prob1, type="Gini")
giniRF = ineq(testSampleRF$prob1, type="Gini")

#Concordance and Discordance
Concordance(actuals=testSample$PLoan, predictedScores=testSample$prob1)
Concordance(actuals=testSampleRF$PLoan, predictedScores=testSampleRF$prob1)
