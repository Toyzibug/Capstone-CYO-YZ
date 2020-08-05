## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message = TRUE)


## ----library, message=FALSE----------------------------------------------------------------------------------------------------------------------------------------------
###Loading packages
if(!require(tidyverse)) install.packages("tidyverse", 
                        repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                        repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                        repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", 
                        repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGgally", 
                        repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", 
                        repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", 
                        repos = "http://cran.us.r-project.org")


## ----data wrangle--------------------------------------------------------------------------------------------------------------------------------------------------------
#download and wrangle the dataset
varnames<-c("Sex", "Length", "Diameter", "Height", "Whole weight", 
            "Shucked weight", "Viscera weight", "Shell weight", "Rings")
abalone <- read.csv(url(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"),
                 header = FALSE, col.names = varnames)

#Data structure
str(abalone)


## ----Output Summary------------------------------------------------------------------------------------------------------------------------------------------------------
#explore the output variable
summary(abalone$Rings)
histogram(abalone$Rings)
#To check if there is output classes with small count
abalone%>%group_by(Rings)%>%mutate(n=n())%>%filter(n()<5)%>%top_n(10)%>%select(Rings,n)


## ----RMSE----------------------------------------------------------------------------------------------------------------------------------------------------------------
#evaluation of models
RMSE <- function(true_Rings, pred){ sqrt(mean((true_Rings - pred)^2))}


## ----Data Intro for Cleaning---------------------------------------------------------------------------------------------------------------------------------------------
#detect any missing values
plot_intro(abalone)
## detect zero values and show the column which contains 0 values
which(!!colSums(abalone==0))


## ----Zero Value in Height------------------------------------------------------------------------------------------------------------------------------------------------
#list observations with 0
abalone%>%filter(Height==0)


## ----Replace Zero value--------------------------------------------------------------------------------------------------------------------------------------------------
# name our data abalone_cleaned as cleaned dataset
abalone_cleaned<-abalone

#use the data-table function to replace 0 with NA
setDT(abalone_cleaned)[Height==0, Height := NA,]

##replace the "NA"s with median value with the same rings
abalone_cleaned[, Height := replace(Height, is.na(Height), median(Height, na.rm = TRUE)),
                by = Rings]


## ----Size boxplot--------------------------------------------------------------------------------------------------------------------------------------------------------
# detect if there are extreme outliers from the three size predictors
boxplot(abalone_cleaned$Length,abalone_cleaned$Diameter,abalone_cleaned$Height,
        col = "Blue",main="Box plot of Length, Diameter and Height")


## ----Plot Height vs Rings------------------------------------------------------------------------------------------------------------------------------------------------
#plot height ~ rings to detect outlier
abalone_cleaned%>%ggplot(aes(Height,Rings,color=Sex))+geom_point()+
  ggtitle("Plot of Height by Rings")


## ----Height Outlier cleaning---------------------------------------------------------------------------------------------------------------------------------------------
#remove the observation with extreme height value(>0.3) and 
abalone_cleaned<-abalone_cleaned%>%filter(Height<0.3)
##box plot the height afterwards
boxplot(abalone_cleaned$Height,xlab="Height",col = "Blue",
        main="Box plot of Height(after cleaning)")


## ----Size predictors Histogram-------------------------------------------------------------------------------------------------------------------------------------------
#see the distribution of the size predictors
plot_histogram(abalone_cleaned[,2:4],geom_histogram_args = list("fill" = "blue",bins=30), 
               nrow = 2L, ncol = 2L)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#let us see histogram 
ggplot(data=abalone_cleaned, aes(x=Diameter, group=Sex, fill=Sex)) +
    geom_density(adjust=1.5, alpha=.4)+ggtitle("Density of Diameter grouped by Sex")


## ----Weight outlier Detection--------------------------------------------------------------------------------------------------------------------------------------------
# detection of outliers from weight predictors
boxplot(abalone_cleaned$Whole.weight,abalone_cleaned$Shucked.weight,
        abalone_cleaned$Viscera.weight, abalone_cleaned$Shell.weight,col = "Blue")


## ----Plot W.w. vs S.w.---------------------------------------------------------------------------------------------------------------------------------------------------
# detection if whole weight > shucked weight with an ab-line with slope=1
qplot(abalone_cleaned$Whole.weight,abalone_cleaned$Shucked.weight, 
      col=abalone_cleaned$Sex)+geom_abline(slope=1)


## ----Filter Whole weight vs Shucked weight-------------------------------------------------------------------------------------------------------------------------------
# remove observations which whole weight smaler than shucked weight
abalone_cleaned<-abalone_cleaned%>%filter(Whole.weight>Shucked.weight)
#plot if outliers are removed
qplot(abalone_cleaned$Whole.weight,abalone_cleaned$Shucked.weight,
      col=abalone_cleaned$Sex)+geom_abline(slope=1)


## ----Filter of Shucked weight vs Viscera weight--------------------------------------------------------------------------------------------------------------------------
#plot to see if all shucked weight > viscera weight
qplot(abalone_cleaned$Shucked.weight,abalone_cleaned$Viscera.weight,
      col=abalone_cleaned$Sex)+geom_abline(slope=1)
#Remove the observation with shucked weight < viscera weight
abalone_cleaned<-abalone_cleaned%>%filter(Shucked.weight>=Viscera.weight)


## ----Wet weight vs Dry shell weight--------------------------------------------------------------------------------------------------------------------------------------
#detect if all wet shell weight > dry shell weight
Wet.Shell.Weight<-abalone_cleaned$Whole.weight-abalone_cleaned$Shucked.weight
qplot(Wet.Shell.Weight,abalone_cleaned$Shell.weight,col=abalone_cleaned$Sex)+
  geom_abline(slope=1)

## remove the observations which donot fulfill the requirement
abalone_cleaned<-abalone_cleaned%>%filter((Whole.weight-Shucked.weight)>=Shell.weight)


## ----Sex predictors to rings---------------------------------------------------------------------------------------------------------------------------------------------
# detection of prevalence 
summary(abalone_cleaned$Sex)
##plot the distribution of sex to rings
ggplot(data=abalone_cleaned, aes(x=Rings, group=Sex, fill=Sex)) +
    geom_density(adjust=1.5, alpha=.4)+ggtitle("Density of Rings grouped by Sex")


## ----Correlation of all predictors---------------------------------------------------------------------------------------------------------------------------------------
#correlation coefficients between all predictors
ggcorr(abalone_cleaned[,-1],label = TRUE, hjust=0.75,label_size = 3, label_round = 2) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)


## ----Correlation of Diameter and Length----------------------------------------------------------------------------------------------------------------------------------
# Correlation coefficients between Length and Diameter including Rings
pair<-abalone_cleaned%>%select(Sex,Diameter,Length,Rings)
ggpairs(pair,columns = 2:4,ggplot2::aes(colour=Sex))


## ----Outliers Diameter and Length----------------------------------------------------------------------------------------------------------------------------------------
# remove odd value with diamter and height 
abalone_cleaned<-abalone_cleaned%>%filter(!(Diameter>0.3&Length<0.2))
##plot diameter and height
qplot(Diameter,Length,data=abalone_cleaned,color=Sex)+geom_smooth(method="lm",col="Black")


## ----Correlation between weight predictors-------------------------------------------------------------------------------------------------------------------------------
# correlation between weight predictors including Rings
weight<-abalone_cleaned%>%select(Sex,Whole.weight,Shucked.weight,Viscera.weight,
                                 Shell.weight,Rings)
ggpairs(weight,columns = 2:6,ggplot2::aes(colour=Sex))


## ----VIF and Variance----------------------------------------------------------------------------------------------------------------------------------------------------
#VIF and variance of predictors
vif(lm(Rings~.,data=abalone_cleaned))
var(abalone_cleaned[,-1])


## ----Reduce level of Sex-------------------------------------------------------------------------------------------------------------------------------------------------
# reduce levels of Sex to infant(1) and not infant(0)
abalone_cleaned<-abalone_cleaned%>%mutate(Infant=ifelse(Sex=="I",1,0))%>%select(-Sex)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(abalone_cleaned)


## ----Spliting to train and test------------------------------------------------------------------------------------------------------------------------------------------
#split into the train(80%) and test set(20%)
index<-createDataPartition(abalone_cleaned$Rings,times = 1, p=0.2,list = FALSE)
train_set<-abalone_cleaned[-index,]
test_set<-abalone_cleaned[index,]


## ----PCA on train set----------------------------------------------------------------------------------------------------------------------------------------------------
# x_train as the train set without output column Rings
x_train<-train_set[,!names(train_set)%in%"Rings"]
# pca the x_train with scaling the predictor first
pca<-prcomp(x_train,scale. = T)
##Examine how many pinciple components should be used for modelling
summary(pca)
qplot(1:ncol(x_train),pca$sdev)+geom_line()


## ----PCA on test set-----------------------------------------------------------------------------------------------------------------------------------------------------
#choose the representitive components
x_train<-pca$x[,1:5]
#transform the test set
##x_test as the new test set and removing output Rings
x_test<-test_set[,!names(test_set)%in%"Rings"]
##stantardize the columns of test set and keep the first 6 columns
x_test<-sweep(as.matrix(x_test),2,colMeans(x_test))%*%pca$rotation
x_test<-x_test[,1:5]


## ----Naive RMSE----------------------------------------------------------------------------------------------------------------------------------------------------------
#mean value of Rings
mean<-mean(train_set$Rings)
#RMSE value using average Rings
naive_rmse<-RMSE(test_set$Rings,mean)
naive_rmse


## ----training with lm----------------------------------------------------------------------------------------------------------------------------------------------------
# train on x_train, no parameter is tuned
fit_lm<-train(x_train,train_set$Rings,method="lm",
              trControl=trainControl(method="cv",number=5,p=0.9))
## make the predition on x_test
pred_lm<-predict(fit_lm,x_test)
## calculate the RMSE 
lm_rmse<-RMSE(test_set$Rings,pred_lm)
lm_rmse


## ----training with kNN---------------------------------------------------------------------------------------------------------------------------------------------------
#train with kNN and tune the k value between 1 and 15:
fit_knn<-train(x_train,train_set$Rings,method="knn", tuneGrid=data.frame(k=seq(1,25,2)),
               trControl=trainControl(method="cv",number=5,p=0.9))
##show the optimized k
fit_knn$bestTune
ggplot(fit_knn,highlight = TRUE)
##make the prediction on test set
pred_knn<-predict(fit_knn,x_test)
## calculate the RMSE 
knn_rmse<-RMSE(test_set$Rings,pred_knn)
knn_rmse


## ----training with GamLoess----------------------------------------------------------------------------------------------------------------------------------------------
#train with gamLoess with tuning the span
fit_gam<-train(x_train,train_set$Rings,method="gamLoess",
               tuneGrid=expand.grid(span=seq(0.15,0.65,len=10),degree=1),
               trControl=trainControl(method="cv",number=5,p=0.9))
## with the optimized span value
fit_gam$bestTune
ggplot(fit_gam,highlight = TRUE)
## predict on test set
pred_gam<-predict(fit_gam,x_test)
## calculate the RMSE 
gam_rmse<-RMSE(test_set$Rings,pred_gam)
gam_rmse



## ----training with RadndomForest-----------------------------------------------------------------------------------------------------------------------------------------
#train with random forest model with tuning the parameter mtry from 1 to 6
fit_rf<-train(x_train,train_set$Rings,method="rf",importance=TRUE, 
              tuneGrid=data.frame(mtry=c(1,6,2)),trControl=trainControl(method="cv",number=5,p=0.9))
##the optimized mtry with plot and value
ggplot(fit_rf,highlight = TRUE)
fit_rf$bestTune
##predict on test set
pred_rf<-predict(fit_rf,x_test)
#RMSE value with the optimized parameter
rf_rmse<-RMSE(test_set$Rings,pred_rf)
rf_rmse



## ----List of RMSE--------------------------------------------------------------------------------------------------------------------------------------------------------
# Listing the RMSE result of all model
rmse_result<-t(tibble(naive_rmse,lm_rmse,knn_rmse,gam_rmse,rf_rmse))
rmse_result


## ----Final model---------------------------------------------------------------------------------------------------------------------------------------------------------
## with optimized k value
fit_knn$bestTune
# final RMSE
knn_rmse

