


rm(list = ls())
####################################################
# This is a helper script used to load all packages
####################################################

# define a vector that contains all of the required packages 
requiredPackages <- c(
  #"lessR"
  "knitr"
  ,"readxl"
  ,"writexl"
  ,"data.table"
  ,"ggplot2"
  ,"GGally"
  ,"ggthemes"
  ,"ggridges" # for the ridge plots
  ,"gridExtra"
  ,"grid"
  ,"kableExtra"
  ,"dplyr"
  ,"visdat"
  ,"stargazer"
  ,"naniar" # to check for missing data
  ,"mice" # for data imputation
  ,"campfin"
  ,"corrplot"
  ,"RColorBrewer"
  ,"plyr"
  ,"pROC"
  ,"broom"
  ,"tidyverse"
  ,"car" # for recode function
  ,"sur"
  ,"MASS"
  ,"VIM" # to plot missing values
  ,"scatterplot3d"
  # ,"Rcmdr"
  ,"HH"
  ,"foreign" # to read in SPSS and ARFF data
  ,"ca" # for correspondence analysis
  ,"FactoMineR" # for correspondence analysis
  ,"factoextra" # for pca plots
  ,"Rtsne" # for tsne models
  ,"MASS" # for non-metric MDS
  ,"lubridate" # for date transformations
  ,"NbClust" # to find the optimal number of clusters for kmeans
  ,"clustree" # to build the cluster node diagram
  ,"ClustOfVar" # to build hierarchical dendogram
  ,"cluster" # for the cluster plots
  ,"rstatix" # for proportion tests
  # ,"farff" # for reading arff file
  ,"cvTools" # explicit creation of folds for cross-validation
  ,"ModelMetrics" # used for precision-recall evaluation of classifiers
  # ,"keras" # for the autoencoders model
  # ,"Keras" # this one also installs tensorflow
  # ,"tensorflow" # for tensorflow neural networks
  ,"devtools" # for the github developer tools
  ,"Metrics" # for AUC and ROC curves
)

# check if the required packages are found in the list of installed packages
new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]

# if any required packages are missing, install them
if(length(new.packages)) install.packages(new.packages)

# load all of the required packages
lapply(requiredPackages, require, character.only = T)

###############################################################################
# This is a helper script used to load custom user defined functions
###############################################################################

###############################################################################
# gets descriptive statistics for every variable in the dataset
###############################################################################
get_desc <- function(x) {
  map(x, ~list(
    min = min(.x),
    max = max(.x),
    mean = mean(.x),
    sd = sd(.x)
  ))
}



###############################################################################
# check for missing and INF values
###############################################################################
checkErrors <- function(x) {
  
  #define the data frame
  df <- x
  
  # define column numbers
  colNums <- match(colnames(df),names(df))
  
  # check if any columns have NA values
  df_na <- as.data.frame(cbind(lapply(lapply(df, is.na), sum)))
  
  # check if any columns have INF values
  df_inf <- as.data.frame(cbind(lapply(lapply(df, is.infinite), sum)))
  
  # save results into a data frame
  df_check <- as.data.frame(cbind(colNums, df_na, df_inf))
  colnames(df_check) <- c("Col_Number", "Count_NA", "Count_INF")
  
  # find percentages
  df_check$Percent_NA   <- round(as.integer(df_check$Count_NA)/nrow(df) * 100, 1)
  df_check$Percent_INF  <- round(as.integer(df_check$Count_INF)/nrow(df) * 100, 1)
  
  # save new data frame with missing or INF values
  df_errs <- subset(df_check, Count_NA > 0 | Count_INF > 0)
  
  # convert to data frame
  df_errs <- as.data.frame(lapply(df_errs, unlist))
  
  if (sum(df_errs$Count_NA) > 0) {
    # sort by largest NA
    df_errs <- df_errs[order(-df_errs$Count_NA),]
    
    # check all columns
    outputTable <- kable(df_errs, caption = "Error Check Table") %>%
      kable_styling(latex_options = "HOLD_position")
    
    return(outputTable)
  } else {
    noErrs <- paste0("There are no missing data elements")
    return(noErrs)
  }
}




########## read in source data ###########
setwd("~/Data Science/MSDS_411/Module_9/Assignment 4/")

# set data source file name
file_name_data_source_train <- "assignment-4-option-1-training.csv"
file_name_data_source_test <- "assignment-4-option-1-test.csv"

df_train<-read_csv(file_name_data_source_train)
df_test<-read_csv(file_name_data_source_test)

# call UDF to check for missing and INF values for train set
checkErrors(df_train)

# check for missing values using library(naniar)
gg_miss_var(df_train)

# plot missing data using library(VIM)
aggr_plot <- aggr(df_train
                  ,col = c('navyblue','red')
                  ,numbers=TRUE
                  ,sortVars=TRUE
                  ,labels=names(df_train)
                  ,cex.axis=.7
                  ,gap=3
                  ,ylab=c("Histogram of missing data","Pattern"))



# call UDF to check for missing and INF values for train set
checkErrors(df_test)

# check for missing values using library(naniar)
gg_miss_var(df_test)

# plot missing data using library(VIM)
aggr_plot <- aggr(df_test
                  ,col = c('navyblue','red')
                  ,numbers=TRUE
                  ,sortVars=TRUE
                  ,labels=names(df_train)
                  ,cex.axis=.7
                  ,gap=3
                  ,ylab=c("Histogram of missing data","Pattern"))




################################################################################
# this code chunk focuses on data cleansing and preparation
################################################################################


#remove observations with missing values

df_train<-df_train[complete.cases(df_train),]

df_test<-df_test[complete.cases(df_test),]

#check again for any missing values
checkErrors(df_train)

checkErrors(df_test)

#create price variable

df_train<-df_train%>%mutate (
  Price=Val/Quant
)
df_test<-df_test%>%mutate (
  Price=Val/Quant
)

df_test$Insp <- case_when(
  df_test$Insp == "ok" ~ 0
  ,df_test$Insp == "fraud" ~ 1
)


#Create x_train, x_test, and y_test dataframes

x_train<-select(df_train, c("Quant","Val",  "Price"))

x_test<-select(df_test, c("Quant","Val","Price"))


y_test<-select(df_test, c("Insp"))




################################################################################
# DBSCAN PRE-LIMS
################################################################################

library(dbscan)

#scale the data
x_train_dbscan<-scale(x_train)
x_test_dbscan<-scale(x_test)

set.seed(123456789)

#code below adjusted from kNNdistplot. source code from https://rdrr.io/cran/dbscan/src/R/kNNdist.R

kNNdistplot_v2 <- function(x, k, ...) {
  kNNdist <- sort(kNNdist(x, k , ...))
  plot(
    sort(kNNdist),
    type = "l",
    ylab = paste(k, "-NN distance", sep = ""),
    xlab = "Points (sample) sorted by distance",
    ylim=c(0,1),
    xlim=c(120000,130000)
  )
}

kNNdistplot_v2(x_train_dbscan,k=5)
abline(h=0.25,lty=2)



################################################################################
# DBSCAN 
################################################################################

set.seed(123456789)

#run dbscan with scaled data
d<-dbscan::dbscan(x_train_dbscan,0.25,30)
#clear memory as much as possible without deleting all the data
gc()

#make prediction on test set
predictions<-predict(d,x_test_dbscan ,x_train_dbscan )

#create predictions dataframe
predictions_df<-as.data.frame(predictions)


#isolate anomalies as predictions that fall into cluster 0 (per the documentation
#https://www.rdocumentation.org/packages/dbscan/versions/1.1-10/topics/dbscan
#predict() can be used to predict cluster memberships for new data points. 
#A point is considered a member #of a cluster if it is within the eps neighborhood of a member of the cluster (Euclidean distance is #used). 
#Points which cannot be assigned to a cluster will be reported as noise points (i.e., cluster ID 0)

predictions_df$predictions<-ifelse(predictions_df$predictions==0,1,0)

#Get F1 score
f1_score<-f1Score(y_test$Insp, predictions_df$predictions, cutoff = 0.5)
#print F1 score
f1_score

#get confusion matrix using table
table(actual=y_test$Insp, prediction=predictions_df$predictions)


################################################################################
# ISOLATION FORESTS - using h2o
################################################################################


library(h2o)
h2o.init()#initiate h2o

#create h2o train and test sets
train_h2o_iso = as.h2o(x_train)
test_h2o_iso = as.h2o(x_test)

#run isolation forest model
model <- h2o.isolationForest(training_frame = train_h2o_iso,
                             sample_rate = 1,
                             max_depth = 20,
                             seed=123456789,
                             ntrees = 200)

#get isolation scores
score <- h2o.predict(model, test_h2o_iso)


islation_scores<-as.data.frame(score$predict)

#fraudulent predictions above 0.923 percentile =1, otherwise not fraud=0
predictions_itree_final<-ifelse(islation_scores$predict>=quantile(islation_scores$predict,0.923), 1, 0)

#calculate f1_score
f1_score<-f1Score(y_test$Insp,predictions_itree_final, cutoff = 0.5)

f1_score

#create confusion matrix
table(actual=y_test$Insp, predicted= predictions_itree_final)

#table(y_test$Insp)


############################################
###Autoencoder
############################################



library(h2o)

x_train_auto<-scale(x_train)
x_test_auto<-scale(x_test)
# initialize h2o cluser
h2o.init()

train_h2o = as.h2o(x_train_auto)
test_h2o = as.h2o(x_test_auto)

#run autoencoder model
model_autoencoder = h2o.deeplearning(x = 1:ncol(train_h2o),
                                     training_frame = train_h2o
                                     , model_id = "Test01"
                                     , autoencoder = TRUE
                                     , reproducible = TRUE
                                     , seed = 42
                                     , hidden = c(25, 2, 25)
                                     , epochs = 100
                                     , activation ="Tanh")


#get anomaly MSEs for each observations of train set
anomaly_mse = h2o.anomaly(model_autoencoder
                          , train_h2o
                          , per_feature = FALSE) %>% as.data.frame()



#create anomaly threshold to be used to isolate anomalies in the test set.
threshold = quantile(anomaly_mse$Reconstruction.MSE, probs = 0.923)

#table(threshold)

#create anomaly MSE scores for the test set
test_anomaly_mse = h2o.anomaly(model_autoencoder
                               , test_h2o
                               , per_feature = FALSE) %>% as.data.frame()


#create predictions based on threshold. 1=Fraud, 0= not fraud
predictions_autoencoder_final<-ifelse(test_anomaly_mse>=threshold, 1, 0)

#create F1 score
f1_score<-f1Score(y_test$Insp,predictions_autoencoder_final, cutoff = 0.5)

f1_score


#create confusion matrix
table(actual=y_test$Insp, predicted= predictions_autoencoder_final)






