---
title: "Getting and Cleaning Data. Final Project"
author: "Ruben Artime"
date: "Friday, August 22, 2014"
output: html_document
---

 Packages
 --------
 The script uses the package *plyr*

 Raw data
 --------
 The script reads the following data files
   * X_train.txt.- contains 70% of the raw data
   * X_text.txt.- contains 30% of the raw data
   * features.txt.- contains the list of features observed in each row
   * y_train.txt.- labels the actvity of the train data
   * y_test.txt.- labels the actvity of the test data
   * activity_labels.txt.- names of the activities


 Variables
 ---------
 The script uses the following variables:
   * dfTrain.- data frame with the train data
   * dfTest.- data frame with the test data
   * dfAll.- data frame with the data from both train and test
   * dfFeat.-data frame with the features vector
   * m/s.- contains the indices of the features vector that correspond with means or std
   * m_names/s_names.- contains the names of the features mean and std
   * dfFilt.- data frame containing only the information related to means or standard deviations
   * lbTrain.- labels for the train data
   * lbTest.- lables for the test data
   * lbAll.- all the labels
   * lbNames.- names of the six different labels
   * act_names.- data frame that merges the information of the labels of each observation with the name of the label
   * res.- final data frame with the average of each variable for each activity and each subject 

 Transformations
 ---------------
 The script first read the train and test data and merge the information in one data frame (dfAll).

```{r}
  dfTrain<-read.table("./X_train.txt")
  dfTest<-read.table("./X_test.txt")
  dfAll<-merge(dfTrain,dfTest,all=TRUE,sort=FALSE)
```


 Then it reads the features vector and extract the columns indices and names that correspond to mean and standard deviation variables.

```{r}  
  dfFeat<-read.table("./features.txt")  
  m<-grep("-mean()",dfFeat$V2,fixed=TRUE)
  m_names<-grep("-mean()",dfFeat$V2,value=TRUE,fixed=TRUE)
  s<-grep("-std()",dfFeat$V2,fixed=TRUE)
  s_names<-grep("-std()",dfFeat$V2,value=TRUE,fixed=TRUE)
```

 Once known the columns of the data frame that correspond to means and std then the script extracts the information from the data frame into a different data frame *dfFilt*

```{r}
dfFilt<-dfAll[,c(m,s)] 
```

 In the second stage, the script reads the labels indices and names for the train and test data.
 
```{r}
  lbTrain<-read.table("./y_train.txt")
  lbTest<-read.table("./y_test.txt")
  lb<-c(lbTrain[,1],lbTest[,1])
  lbAll<-data.frame(lb) 
  lbNames<-read.table("./activity_labels.txt") 
  act_names<-merge(lbAll,lbNames,by.x="lb",by.y="V1",sort=FALSE)  
```

  With the following line, the script creates a column in the data frame to set the activity name of each observation

```{r}
dfFilt$act_name<-act_names$act_name
```

 Finally, to calculate an independent tidy data set with the average of each variable for each activity and each subject, the script runs a for loop to calculate the means of each variable grouped by the activity and store in a final data frame (*res*) that is written out to a file.

```{r}
  res<-ddply(dfFilt,.(act_name),summarize,mean(dfFilt[,1]))
  names(res)<-c("Activity",paste(names(dfFilt)[1],"/MEAN"))
```  
```{r}
  for (i in 2:(ncol(dfFilt)-1))
  {
    cat("Calculating means of ", i, "\n")
    auxdf<-ddply(dfFilt,.(act_name),summarize,mean(dfFilt[,i]))
    res[,paste(names(dfFilt)[i],"/MEAN")]<-auxdf[,2]
  }  
```
