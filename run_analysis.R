#Here are the data for the project:
#  
#  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
#You should create one R script called run_analysis.R that does the following. 
#
#Merges the training and the test sets to create one data set.
## LOS DOS CONJUNTOS SON IGUALES, TEST TIENE EL 30% DE LOS DATOS Y TRAIN EL 70%
## SE CONCATENAN PONIENDO UNO DETRAS DEL OTRO

#Extracts only the measurements on the mean and standard deviation for each measurement. 
## CADA REGISTRO (2947+7352=10299) DEL DATASET RESULTANTE TIENE 561 VALORES
## PARA SABER QUE COLUMNAS SON MEAN() Y CUALES SON STD() HAY QUE LEER FEATURES.TXT

#Uses descriptive activity names to name the activities in the data set
# CADA REGISTRO EN EL DATA SET SE CORRESPONDE CON UNA DE 6 ACTIVIDADES
# LOS FICHEROS y_test/y_train tienen para cada registro el id de la actividad

#Appropriately labels the data set with descriptive variable names. 

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

cleanData <- function() { 
  ## Read train data
  print("Reading train data...")
  dfTrain<-read.table("./X_train.txt")
  
  ## Read test data
  print("Reading test data...")
  dfTest<-read.table("./X_test.txt")
  
  ## Merge
  print("Merging train and test data...")
  dfAll<-merge(dfTrain,dfTest,all=TRUE,sort=FALSE)
  
  # Read all the features
  print("Reading features...")
  dfFeat<-read.table("./features.txt")
  
  #Vector containing the indices for mean()
  print("Calculating mean and std positions...")
  m<-grep("-mean()",dfFeat$V2,fixed=TRUE)
  m_names<-grep("-mean()",dfFeat$V2,value=TRUE,fixed=TRUE)
  #Vector containing the indices for std()
  s<-grep("-std()",dfFeat$V2,fixed=TRUE)
  s_names<-grep("-std()",dfFeat$V2,value=TRUE,fixed=TRUE)
  
  #Extract the means and stds
  print("Extracting means and standard deviations...")
  dfFilt<-dfAll[,c(m,s)]  
  #Appropriately labels the data set with descriptive variable names.
  names(dfFilt)<-c(m_names,s_names)
  
  print("Reading labels...")
  # Labels of the train data
  lbTrain<-read.table("./y_train.txt")
  # Labels of the test data
  lbTest<-read.table("./y_test.txt")
  ## Merge labels
  lb<-c(lbTrain[,1],lbTest[,1])
  lbAll<-data.frame(lb)
  
  print("Reading activity names...")
  
  #Read names
  lbNames<-read.table("./activity_labels.txt") 
  act_names<-merge(lbAll,lbNames,by.x="lb",by.y="V1",sort=FALSE)
  names(act_names)<-c("act_id","act_name")
  
  dfFilt$act_name<-act_names$act_name
  
  cat("Calculating means of ", 1, "\n")
  res<-ddply(dfFilt,.(act_name),summarize,mean(dfFilt[,1]))
  names(res)<-c("Activity",paste(names(dfFilt)[1],"/MEAN"))
  print("Entering bucle...")
  for (i in 2:(ncol(dfFilt)-1))
  {
    cat("Calculating means of ", i, "\n")
    auxdf<-ddply(dfFilt,.(act_name),summarize,mean(dfFilt[,i]))
    res[,paste(names(dfFilt)[i],"/MEAN")]<-auxdf[,2]
  }  
  write.table(res,"./tidymeans.txt",sep=" ",row.name=FALSE)
}

