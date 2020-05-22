#Loading the dataset and descompressing the zip
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip','dataset.zip')
unzip('dataset.zip')
#setting wd to the test file
setwd('./UCI HAR Dataset/test')

#Get the activity and subject performing it
labels<-scan('y_test.txt')
subject<-scan('subject_test.txt')

#Takes a line of the file, and splits it into its numeric values
#returns the corresponding numeric vector
trimmer<-function(x){
  splited<-strsplit(x,' ')[[1]]
  counter<-1
  vec<-vector(mode='numeric')
  for(i in splited){
    if(i!=''){
      vec[counter]<-as.numeric(i)
      counter<-counter+1
    }
  }
  vec
}

#Opens the file and sets each line as a numeric vector
con<-file('X_test.txt')
archivo<-readLines(con)
close(con)
data<-lapply(archivo,trimmer)


filenames<-list.files('./Inertial Signals')

#makes readable names for the files in Inertial Singals/
filenames2<-vector(mode = 'character')
counter=1
for(file in filenames){
  index<-gregexpr('*\\.',file)[[1]][1]
  file<-substr(file,1,index-6)
  file<-gsub('_','',file)
  filenames2[counter]<-file
  counter<-counter+1
}

#Reads each file in Inertial Signals, reads it, transforms it 
#into an integer vector list and saves that list as its respective 
#value
for(i in seq(1:9)){
  con<-file(paste0('./Inertial Signals','/',filenames[i]))
  datos<-readLines(con)
  close(con)
  datos<-lapply(datos,trimmer)
  assign(filenames2[i],datos)
}

#Sets the labels to its corresponding activity
labels2<-vector(mode='character')
counter=1
for(i in labels){
  k='ERROR'
  if(i==1){
    k='walking'
  }else if(i==2){
    k='walking_upstairs'
  } else if(i==3){
    k='walking_downstairs'
  } else if(i==4){
    k='sitting'
  } else if(i==5){
    k='standing'
  } else if(i==6){
    k='laying'
  }else{
    print('ERROR')
  }
  labels2[counter]=k
  counter<-counter+1
}

#Organizing test data on a dataframe
dataframe<-data.frame(subjectid=subject,
                      activity=labels2,
                      bodyaccx=I(bodyaccx),
                      bodyaccy=I(bodyaccy),
                      bodyaccz=I(bodyaccz),
                      bodygyrox=I(bodygyrox),
                      bodygyroy=I(bodygyroy),
                      bodygyroz=I(bodygyroz),
                      datatype=rep('test',length(labels2)),
                      observation=I(data))

#The code below is a copy-paste, does the same process
#on the train folder

setwd('./../train')

#Get the activity and subject performing it
labels<-scan('y_train.txt')
subject<-scan('subject_train.txt')

#Opens the file and sets each line as a numeric vector
con<-file('X_train.txt')
archivo<-readLines(con)
close(con)
data<-lapply(archivo,trimmer)


filenames<-list.files('./Inertial Signals')

#makes readable names for the files in Inertial Singals/
filenames2<-vector(mode = 'character')
counter=1
for(file in filenames){
  index<-gregexpr('*\\.',file)[[1]][1]
  file<-substr(file,1,index-6)
  file<-gsub('_','',file)
  filenames2[counter]<-file
  counter<-counter+1
}

#Reads each file in Inertial Signals, reads it, transforms it 
#into an integer vector list and saves that list as its respective 
#value
for(i in seq(1:9)){
  con<-file(paste0('./Inertial Signals','/',filenames[i]))
  datos<-readLines(con)
  close(con)
  datos<-lapply(datos,trimmer)
  assign(filenames2[i],datos)
}

#Sets the labels to its corresponding activity
labels2<-vector(mode='character')
counter=1
for(i in labels){
  k='ERROR'
  if(i==1){
    k='walking'
  }else if(i==2){
    k='walking_upstairs'
  } else if(i==3){
    k='walking_downstairs'
  } else if(i==4){
    k='sitting'
  } else if(i==5){
    k='standing'
  } else if(i==6){
    k='laying'
  }else{
    print('ERROR')
  }
  labels2[counter]=k
  counter<-counter+1
}

#Organizing train data on a dataframe
dataframe2<-data.frame(subjectid=subject,
                      activity=labels2,
                      bodyaccx=I(bodyaccx),
                      bodyaccy=I(bodyaccy),
                      bodyaccz=I(bodyaccz),
                      bodygyrox=I(bodygyrox),
                      bodygyroy=I(bodygyroy),
                      bodygyroz=I(bodygyroz),
                      datatype=rep('train',length(labels2)),
                      observation=I(data))

#Finally, we merge the two dataframes 
dataframe<-rbind(dataframe,dataframe2)

#As we only need dataframe3 all other objects in the enviroment 
#will be errased
list<-ls(all.names = T)
list<-list[!(list %in% 'dataframe')]
rm(list=list)

#the data we have is already organiced and tidy, but its hard to see 
#when many of our columns hold long vectors; a new one will be made 
#in which these columns hold the mean or standard deviation

dataframe2<-data.frame(subjectid=dataframe$subjectid,
                      activity=dataframe$activity,
                      datatype=dataframe$datatype,
                      bodyaccxmean=sapply(dataframe$bodyaccx,function(x) mean(x[[1]])),
                      bodyaccymean=sapply(dataframe$bodyaccy,function(x) mean(x[[1]])),
                      bodyacczmean=sapply(dataframe$bodyaccz,function(x) mean(x[[1]])),
                      bodyaccxsd=sapply(dataframe$bodyaccx,sd),
                      bodyaccysd=sapply(dataframe$bodyaccy,sd),
                      bodyacczsd=sapply(dataframe$bodyaccz,sd),
                      bodygyroxmean=sapply(dataframe$bodygyrox,function(x) mean(x[[1]])),
                      bodygyroymean=sapply(dataframe$bodygyroy,function(x) mean(x[[1]])),
                      bodygyrozmean=sapply(dataframe$bodygyroz,function(x) mean(x[[1]])),
                      bodygyroxsd=sapply(dataframe$bodygyrox,sd),
                      bodygyroysd=sapply(dataframe$bodygyroy,sd),
                      bodygyrozsd=sapply(dataframe$bodygyroz,sd),
                      observationmean=sapply(dataframe$observation,function(x) mean(x[[1]])),
                      observationsd=sapply(dataframe$observation,sd))

#To make a final dataframe, with the mean of each measurement, per subject and 
#activity, we first make a list of measurements, to then calculate the mean

values<-c("bodyaccxmean","bodyaccymean","bodyacczmean","bodygyroxmean","bodygyroymean",
          "bodygyrozmean","observationmean")

#we initialice the vectors that will make the columns of the dataframe
subjectid=vector(mode='numeric')
activity=vector(mode='character')
measure=vector(mode='character')
measuremean=vector(mode='numeric')


counter<-1
for(value in values){
  #here wa make a matrix that holds the mean of the measurement  for each activity(row)
  #and each subject(column)
  matrix<-tapply(dataframe2[,value], list(dataframe2$activity,dataframe2$subjectid), mean)
  for(id in colnames(matrix)){
    for(act in rownames(matrix)){
      #and put each value on its respective vector
      subjectid[counter]<-as.numeric(id)
      activity[counter]<-act
      measure[counter]<-value
      measuremean[counter]<-matrix[act,id]
      counter<-counter+1
    }
  }
}
#And finally use them to make the dataframe
dataframe3<-data.frame(subjectid=subjectid,activity=activity,measure=measure,measuremean=measuremean)

