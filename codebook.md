# Coursera-assigment-3
Repo for the peer reviwed asigment of the getting and cleaning data course

Data download and unzipp, first working on `test/` file
```
{
  download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip','dataset.zip')
  unzip('dataset.zip')
  setwd('./UCI HAR Dataset/test')
}
```
Reading labels and subject
```
}
  labels<-scan('y_test.txt')
  subject<-scan('subject_test.txt')
}
```
A function for reading and parsing data
```
{
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
}
```
Reading and parsing observations
```
}
  con<-file('X_test.txt')
  archivo<-readLines(con)
  close(con)
  data<-lapply(archivo,trimmer)
}
```
Raking readable names for the files on `Internal Signals/`
```
{
  filenames<-list.files('./Inertial Signals')
  filenames2<-vector(mode = 'character')
  counter=1
  for(file in filenames){
    index<-gregexpr('*\\.',file)[[1]][1]
    file<-substr(file,1,index-6)
    file<-gsub('_','',file)
    filenames2[counter]<-file
    counter<-counter+1
  }
{
```
Reading each file in Inertial Signals, transforming it into an integer vector list and saving that list as its respective value
```
{
  for(i in seq(1:9)){
    con<-file(paste0('./Inertial Signals','/',filenames[i]))
    datos<-readLines(con)
    close(con)
    datos<-lapply(datos,trimmer)
    assign(filenames2[i],datos)
  }
}
```
Making explicit labels
```
{
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
}
```
Organizing test data on a dataframe
```
{
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
}
```
Repeting the process for the `train/` folder
```
{
  setwd('./../train')

  labels<-scan('y_train.txt')
  subject<-scan('subject_train.txt')

  con<-file('X_train.txt')
  archivo<-readLines(con)
  close(con)
  data<-lapply(archivo,trimmer)

  filenames<-list.files('./Inertial Signals')

  filenames2<-vector(mode = 'character')
  counter=1
  for(file in filenames){
    index<-gregexpr('*\\.',file)[[1]][1]
    file<-substr(file,1,index-6)
    file<-gsub('_','',file)
    filenames2[counter]<-file
    counter<-counter+1
  }

  for(i in seq(1:9)){
    con<-file(paste0('./Inertial Signals','/',filenames[i]))
    datos<-readLines(con)
    close(con)
    datos<-lapply(datos,trimmer)
    assign(filenames2[i],datos)
  }

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
}
```
Making a joined dataframe
```
{
  dataframe<-rbind(dataframe,dataframe2)
}
```
Errasing all othe variables (except the main dataframe)
```
{
  list<-ls(all.names = T)
  list<-list[!(list %in% 'dataframe')]
  rm(list=list)
}
```
Making a dataframe that holds the means and standard deviations of each measurement
```
{
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
}
```
Initializing vectors that will construct the final dataframe of means for each measurement, per subject and activity.
also listing the measurement to loop over them
```
{
  values<-c("bodyaccxmean","bodyaccymean","bodyacczmean","bodygyroxmean","bodygyroymean",
            "bodygyrozmean","observationmean")

  subjectid=vector(mode='numeric')
  activity=vector(mode='character')
  measure=vector(mode='character')
  measuremean=vector(mode='numeric')
}
```
Creating a matrix with the mean of each activity and and subject id, and assigning the corresponding values to each vector
```
{
  counter<-1
  for(value in values){
    matrix<-tapply(dataframe2[,value], list(dataframe2$activity,dataframe2$subjectid), mean)
    for(id in colnames(matrix)){
      for(act in rownames(matrix)){
        subjectid[counter]<-as.numeric(id)
        activity[counter]<-act
        measure[counter]<-value
        measuremean[counter]<-matrix[act,id]
        counter<-counter+1
      }
    }
  }
}
```
And finally using them to make final dataframe
```
{
dataframe3<-data.frame(subjectid=subjectid,activity=activity,measure=measure,measuremean=measuremean)
}
```
