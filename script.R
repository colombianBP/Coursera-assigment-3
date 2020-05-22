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

dataframe2<-data.frame(subjectid=dataframe3$subjectid,
                      activity=dataframe3$activity,
                      datatype=dataframe3$datatype,
                      dodyaccxmean=sapply(dataframe3$bodyaccx,function(x) mean(x[[1]])),
                      dodyaccymean=sapply(dataframe3$bodyaccy,function(x) mean(x[[1]])),
                      dodyacczmean=sapply(dataframe3$bodyaccz,function(x) mean(x[[1]])),
                      dodyaccxsd=sapply(dataframe3$bodyaccx,sd),
                      dodyaccysd=sapply(dataframe3$bodyaccy,sd),
                      dodyacczsd=sapply(dataframe3$bodyaccz,sd),
                      dodygyroxmean=sapply(dataframe3$bodygyrox,function(x) mean(x[[1]])),
                      dodygyroymean=sapply(dataframe3$bodygyroy,function(x) mean(x[[1]])),
                      dodygyrozmean=sapply(dataframe3$bodygyroz,function(x) mean(x[[1]])),
                      dodygyroxsd=sapply(dataframe3$bodygyrox,sd),
                      dodygyroysd=sapply(dataframe3$bodygyroy,sd),
                      dodygyrozsd=sapply(dataframe3$bodygyroz,sd),
                      observationmean=sapply(dataframe3$observation,function(x) mean(x[[1]])),
                      observationsd=sapply(dataframe3$observation,sd))


