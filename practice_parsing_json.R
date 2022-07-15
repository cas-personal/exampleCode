######DATA SUMMARY######

#This data was downloaded from data.world to practice handling json data in R
#The data is the all bills introduced by the 1997-1998 and 2017-2018 congress. The two sets were chosen among the rest of the options as they are exactly 20 years apart. 

######LOADING DATA (original)#########
install.packages("rjson")
library(rjson)

#"/Downloads/bills/hconres/hconres1"
#folder/subfolder/specificfolder will be our naming convention 

#first looking at the folder called bills which holds 2017-2018 congressional data
setwd() #set your working directory here
subFolderList <-list.files(path = paste(getwd(),"/bills",sep=""))

#there is a nested folder structure in here each with additional folders. See sample path above to get to the lowest level folder.

#thought process:
# step 1: get the list of specificfolders within the subfolder which is element [i] in our initial list defined above
# step 2: cycle through the specific folder list by looking at element [j]
# step 3: cycle through and read the files in subfolder [i] specific folder [j] (this was the initial thought although it was discovered that only one data.json file lives in each specificfolder. This step is no needed for the final version of this code)

i<-1
j<-1

for (i in 1:length(subFolderList)) {
  directory<-paste(paste(getwd(),"/bills/",sep=""),subFolderList[i],sep="")
  specificFolderList<-list.files(path = directory )
  for (j in 1:length(specificFolderList)) {
    subdirectory<-paste(directory,"/",specificFolderList[j],sep="")
    fileList<-list.files(path = subdirectory)
    assign(paste(subFolderList[i],"_",specificFolderList[j],sep=""), as.data.frame.complex(fromJSON(file = paste(subdirectory,"/data.json",sep="")))) 
  }
}

#troubleshooting notes
#actions and committee subsections do not have the same number of arguments in their entries so we are using the .complex version of as.data.frame


#next looking at the folder called bills which holds 1997-1998 congressional data
subFolderList <-list.files(path = paste(getwd(),"/congress",sep=""))

#the 1997 data has multiple files within the specificFolders. We'll need to isolate the data.json files
i<-1
j<-1

for (i in 1:length(subFolderList)) {
  directory<-paste(paste(getwd(),"/congress/",sep=""),subFolderList[i],sep="")
  specificFolderList<-list.files(path = directory )
  for (j in 1:length(specificFolderList)) {
    subdirectory<-paste(directory,"/",specificFolderList[j],sep="")
    assign(paste("1997_",subFolderList[i],"_",specificFolderList[j],sep=""), as.data.frame.complex(fromJSON(file = paste(subdirectory,"/data.json",sep="")),fix.empty.names = TRUE)) 
  }
}

#after working with these data locally in RStudio it's clear that even one year is too much data to be loaded into R. We'll look at one bill type at a type ex. hjres(house joint resolution)

######LOADING DATA (subset)#########
library(rjson)

#first looking at the folder called bills which holds 2017-2018 congressional data
  #as we load the data in we will log how many bills we are loading for later
j<-1
num_2017_bills<-0

directory<-paste(getwd(),"/bills/hjres",sep="")
specificFolderList<-list.files(path = directory )
  for (j in 1:length(specificFolderList)) {
    subdirectory<-paste(directory,"/",specificFolderList[j],sep="")
    fileList<-list.files(path = subdirectory)
    assign(paste("2017_hjres_",specificFolderList[j],sep=""), as.data.frame.complex(fromJSON(file = paste(subdirectory,"/data.json",sep="")))) 
    num_2017_bills<-num_2017_bills+1
  }


#next looking at the folder called bills which holds 1997-1998 congressional data
  #as we load the data in we will catalog the bill id in a list 
#the 1997 data has multiple files within the specificFolders. We'll need to isolate the data.json files
j<-1
num_1997_bills<-0
directory<-paste(getwd(),"/congress/hjres",sep="")
specificFolderList<-list.files(path = directory)
  for (j in 1:length(specificFolderList)) {
    subdirectory<-paste(directory,"/",specificFolderList[j],sep="")
    assign(paste("1997_hjres_",specificFolderList[j],sep=""), as.data.frame.complex(fromJSON(file = paste(subdirectory,"/data.json",sep="")),fix.empty.names = TRUE)) 
    num_1997_bills<-num_1997_bills+1
  }

#####DATA PROFILING######
print((num_2017_bills-num_1997_bills)/num_1997_bills)
#bills increase 20.69% from 1997 to 2017

#creating a data frame of the subject areas covered to look at the focus of the bills across the years

#2017 subjects
subjects_2017<-c()
subjects_1997<-c()
dfList<-names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
for (i in 1:length(dfList)){
  if (substr(dfList[i],1,4)=="2017") {
    subjects_2017<-c(subjects_2017,unlist(get(dfList[i])[19,]))#elements of the dfList elements are vectors and need to be transformed before being added into a list
  } else {
    subjects_1997<-c(subjects_1997,unlist(get(dfList[i])[21,]))
  }
}

tab<-table(subjects_1997)
cum_sum_tab<-cumsum(tab)

#table is then written out in a csv for use in a Tableau Visualization
