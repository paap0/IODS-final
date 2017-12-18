#Paula paap0 Bergman Nov 2017 Week 3
#Exercise 3 Data wrangling part
#The orginal dataset from F. Pagnotta & H. M. Amran (2008). Using Data Mining To Predict Secondary
#School Student Alcohol Consumption. Department of Computer Science,
#University of Camerino. Referred 05/02/17.
#https://archive.ics.uci.edu/ml/datasets/STUDENT+ALCOHOL+CONSUMPTION

#HOWEVER, for the exercise unzipped from the folder provided kindly by 
#assistant Anna (see IODS discussion forum for details)


############################
#Exercise 3 DATA WRANGLING #
############################

library(dplyr)

#1..zip-file downloaded, files unzipped, the two files ectracted and moved in my folder

#2.New script created. Name, date and short description written

#3.Set working directory
setwd("~/GitHub/IODS-project/data")

#3.Read in data
math <- as.data.frame(read.table('student-mat.csv', sep=';', header = TRUE))
portugese <- as.data.frame(read.table('student-por.csv', sep=';', header = TRUE))

#3.Explore the structure and dimensions with glimpse().
glimpse(math)#395 observations and 33 variables
glimpse(portugese)#649 observations and 33 variables

#4.Define the identifiers to be used in joining the tables as described in point 4
join_by<-c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

#4.Join the datasets based on the identifier columns
mathpor <- inner_join(math, portugese, by = join_by, suffix = c(".math", ".portugese"))

#4.Explore the structure and dimensions
glimpse(mathpor)#382 observations and 53 variables
#4.Merging results in numerous duplicated variables as can be seen
colnames<-colnames(mathpor)
colnames

#5.Create a data frame with only the joined columns
alc<-dplyr::select(mathpor,one_of(join_by))
glimpse(alc)#382 observations and 13 variables

#5.To combine the "duplicated" answers firstly define the ones not used for joining
nonid_col<-colnames(math)[!colnames(math) %in% join_by]
#5.Let's look at their names
nonid_col

#5.By for looping select two collumns from mathpor with the same name and
#if the first is numeric calculate a rounded average of the two
#if it in not numeric, then include the first of the two 

for(colnames in nonid_col) {
    two_columns<-dplyr::select(mathpor,starts_with(colnames))
    first_column<-dplyr::select(two_columns,1)[[1]]
    
    if(is.numeric(first_column)){
       alc[colnames]<-round(rowMeans(two_columns))      
    }  
       else {
       alc[colnames]<-first_column
      }
}

#5.Checking the structure of the generated dataset
glimpse(alc) #382 observations 33 variables

#6.Create variable alc_use by averaging weekdays and weekends consuption
alcall <- mutate(alc, alc_use = (Dalc + Walc) / 2)

#6. Define a logical column referring to high_use based on the alc_use
alc <- mutate(alc, high_use = alc_use > 2)

#7. Save and check the created, joined and modified dataset 
write.csv(alc, file = "alc.csv", row.names = FALSE)

alctest<-read.csv(file="alc.csv", header=TRUE)
dim(alctest)#should be 382 obs and 35 variables: Correct!
head(alctest)


