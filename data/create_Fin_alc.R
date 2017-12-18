#Paula paap0 Bergman Dec 2017 Final assignment
#Final assignment: Data wrangling
#The orginal dataset from F. Pagnotta & H. M. Amran (2008). Using Data Mining To Predict Secondary
#School Student Alcohol Consumption. Department of Computer Science,
#University of Camerino. Referred 05/02/17.
#https://archive.ics.uci.edu/ml/datasets/STUDENT+ALCOHOL+CONSUMPTION

####################################
####################################
##FINAL ASSIGNMENT DATA WRANGLING ##
####################################
####################################

library(dplyr)
#zip-file downloaded, files unzipped, the two files ectracted and moved in my folder

#New script created. Name, date and short description written

#Set working directory 
setwd("~/GitHub/IODS-final/data")

#Read datasets
math <- as.data.frame(read.table('student-mat.csv', sep=';', header = TRUE))
portugese <- as.data.frame(read.table('student-por.csv', sep=';', header = TRUE))

#Explore structure and dimensions with glimpse() to assure that the datasets are ok
glimpse(math)#395 observations and 33 variables
glimpse(portugese)#649 observations and 33 variables

#Define identifiers to be used in joining the tables according to my preliminary interest
join_by<-c("sex","age","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery")

#Join the datasets based on the identifier columns
mathpor <- inner_join(math, portugese, by = join_by, suffix = c(".math", ".portugese"))

#Explore the structure and dimensions
glimpse(mathpor)#434 observations and 57 variables

#Check colnames in the merged dataset
colnames(mathpor)

#Create a data frame with only the joined columns
alc<-dplyr::select(mathpor,one_of(join_by))
glimpse(alc)#434 observations and 9 variables

#To combine the "duplicated" answers firstly define the ones not used for joining
nonid_col<-colnames(math)[!colnames(math) %in% join_by]

#Let's look at their names
nonid_col

#By for looping select two collumns from mathpor with the same name and
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

#Checking the structure of the generated dataset
glimpse(alc) #434 observations 33 variables

##################################################################################
##################################################################################
##Several differently defined alcohol-usage variables created and investigated 
##as well as several different variables and categorizing values tested to both meet 
##my ideas of appropriate levels as well as assure the right numbers of observations 
##in groups and any associations with each other
##e.g. by plotting, tabling and using summaries (scripts not shown)
##################################################################################
##################################################################################

#Create variables for weekday and weekend use
#defining high use as being more than "low"(2) EITHER at weekdays OR at weekends
alc$alclotW_or_WE<-as.factor(ifelse(alc$Dalc>2|alc$Walc>2,1,0))

#Categorize or re-categorize variables

#Age groups 16 or younger, 17, 18 or older(adult)
summary(alc$age)
alc$agecat<-cut(alc$age,breaks=c(0,16,17,22))

#Mother´s education none until 4th grade,5-9th grade,secondary, higher education 
table(alc$Medu)
alc$Meducat<-cut(alc$Medu, breaks=c(0,2,3,4),include.lowest = TRUE)
table(alc$Meducat)#checking group sizes

#Father´s education none until 4th grade,5-9th grade,secondary, higher education 
table(alc$Fedu)
alc$Feducat<-cut(alc$Fedu, breaks=c(0,2,3,4),include.lowest = TRUE)
table(alc$Feducat)#checking group sizes

#Family realtionships poor or bad, average, good or very good (I want to categorize
#separately the "in-between-persons")
table(alc$famrel)
alc$Famrelcat<-cut(alc$famrel, breaks=c(0,2,3,5))
table(alc$Famrelcat)

#Going out very low-low, average, often-very often (I want to categorize
#separately the "in-between-persons")
table(alc$goout)
alc$gooutcat<-cut(alc$goout, breaks=c(0,2,3,5))
table(alc$gooutcat)

#Health status very bad-bad, average, good-very good (I want to categorize
#separately the "in-between-persons")
table(alc$health)
alc$healthcat<-cut(alc$health, breaks=c(0,2,3,5))
table(alc$healthcat)

#Failures either none or one or more 
table(alc$failures)#(I assume that the majority do not fail and if so, it does not really matter how many times)
alc$failurescat<-as.factor(ifelse(alc$failures>0,1,0))
table(alc$failurescat)

#Absences based on 25% and 75%, thus the ones hardly ever absent and the ones
#often or very often absent differ from the "in between 1-6h people" (I assume)
summary(alc$absences)
alc$absencescat<-cut(alc$absences,breaks=c(0,1,6,45),include.lowest = TRUE)
table(alc$absencescat)

#Final grade based on approx 25%,50%,75%
summary(alc$G3)
alc$G3cat<-cut(alc$G3,breaks=c(0,10,12,14,18), include.lowest = TRUE)
table(alc$G3cat)

#Create a smaller dataset
dfalc<-alc[c("sex","agecat","Pstatus","Meducat","Feducat","Mjob","Fjob","guardian","famsup",
                       "higher","romantic","activities","Famrelcat","gooutcat",
                       "healthcat","failurescat","absencescat", "G3cat","G3","alclotW_or_WE")]

#Rename the variables to facilitate interpretation later on
dfalc<-plyr::rename(dfalc, c("sex"="Gender",
                          "agecat"="Agegroup",
                          "Pstatus"="Parents together",
                          "Meducat"="Education_M",
                          "Feducat"="Education_F",
                          "Mjob"="Job_M",
                          "Fjob"="Job_F",
                          "guardian"="Guardian",
                          "famsup"="Familial_support",
                          "higher"="Education_pos",
                          "romantic"="In_relationship",
                          "activities"="Activities",
                          "Famrelcat"="Family_relationships",
                          "gooutcat"="Going_out",
                          "healthcat"="Healthgroup",
                          "failurescat"="Class_failures",
                          "absencescat"="Absencegroup", 
                          "G3cat"="Performancegroup",
                          "G3"="Final_Grade",
                          "alclotW_or_WE"="High_alcohol"))


#All variables except for the Final_Grade are factors, thus convert
dfalc<-mutate_all(dfalc,as.factor)
#And Final_Grade to numeric (a little clumsy)
dfalc$Final_Grade<-as.numeric(dfalc$Final_Grade)

#Check that the 
str(dfalc)#434 and 19 factor and one numeric variables

#Check for NA:s
table(is.na(dfalc))#ok, no further worries on that

#Save and check the created, joined and modified dataset 
write.csv(dfalc, file = "alccatfiso.csv", row.names = FALSE)

alctest<-read.csv(file="alccatfiso.csv", header=TRUE)
dim(alctest)#should be 434 obs and 20 variables: Correct!
str(alctest)#3 integer values, most likely due to csv-format, remeber later!




