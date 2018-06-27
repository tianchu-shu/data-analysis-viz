#First Data Expoloration

#Load data for vds invitation info
library(readxl)
library(plyr)
library(tidyverse)

#Load data for calculating countries' medical friendness
med <- read_excel("C:/Users/tshu/Downloads/med.xls")
View(med)

# paste cells into one string, use "," as separator
post.string <- paste (med$`PCV Posts Approved for Invitation [ListOfPosts_PCV]` , collapse = ",")

# split string at ","
post.vector <- strsplit (post.string , "," )[[1]]

#get rid of . \n to prevent errors
post.vector.clean <- gsub("\n" , "" , post.vector)

#tabulate data
table(post.vector.clean)


rm(post.string)
rm(post.vector)

write.csv(table(post.vector.clean), file ="post.csv", sep = " ", row.names=FALSE)
