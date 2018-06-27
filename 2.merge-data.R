#Merge Data

#Load data for vds invitation info
library(readxl)
library(plyr)
library(tidyverse)

data <- read_excel("C:/Users/tshu/Downloads/data.xls")
View(data)

#count how many unique post(country) in the dataset
unique(data$Post)

post_med <- read_csv("C:/Users/tshu/Downloads/post.csv")

#drop the first row of all countries and the last row of total
#only keep the post name and freq columns
post_med <- post_med[-c(1, 96), -c(2,3)]

#merge the two dataset by Post name
df <- join(data, post_med, by = "Post", type="left")
df <- unique(df)

#filter the data by Enter on duty
df <- df[df$`Enter on Duty` > 0,]

summary(df)

ggplot(data = df) + geom_point(mapping = aes(x=VR, y=Invited, color=Q))

ggplot(data = df) + 
  + geom_bar(mapping = aes(x = Region, fill = Sector), 
             position="dodge")

##
bar <- ggplot(data = df) + 
       geom_bar(mapping = aes(x = Region, fill = Sector), 
             show.legend = FALSE,
             width = 1)  
       theme(aspect.ratio = 1) + labs(x = NULL, y = NULL)
 
bar + coord_flip()
bar + coord_polar()

##
ggplot(df, aes(Region, freq)) +
  geom_point(aes(colour = Sector))
