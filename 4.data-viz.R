#Data-viz

library(ggplot2)
library(ggvis)
library(tidyverse)
library(reshape2)

ggplot(df, aes(freq, fill_rate)) +
  geom_point(aes(color = FY)) +
  geom_smooth(se = FALSE) +
  labs(title = "")


ggplot(data = melt(df), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')


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


df %>% ggvis(~fill_rate, ~freq, fill = ~Region) %>% layer_points()
df %>% ggvis(~fill_rate, ~freq, fill = ~Sector) %>% layer_points()