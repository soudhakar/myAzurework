setwd("C:\\Soudhakar\\Projects\\Personal\\Class training")
library(dplyr)

#read the file
df = read.csv("2015.csv")

summary(df)
str(df)
colnames(df)
nrow(df)
ncol(df)

#subset
india <- df%>% filter(Country =="India")

#select column

hap <-df %>% 
  select(Country, starts_with("Happiness"))

#summarise
All <- df %>% summarise(mean = mean(Happiness.Score))
region <- df %>%group_by(Region) %>%summarise(mean = mean(Happiness.Score))


# sort my summary
region <- df %>%group_by(Region) %>%summarise(mean = mean(Happiness.Score)) %>% arrange(desc(mean))

  
# multiple condition

top100 <- df %>% filter(Happiness.Rank<=100) %>%
  group_by(Region) %>%summarise(Happ =mean(Happiness.Score),n=n()) %>% arrange(desc(Happ,n))


top100 <- df %>% filter(Happiness.Rank<=100, Region =="Eastern Asia") %>%
  group_by(Country) %>%summarise(Happ =mean(Happiness.Score),n=n()) %>% arrange(desc(Happ,n))

#case: How many countries from Southeastern Asia has happiness insex more than 5 and genrosity more than 0.5


library(ggplot2)


# visualisation


# bar graph
p <- ggplot(data =top100,aes( x=Country,y= Happ)) + geom_bar(stat="identity")
plot(p)

# piechart

regsumm <- df %>% group_by(Region) %>% summarise(n=n())

bp<- ggplot(regsumm, aes(x="", y=n, fill=Region))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie


library(googleVis)

Pie <- gvisPieChart(regsumm)
plot(Pie)


# stacked bar

#top 10 country  by region

top10R <- df %>% select(Country, Region, Happiness.Rank, Happiness.Score) %>% filter(Happiness.Rank<=10) %>% group_by(Country, Region) %>% summarise(mean=mean(Happiness.Score))



#Scatter chart


#Make beautiful charts
p+geom_bar(stat="identity" , color="blue", fill="white")+
  theme_minimal()



# map charts

