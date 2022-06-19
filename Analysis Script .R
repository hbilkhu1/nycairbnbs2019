##getting ready
#import packages
install.packages("tidyverse")
library(tidyverse)

#read csv file to R
abnyc <- read.csv("AB_NYC_2019.csv")
print(summary(abnyc))
head(abnyc)

#how big is the dataset?
dim(abnyc)

##data cleaning
#let's see the number of zeros in certain columns
colSums(abnyc == 0)

#we want to look at the available Airbnbs and non free ones
avail_abnyc <- filter(abnyc, availability_365 > 0 & price > 0) 

##exploratory & analysis
#average price of one night? range of prices?
mean(avail_abnyc$price)
range(avail_abnyc$price)

#which neighborhood group has the highest average price? Lowest?
avail_abnyc %>%
  group_by(neighbourhood_group) %>%
  summarize(length(neighbourhood_group), mean(price))

#what is the average price by room type?
avail_abnyc %>%
  group_by(room_type) %>%
  summarize(length(room_type), mean(price))

#plot price on graph 
g1 <- ggplot(avail_abnyc, aes(x=price)) + 
  geom_histogram(colour="black", fill="white") +
  scale_x_continuous(breaks = seq(0, 10000, by = 2000)) + 
  labs(title = "Price Distribution") +
  theme(plot.title = element_text(hjust=0.5))
g1

#plot price and availability on graph, facet by neighbourhood group
g2 <- ggplot(avail_abnyc, aes(x = price, y = availability_365))
g2 + geom_point() + facet_wrap(~neighbourhood_group) 

#price distribution is skewed heavy, let's see the number of high priced Airbnbs
high_bnb <- filter(avail_abnyc, price > 1000)
nrow(high_bnb)

#what is the average price of Airbnbs over $1,000 by neighborhood group?
high_bnb %>%
  group_by(neighbourhood_group) %>%
  summarize(length(neighbourhood_group), mean(price))

#let's look at these high priced Manhattan Airbnbs and how available they are throughout the year
highmanh_bnb <- filter(high_bnb, neighbourhood_group == "Manhattan")

highmanh_plot <- ggplot(highmanh_bnb, aes(x = price, y = availability_365)) + geom_point()
highmanh_plot

#lastly, let's pose a question: do hosts who have more than one Airbnb listing in our dataset charge more or less than hosts with only one?
morethanone_bnb <- filter(avail_abnyc, calculated_host_listings_count > 1)
one_bnb <- filter(avail_abnyc, calculated_host_listings_count == 1)

mean(morethanone_bnb$price) 
mean(one_bnb$price)
