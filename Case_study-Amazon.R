library(dplyr)
library(tidyverse) 
library(ggplot2)
library(mosaic)
library(lazyeval)
library(statisticalModeling)
#install.packages("plotly")
library(plotly)
items <- read.csv("C:\\Users\\Vaishnavi\\Downloads\\items.csv")

reviews <- read.csv("C:\\Users\\Vaishnavi\\Desktop\\SDBI\\Semester-I\\R Programming\\Case studies\\Amazon\\reviews.csv")

glimpse(items)
items <- items %>% group_by(brand) %>% mutate(product_count = n())

#all the brands of the smart phone available on amazon
q1 <- unique(items$brand)
#visualization1
##ggplot(items, aes(x = brand)) + geom_bar() +
#  labs(x = "Smartphone Brands", y = "Number of products", 
#      title = "Popular Smartphone Brands on Amazon")
#OR
fig1 <- plot_ly(items, x = ~brand) %>% add_histogram() %>% 
  layout(xaxis = list(title = "Smartphone Brands"),
         yaxis = list(title = "Number of products"), 
         title = "Popular Smartphone Brands on Amazon")
fig1

#Which smart phone has highest reviews and belongs to which brand?
q2 <- items %>% select(brand, title, totalReviews) %>% 
  arrange(desc(totalReviews)) %>% top_n(1)

#which smart phone brand have highest rating?
q3 <- items %>% select(brand, title, rating) %>% 
  arrange(desc(rating)) %>% top_n(1)

#which smart phone brand have lowest rating?
q4 <- items %>% select(brand, title, rating) %>% 
  arrange(rating) %>% slice(1)

#visualization2
#ggplot(items, aes(x = rating, fill = brand)) + geom_bar()
#OR
fig2 <- plot_ly(items, y = ~rating, x = ~brand, type = "box") %>% 
  layout(xaxis = list(title = "Smartphone Brands"),
         yaxis = list(title = "Overall Rating"), 
         title = "Ratings of Smartphone Brands")
fig2
#how many smart phones does a brand has on Amazon?
q5 <- items %>%group_by(brand) %>%  
  summarise(count = n()) %>% arrange(desc(count))
 

#merged items and reviews dataset
df <- inner_join(items, reviews, by = "asin")

df <-df %>% rename(overall_rating = rating.x,
              rating_per_user = rating.y)

# heatmap showing the ratings per user and overall rating w.r.t brands available on Amazon
fig3 <- plot_ly(df,  x = ~brand, y = ~rating_per_user, 
        z = ~overall_rating, type = "heatmap") %>% 
  layout(xaxis = list(title = "Smartphone Brands"),
         yaxis = list(title = "Ratings per user"), 
         title = "Heatmap of Ratings per user")
fig3

# line graph showing brand value
q6 <- df %>% group_by(brand) %>% select(brand, rating_per_user)%>% 
  summarize(average_rating = mean(x = rating_per_user)) 

fig4 <- plot_ly(q6, x = ~brand, y = ~average_rating,
                type = "scatter", mode = "lines+markers") %>% 
  layout(xaxis = list(title = "Smartphone Brands"),
         yaxis = list(title = "Average rating"), 
         title = "Brand Value")
fig4

#Year wise rating of brands
q7 <- df %>% group_by(brand, Year) %>% select(rating_per_user) %>% 
  summarize(average_rating = mean(rating_per_user))
  
plot_ly(q7, x = ~Year, y = ~average_rating,name = ~brand,
        type = "scatter", mode = "lines")

#Verified and unverified reviews
df %>% group_by(brand, verified) %>% summarize(summry = count(totalReviews)) %>% 
  plot_ly(x= ~brand, y = ~summry, color = ~verified, type = 'bar')

#Summary of growth
df %>% group_by(Year) %>%  
  summarise(Growth = sum(totalReviews))%>% 
  plot_ly(x= ~Year, y =~ Growth, type= 'scatter', mode = 'markers+lines') %>% 
  layout(title = "Increase of Amazon Customers over the years", 
         yaxis = list(title = "Reviews"))

