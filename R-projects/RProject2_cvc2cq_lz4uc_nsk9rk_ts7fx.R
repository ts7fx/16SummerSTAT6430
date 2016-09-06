#Project 2
#Nick Kim, Lev Zadvinskiy, Colin Cassady, Jack Tianye Song


#Loading datasets
library(dplyr)
library(stringr)
df_rev <- read.table("reviews.txt", sep = '\t')
colnames(df_rev) <- c("reviewer_id", "movie_id", "rating", "timestamp")

df_ers <- read.table("reviewers.txt", sep = '|')
colnames(df_ers) <- c("reviewer_id", "age", "gender", "occupation", "Zipcode")

df_gen <- read.csv("genres.txt", sep = '|', header = FALSE)
df_gen <- subset(df_gen, select = -V4)
colnames(df_gen) <- c("movie_id", "movie_title", "release_date", "IMDb_URL", "Unknown", "Action", "Adventure", "Animation", "Children's", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")

df_zip <- read.csv("zipcodes.txt", colClasses = c(rep("factor", 5)))

#Initial class structure cleanup
df_rev$reviewer_id <- as.factor(df_rev$reviewer_id)
df_rev$movie_id <- as.factor(df_rev$movie_id)
df_ers$reviewer_id <- as.factor(df_ers$reviewer_id)
df_gen$movie_id <- as.factor(df_gen$movie_id)

#Filtering dataset df_zip by zipcode to include only the first occurance of zipcode
df_zip2 <- df_zip[!duplicated(df_zip$Zipcode),]

#Include only those columns in df_zip2 that will be used for analysis
df_zip2 <- subset(df_zip2, select = c("Zipcode", "State", "Country", "TaxReturnsFiled", "EstimatedPopulation", "TotalWages"))

#Merging datasets into one
df <- merge.data.frame(df_rev, df_ers, by = 'reviewer_id', all = TRUE, sort = FALSE)
df <- merge.data.frame(df, df_gen, by = 'movie_id', all = TRUE, sort = FALSE)
df <- merge.data.frame(df, df_zip2, by.x = 'Zipcode', all.x =  TRUE, sort = FALSE)




################################################################## Answering the Appendix Questions
# 1. Which reviewer reviewed the most movies?
df_1 <- group_by(df, reviewer_id) %>%
  summarise(revCount = length(movie_id)) %>%
  arrange(desc(revCount)) 
head(df_1,1)

# 2. Which state/territory/Canada produced the top-5 most reviews?
df_2 <- group_by(df, Zipcode) %>%
  summarise(revCount = length(movie_id)) %>%
  arrange(desc(revCount)) 
head(df_2,5)
df[df$Zipcode==55414,][1,] #STATE OF MN

# 3. What percentage of reviews involved movies classified in at least two genres?
count <- 0
for (i in c(1:nrow(df)) ){
  temp <- table(df[i,12:30] == TRUE)
  if(temp[names(temp) == "TRUE"] >= 2)
    count <- count + 1
}
print(count/nrow(df))
# 0.69938 

# 4. What percentage of movies have 1, 2, 3, ... reviews? (Need a percentage for each.)
df_4 <- group_by(df, movie_id) %>%
  summarise(revCount = length(reviewer_id)) %>%
  group_by(revCount) %>%
  summarise(newCount = sum(revCount==revCount))
df_4
df_4$Percentages=(df_4$newCount/sum(df_4$newCount))*100

############################################################################# What makes a good review?

# Analysis

library(dplyr)
library(ggplot2)
library(likert)

# Part 1: What seems to be associated with a high rating?

# Comparing age of reviewer with age of the movie since release (in terms of 1998 as base year)
# The idea is to test and see if a difference in age between reviewer and the movie since release date has any effect on the rating 

# Creating a base year variable
thr <- as.Date("1998-10-23")

# Creating a test dataset where ageDelta is the difference between reviewer's age and 'age' of movie since release up to 1998
df_test <- mutate(df, movieAge = thr - release_date) %>%
  mutate(movieAge = round(movieAge/365.25, digits = 0), na.rm = TRUE) %>%
  mutate(ageDelta = age - movieAge) %>%
  group_by(rating) 

# Summarising results
summarise(df_test, mean = as.numeric(mean(ageDelta, na.rm = TRUE)))
# Higher rating was given if the age of the movie is closer to the age of the reviewer

# Visualizing the results with violin plot
ggplot(data = df_test, aes(x = rating, y = as.numeric(ageDelta), group = rating)) + 
  geom_violin(scale = 'count', na.rm = TRUE) + 
  labs(x = "Movie Rating", y = "Age Delta", title = "Ratings VS Reviewer/Movie Age Difference") + 
  theme_dark()
# Hard to tell if any significant relationship exists as most data is concentrated about 20 year mark

# Noticed the increase in count of rating variable when Age Delta is negative, plotting the data over negative interval to confirm a pattern
ggplot(data = df_test, aes(x = rating, y = as.numeric(ageDelta), group = rating)) + 
  geom_violin(scale = 'count', na.rm = TRUE) + ylim(-60, 0) +
  labs(x = "Movie Rating", y = "Age Delta", title = "Ratings VS Reviewer/Movie Age Difference") + 
  theme_dark()
# Clearly, an inverse relation exists between the age of the movie and the age of the reviewer
# A younger reviewer is more likely to give a high score on a movie much older than he/she is



# This snippet of code demonstrate how to 
# 1. divide movies into sub-categories which are based on movie release dates
# and 2. compute the average rating of movies in each decades.

# In order to achieve the abovementioned goal, here is how i break everything 
# into steps:

# 1. create new categorical variable "decades" which
#   20s <- 1920 < release date <= 1930
#   30s <- 1930 < release date <= 1940
#   and so on

# 2. group data frame based on decades, 
# 3. compute statistics

df$decades <- cut(df$release_date, 
                  breaks=c(as.Date("1920-01-01"),as.Date("1930-01-01"),
                           as.Date("1940-01-01"),as.Date("1950-01-01"),
                           as.Date("1960-01-01"),as.Date("1970-01-01"),
                           as.Date("1980-01-01"),as.Date("1990-01-01"),
                           as.Date("2000-01-01")), 
                  labels=c("20s", "30s","40s","50s","60s","70s","80s",
                           "90s"))

df_snip <- group_by(df, decades) %>%
  na.omit() %>%
  summarise(totalRev = length(movie_id), avgRating = mean(rating))

df_snip

# Plot of the df_snip
plot(df_snip$decades, df_snip$avgRating, xlab = "Decade", ylab = "Average Rating", main = "Average Rating per Decade")
# 40's appear to be a great decade in terms of movie quality

# Plot of the density of the movie reviews by decade and rating
ggplot(data = df, aes(x = df$decades, y = df$rating)) + geom_jitter(na.rm = TRUE, color = 'indianred') +
  labs(x = "Decade", y = "Movie Rating", title = "Density of Movie Reviews by Decade and Rating")



# Restructuring data to a likert-friendly format, finding summary and plotting the likert graph
# Creating a separate dataframe with only ratings and genres
df_ctg <- subset(df, select = rating:Western)
df_ctg <- subset(df_ctg, select = -timestamp:-IMDb_URL)

# Assigning a rating value for a genre where varaible was equal to 1
df_ctg$Unknown[df_ctg$Unknown == 1] <- df_ctg$rating[df_ctg$Unknown == 1]
df_ctg$Action[df_ctg$Action == 1] <- df_ctg$rating[df_ctg$Action == 1]
df_ctg$Adventure[df_ctg$Adventure == 1] <- df_ctg$rating[df_ctg$Adventure == 1]
df_ctg$Animation[df_ctg$Animation == 1] <- df_ctg$rating[df_ctg$Animation == 1]
df_ctg$Childrens[df_ctg$Childrens == 1] <- df_ctg$rating[df_ctg$Childrens == 1]
df_ctg$Comedy[df_ctg$Comedy == 1] <- df_ctg$rating[df_ctg$Comedy == 1]
df_ctg$Crime[df_ctg$Crime == 1] <- df_ctg$rating[df_ctg$Crime == 1]
df_ctg$Documentary[df_ctg$Documentary == 1] <- df_ctg$rating[df_ctg$Documentary == 1]
df_ctg$Drama[df_ctg$Drama == 1] <- df_ctg$rating[df_ctg$Drama == 1]
df_ctg$Fantasy[df_ctg$Fantasy == 1] <- df_ctg$rating[df_ctg$Fantasy == 1]
df_ctg$FilmNoir[df_ctg$FilmNoir == 1] <- df_ctg$rating[df_ctg$FilmNoir == 1]
df_ctg$Horror[df_ctg$Horror == 1] <- df_ctg$rating[df_ctg$Horror == 1]
df_ctg$Musical[df_ctg$Musical == 1] <- df_ctg$rating[df_ctg$Musical == 1]
df_ctg$Mystery[df_ctg$Mystery == 1] <- df_ctg$rating[df_ctg$Mystery == 1]
df_ctg$Romance[df_ctg$Romance == 1] <- df_ctg$rating[df_ctg$Romance == 1]
df_ctg$SciFi[df_ctg$SciFi == 1] <- df_ctg$rating[df_ctg$SciFi == 1]
df_ctg$Thriller[df_ctg$Thriller == 1] <- df_ctg$rating[df_ctg$Thriller == 1]
df_ctg$War[df_ctg$War == 1] <- df_ctg$rating[df_ctg$War == 1]
df_ctg$Western[df_ctg$Western == 1] <- df_ctg$rating[df_ctg$Western == 1]

# Removing rating column from equation
df_ctg <- subset(df_ctg, select = -rating)

# Assigning factor classicication with 5 levels to the dataframe
df_ctg <- as.data.frame(lapply(df_ctg, factor, levels = c("1", "2", "3", "4", "5")))

# Applying likert analysis 
df_ctg <- likert(df_ctg)

# Summary of likert
summary(df_ctg)

# Plotting the data to visualize categorical relationships of genres based on rating
plot(df_ctg, col = c('#ff1a1a', '#ff6666', '#e6e6e6', '#66b3ff', '#1a8cff'))
# Film Noir, War and Documentary were the 3 top rated categories of movies

##############################################################
############################################################## What Groups are more likely to provide higher ratings?

#Group by occupation to see which ones will give you the highest ratings
occ=group_by(df,occupation)
x=summarise(occ,avgrate=mean(rating))
x=x[order(-x$avgrate),]
x #Shows that "none, lawyer, and doctor round out the top 3"
reviewer=group_by(df,reviewer_id)

library(ggplot2)

ggplot(data=x, aes(x = reorder(occupation, avgrate), y = avgrate), sort = sort(avgrate)) + labs(x = "Occupation", y = "Average Rating") + geom_bar(stat="identity") + coord_flip()

gend=group_by(df,gender)
men=summarise(gend,avggend=mean(rating),counts=length(gender))
men

#gender  avggend
#<fctr>    <dbl>
#1      F 3.531507
#2      M 3.529289


reviewer_avg=summarise(reviewer,avg=mean(rating),counts=length(reviewer_id), occupation = head(occupation,1), age = head(age,1), gender = head(gender,1))
reviewer_avg
reviewer_avg_sorted=reviewer_avg[order(-reviewer_avg$avg),]
high10=reviewer_avg_sorted[1:10,] #see what the people giving the highest average ratings have in common
high10
low10=reviewer_avg_sorted[(length(reviewer_avg_sorted$reviewer_id)-9):length(reviewer_avg_sorted$reviewer_id),] # see what the people giving lowest 
low10
mean(low10$counts)
median(low10$counts)
mean(reviewer_avg_sorted$avg) 
mean(high10$age);mean(low10$age)


  
#Subset into two groups - those who give average rating >= 4 or less than 4
four_or_greater=reviewer_avg_sorted[reviewer_avg_sorted$avg >= 4.0,] ; dim(four_or_greater)
four_or_less=reviewer_avg_sorted[reviewer_avg_sorted$avg < 4.0,] ; dim(four_or_less)
#How many reviews on average does someone who gives an avg rating of >= 4 have under their belt?
mean(four_or_greater$counts) #77.2125
mean(four_or_less$counts) #111.9361
