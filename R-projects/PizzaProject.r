#Project_1
#Nick Kim, Lev Zadvinskiy, Jack Song, Colin Cassady


library(stringr)
con = file("pizza_requests.txt", open="r")

df = data.frame(matrix())

user_count = 1

# The basic idea here is that we're going to iterate over the entire file, line by line
# storing the information as we go and creating columns as we run into new field names.
while (length(oneline <- readLines(con, n = 1, warn = FALSE)) > 0)
{
  # If the line is empty ignore it and move on
  if (oneline == "    ")
    next
  
  # If we run into a user break, simply skip over it, increment the user count, and
  # create a new user.
  if (oneline == "%%%%%%%%%%")
  {
    # Skip, and then read the next line.
    oneline <- readLines(con, n = 1, warn = FALSE)
    oneline <- readLines(con, n = 1, warn = FALSE)
    
    # Increment user_count
    user_count = user_count + 1
    df[user_count,] = NA
  }
  
  # Parse line with a regex. results[1] = entire match, results[2] = variable name, results[3] = value {}
  regexresults = unlist(str_match_all(oneline, '\\"([^\\"]*)\\": (null|true|false|\\"[^\\"]*\\"|[\\d\\.-]+|\\{\\}|\\{)'))
  fieldname = regexresults[2]
  value = regexresults[3]
  
  if (!(fieldname %in% names(df)))
  {
    # If it's not a column already in the data frame, add it.
    df[fieldname] = NA
  }
  
  if (fieldname == "requester_subreddits_at_request")
  {
    # User hasn't subscribed to any subreddits
    if (value == "{}")
    {
      value = ""
    }
    else
    {      
      # If it's the subreddit list, we want to contatinate all of their subreddits to a space-delimited string, 
      # and then store it in the data frame.
      
      sublist = c()
      
      while (length(oneline <- readLines(con, n = 1, warn = FALSE)) > 0)
      {
        # Read lines until we hit "}", since that's the character that closes the list
        subregexresult = unlist(str_match_all(oneline, '\\"([^\\"]*)\\"|(\\})'))
        subredditname = subregexresult[2]
        closingbrace = subregexresult[3]
        
        if (closingbrace == "}")
        {
          break
        }
        
        # Sticking the subreddit names into a vector
        sublist = c(sublist, subredditname)
      }
      
      # Collapse the vector into a space delimited string and store it as the value
      value <- paste(sublist, collapse = ' ')
    }
  }
  
  # Assign the current value to the current user under the current field name, and reiterate.
  df[fieldname][user_count,] = value
}

close(con)

attach(df)
names(df)

###################### ANSWERING THE APPENDIX QUESTIONS
truecount=0
for(i in 1:length(requester_received_pizza)){
  if(df[i,"requester_received_pizza"]=="true"){
    truecount=truecount+1
  }
}
truecount #1397 people received pizzas
falsecount=length(requester_received_pizza)-truecount ; falsecount #4274
percentage=truecount/(truecount+falsecount) ; percentage #0.246341


nopizza=df[requester_received_pizza=="false",]
numsubreddits=as.numeric(nopizza$requester_number_of_subreddits_at_request)
mean(numsubreddits) #17.37833

yespizza=df[requester_received_pizza=="true",]
yes.numdownvotes=as.numeric(yespizza$number_of_downvotes_of_request_at_retrieval)
mean(yes.numdownvotes) #2.631353


################################################INTRODUCTORY ANALYSIS OF SUCCESSFUL REQUESTS
yes_num_posts=as.numeric(yespizza$requester_number_of_posts_at_request)
no_num_posts=as.numeric(nopizza$requester_number_of_posts_at_request)
mean(no_num_posts);mean(yes_num_posts)
# 20.21572
# 24.8325

no_age=as.numeric(yespizza$requester_account_age_in_days_at_request)
yes_age=as.numeric(nopizza$requester_account_age_in_days_at_request)
mean(no_age);mean(yes_age)
#279.9025
#244.0809

yes_dayspost=as.numeric(yespizza$requester_days_since_first_post_on_raop_at_request)
no_dayspost=as.numeric(nopizza$requester_days_since_first_post_on_raop_at_request)
mean(no_dayspost);mean(yes_dayspost)
#13.46623
#26.06974


newaccount_nopiz=nopizza[nopizza$requester_account_age_in_days_at_request=="0.0",]
newaccount_yespiz=yespizza[yespizza$requester_account_age_in_days_at_request=="0.0",]
dim(newaccount_nopiz) #1071 people on the nopizza dataframe had made new accounts when they made a new request
dim(newaccount_yespiz) #195 people on the yespizza dataframe had made new accounts when they made a new request


####### Analysis of Requests by Time of Day
library(readr)
library(dplyr)
library(scales)
unix_timestamp_of_request_utc
numtimes=as.numeric(df$unix_timestamp_of_request_utc)
times=as.POSIXct(numtimes, origin="1970-01-01")
head(times)

class(df$times)
df$times <- as.POSIXct(paste("2012-01-01", substr(times, 12, 16)))
class(df$times)
par(mfrow=c(3,1))
hist(df$times,breaks="hours",freq=T,xlab="Time of Day",main="Histogram of Requests by Time of Day")
hist(nopizza$times,breaks="hours",freq=T,xlab="Time of Day",main="Histogram of Successful Requests by Time of Day")
hist(yespizza$times,breaks="hours",freq=T,xlab="Time of Day",main="Histogram of Unsuccesful Requests by Time of Day")

library(ggplot2)
qplot(df$times,fill=df$requester_received_pizza)+
  scale_x_datetime(labels=date_format("%H:%M",tz="EST"))+
  ggtitle("Requests by Time of Day and Receiving of Pizza")+theme(plot.title=element_text(size=28),legend.text=element_text(size=16))+
  labs(x="Time of Day",y="Frequency")+theme(legend.justification=c(0,1),legend.position=c(0,1))+
  scale_fill_discrete(name="Requester Received Pizza?")

###### Total number of interactions Analysis
mean(as.numeric(df$request_number_of_comments_at_retrieval)) #Overall avg number of comments 2.869688
no_mean=mean(as.numeric(nopizza$request_number_of_comments_at_retrieval)) #Average for no pizza 2.056387
yes_mean=mean(as.numeric(yespizza$request_number_of_comments_at_retrieval)) #Average for yes pizza 5.35791
no_sd=sd(as.numeric(nopizza$request_number_of_comments_at_retrieval));no_sd
yes_sd=sd(as.numeric(yespizza$request_number_of_comments_at_retrieval));yes_sd
no_mean-(2*(no_sd/sqrt(n1))) #(1.92393,2.188845)
no_mean+(2*(no_sd/sqrt(n1)))
yes_mean-(2*(yes_sd/sqrt(n2))) #(5.005318,5.710502)
yes_mean+(2*(yes_sd/sqrt(n2)))



mean(as.numeric(df$requester_upvotes_plus_downvotes_at_request)) #3715.651
mean(as.numeric(nopizza$requester_upvotes_plus_downvotes_at_request)) #3256.756
mean(as.numeric(yespizza$requester_upvotes_plus_downvotes_at_request)) #5119.603

mean(as.numeric(df$requester_upvotes_plus_downvotes_at_retrieval)) #7629.764
nomean=mean(as.numeric(nopizza$requester_upvotes_plus_downvotes_at_retrieval)) #6553.142
yesmean=mean(as.numeric(yespizza$requester_upvotes_plus_downvotes_at_retrieval)) #10923.59

n1=length(nopizza$requester_upvotes_plus_downvotes_at_retrieval) #4274
n2=length(yespizza$requester_upvotes_plus_downvotes_at_retrieval) #1397
sdno=sd(as.numeric(nopizza$requester_upvotes_plus_downvotes_at_retrieval));sdno
sdyes=sd(as.numeric(yespizza$requester_upvotes_plus_downvotes_at_retrieval));sdyes

nomean-(2*(sdno/sqrt(n1))) #(5921.926,7184.358)
nomean+(2*(sdno/sqrt(n1))) 

yesmean-(2*(sdyes/sqrt(n2))) #(7778.855,14068.33)
yesmean+(2*(sdyes/sqrt(n2))) 


plot(as.numeric(nopizza$requester_number_of_comments_at_retrieval),
     as.numeric(nopizza$requester_upvotes_plus_downvotes_at_retrieval),xlim=c(0,950),ylim=c(0,50000),
     xlab="Number of Comments at Retrieval",ylab="Requester Upvotes + Downvotes at Retrieval", main="
     Comments vs. Total votes")

points(as.numeric(yespizza$requester_number_of_comments_at_retrieval),
       as.numeric(yespizza$requester_upvotes_plus_downvotes_at_retrieval),xlim=c(0,950),ylim=c(0,50000),col="red")
legend("topleft",legend=c("True","False"))


###################################################
###################################################
###################################################

#Analysis of upvotes minus downvotes data (at request and at retrieval) and it's contribution to a successul pizza request

library(aod)

#Converting required columns into numeric data
df$requester_upvotes_minus_downvotes_at_request <- as.numeric(df$requester_upvotes_minus_downvotes_at_request)
df$requester_upvotes_minus_downvotes_at_retrieval <- as.numeric(df$requester_upvotes_minus_downvotes_at_retrieval)

#Reviewing the columns to gather basic statistics
summary(df$requester_upvotes_minus_downvotes_at_request)
summary(df$requester_upvotes_minus_downvotes_at_retrieval)

#Testing with logistic regression due to dichotomous outcome variables.
#Test on "at request" variable:
df_req.glm <- glm(requester_received_pizza ~ requester_upvotes_minus_downvotes_at_request, data = df, family = "binomial")

summary(df_req.glm)

#Test on "at retrieval" variable:
df_ret.glm <- glm(requester_received_pizza ~ requester_upvotes_minus_downvotes_at_retrieval, data = df, family = "binomial")

summary(df_ret.glm)

#Test on both variables:
df_test.glm <- glm(requester_received_pizza ~ requester_upvotes_minus_downvotes_at_request + requester_upvotes_minus_downvotes_at_retrieval, data = df, family = "binomial")

summary(df_test.glm)

wald.test(b = coef(df_test.glm), Sigma = vcov(df_test.glm), Terms = 2:3)
#Logistic regression tests show the following:
#Both upvotes minus downvotes at request and retrieval are statistically significant, independently and together.
#For every 1 positive point of upvotes minus downvotes the success of pizza delivery is changed by a very small amount. 
#Wald test further confirms statistical significance of both variables to the model.

cor(df$requester_received_pizza, df$requester_upvotes_minus_downvotes_at_request)
cor(df$requester_received_pizza, df$requester_upvotes_minus_downvotes_at_retrieval)
#Correlation test shows that upvote minus downvote data and receipt of pizza data have poor relevance to one another. 

#Logistic regression plot
plot(df$requester_upvotes_minus_downvotes_at_request, df$requester_received_pizza, xlab="Requester Upvotes Minus Downvotes", ylab="Probability", main="Logistic Regression Plot")
curve(predict(df_req.glm, data.frame(requester_upvotes_minus_downvotes_at_request=x), type="resp"), add=TRUE, col="red")
curve(predict(df_ret.glm, data.frame(requester_upvotes_minus_downvotes_at_retrieval=x), type="resp"), add=TRUE, col="blue")
legend('bottomright', c('at Request', 'at Retrieval'), col=2:1, lwd=1:2, bty='n', cex=0.7)
#As seen from the plot, "at retrieval" data is slightly better at predicting the outcome of pizza request.
#However, logistic regression model is not a good estimate of the outcome if the number of upvotes minus downvotes is low.
#Given that majority of the data falls into "low" category, the overall conclusion is that the difference between requester upvotes and downvotes is NOT a good indicator of a successful pizza request.

#Testing on a smaller sample, between 0 and 1000. 
#Test on "at request" variable with limits:
df_req2.glm <- glm(requester_received_pizza[requester_upvotes_minus_downvotes_at_request >= 0 & requester_upvotes_minus_downvotes_at_request <= 1000] ~ requester_upvotes_minus_downvotes_at_request[requester_upvotes_minus_downvotes_at_request >= 0 & requester_upvotes_minus_downvotes_at_request <= 1000], data = df, family = "binomial")

summary(df_req2.glm)

cor(df$requester_received_pizza[df$requester_upvotes_minus_downvotes_at_request >= 0 & df$requester_upvotes_minus_downvotes_at_request <= 1000],df$requester_upvotes_minus_downvotes_at_request[df$requester_upvotes_minus_downvotes_at_request >= 0 & df$requester_upvotes_minus_downvotes_at_request <= 1000])

#Test on "at retrieval" variable with limits:
df_ret2.glm <- glm(requester_received_pizza[requester_upvotes_minus_downvotes_at_retrieval >= 0 & requester_upvotes_minus_downvotes_at_retrieval <= 1000] ~ requester_upvotes_minus_downvotes_at_retrieval[requester_upvotes_minus_downvotes_at_retrieval >= 0 & requester_upvotes_minus_downvotes_at_retrieval <= 1000], data = df, family = "binomial")

summary(df_ret2.glm)

cor(df$requester_received_pizza[df$requester_upvotes_minus_downvotes_at_retrieval >= 0 & df$requester_upvotes_minus_downvotes_at_retrieval <= 1000],df$requester_upvotes_minus_downvotes_at_retrieval[df$requester_upvotes_minus_downvotes_at_retrieval >= 0 & df$requester_upvotes_minus_downvotes_at_retrieval <= 1000])

#Logistic regression plot with limits
plot(df$requester_upvotes_minus_downvotes_at_request[df$requester_upvotes_minus_downvotes_at_request >= 0 & df$requester_upvotes_minus_downvotes_at_request <= 1000], df$requester_received_pizza[df$requester_upvotes_minus_downvotes_at_request >= 0 & df$requester_upvotes_minus_downvotes_at_request <= 1000], xlab="Requester Upvotes Minus Downvotes", ylab="Probability", main="Logistic Regression Plot")
curve(predict(df_req2.glm, data.frame(requester_upvotes_minus_downvotes_at_request=x), type="resp"), add=TRUE, col="red")
curve(predict(df_ret2.glm, data.frame(requester_upvotes_minus_downvotes_at_retrieval=x), type="resp"), add=TRUE, col="blue")
legend('right', c('at Request', 'at Retrieval'), col=2:1, lwd=1:2, bty='n', cex=0.7)
#This graph further proves that predictability of the upvote minus downvote data is very low, between 20 and 30 percent. 



#################################################################
# vars interested:
#   "request_text": Full text of the request.
#   "requester_received_pizza": Boolean indicating the success of the request, i.e., 
#    whether the requester received pizza. 

# Coding Logic:
# first get rid of the "\\n\\n"s, then get rid of the "\" at the beginning of each reddit text
# next, replace informal contractions with a placeholder for more accurate word counting
df2['request_text'] <- str_replace_all(df2$request_text, "\\W+n\\W+n", " ") %>% 
  str_replace_all("\"", " ") %>%
  str_replace_all("\\w+\\'\\w+", "placeholder")

# count the words
df2['wordCount'] <- sapply(gregexpr("[A-z]\\W+", df2$request_text), length) + 1L

# group the data based on whether the requester received pizza or not, then compute mean of the 
# length of reddit_text for both situations.
df3 <- group_by(df2, requester_received_pizza) %>%
  summarise(meanWordCount = mean(wordCount, na.rm=TRUE))
df3
