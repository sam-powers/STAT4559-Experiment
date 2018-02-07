install.packages("anytime")
library(anytime)
library(tidyverse)
library(tidytext)
library(lubridate)

# Cut off dates to divide the CVILLE data.
# These are used in the Query Language for 
as.numeric(as.POSIXct("2017-08-01 0:00:00 EST"))
as.numeric(as.POSIXct("2017-08-13 0:00:00 EST"))
as.numeric(as.POSIXct("2017-08-14 0:00:00 EST"))
as.numeric(as.POSIXct("2017-08-15 0:00:00 EST"))
as.numeric(as.POSIXct("2017-08-16 0:00:00 EST"))
as.numeric(as.POSIXct("2017-08-21 0:00:00 EST"))
as.numeric(as.POSIXct("2017-08-31 24:00:00 EST"))

id1.12 <- "1CDZ_hR3kkJ2170YNRiE1cOGtAzcQgGDe" 
id13 <- "1COrGtDGd6O1DT6JmuGR_oK1CuH_P1xjX"
id14 <- "1CQ9SbUYkvwFpcV6GK06tfwL7PHLtCBya"
id15 <- "1CUg_UMcMgZ6kt6q-m0ObnNJsXFqo5mSa"

# Download Data #
aug1_12 <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id1.12))
aug13 <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id13))
aug14 <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id14))
aug15 <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id15))
aug1_15 <-rbind(aug1_12, aug13, aug14, aug15)

########
# Define the Functions I want for quick analysis #

# sentiment #
sentimental <- function(dataframe){
  d <- data.frame(dataframe)
  d$body <- as.character(d$body)
  d <- d %>% 
    unnest_tokens(word, body, token = "words", format = "text") %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    count(sentiment, ID) %>%
    spread(sentiment, n, fill = 0) 
  dataframe <- left_join(dataframe, d, by = "ID") %>%
    replace_na(list(negative = 0, positive = 0)) %>%
    mutate(sentiment = positive - negative, negative = -1*(negative))
} 

########


# Select the portions I want #
aug1_15 <- aug1_15 %>% 
  dplyr::select(body, created_utc, subreddit) %>%
  mutate(ID = 1:length(aug1_15$created_utc))

#######

# Use formula to get the sentiments #
aug1_15 <- sentimental(aug1_15)
plotdata <- aug1_15 %>% gather("sent","n", 5:6 )


# Calculate number of tweets per hour #
rates.aug1_15 <- aug1_15 %>% 
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  count(hour) %>%
  rename(count = n)

# Average Sentiment per hour #
avg.aug1_15 <- aug1_15 %>%
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  group_by(hour) %>% 
  summarise(avgsent = mean(sentiment), avgpos = mean(positive), avgneg = mean(negative))

# Join them
rates.aug1_15 <- rates.aug1_15 %>% left_join(avg.aug1_15)

plotdata.hourly <- rates.aug1_15 %>% gather("type","n", 2:3 )
plot.hourly.pos.neg <- rates.aug1_15 %>% gather("type", "n", 4:5)

# Plot number of tweets per hour and sentiment #
 # count and sentiment
ggplot(plotdata.hourly, aes(x=hour, y = n, color= type)) + geom_point(size = .001)
  
  # Negative/Positive by time
ggplot(plotdata, aes(x = anytime(created_utc), y = n, color = sent )) + 
  geom_point(size = .001) 

  # Sentiment by time
ggplot(aug1_15, aes(x= anytime(created_utc), y = sentiment)) + geom_point(size = .001) 
  
  # Neg/Pos grouped hourly
ggplot(plotdata, aes(x = floor_date(anytime(created_utc)), y = n, color = sent )) + 
  geom_point(size = .001) 

  # Avg pos/negative by hour
ggplot(plot.hourly.pos.neg, aes(x=hour, y = n, color= type)) + geom_point(size = .001)
















####### Trash ########
'

##### getting sentiment into the data set
d <- data.frame(aug1_15)
d$body <- as.character(d$body)
d <- d %>% unnest_tokens(word, body, token = "words", format = "text")
d <- d %>% inner_join(get_sentiments("bing"), by = "word") 
d <- d %>% count(sentiment, ID)
d <- d %>% spread(sentiment, n, fill = 0)
joined <- left_join(aug1_15, d)
joined <- joined %>% replace_na(list(negative = 0, positive = 0)) %>% mutate(sentiment = positive - negative)


#########
# This was a truly inefficient idea, but it ended up working. The above solution is much better.
d <- data.frame(txt = aug1_15$body[1]) 
d$txt <- as.character(d$txt)
d <- d %>% unnest_tokens(word, txt, token = "words", format = "text")
d <- d %>% inner_join(get_sentiments("bing"))

d <- d  %>%  count(sentiment) 
if (length(d$sentiment) > 1) {
d <- d %>% 
spread(sentiment, n, fill = 0) %>%            
mutate(sentiment = positive - negative)
} 
else { 
if (d$sentiment[1] == "positive") {
d <- data.frame(negative = 0, positive = d$n[1], sentiment = d$n[1])
}
else {
d <- data.frame(negative = d$n[1], positive = 0, sentiment = -1* (d$n[1]))
}
}
d <- data.frame(sentiment = c("positive, negative"), n = c(0,0))
d <- d  %>%   spread(sentiment, n, fill = 0)
d <- d  %>%   mutate(sentiment = positive - negative)


# sapply
unnest_tokens(testframe, tokens, char)


sentiments <- NULL      
d <- NULL

get_sent <- function(tweet){
d <- data.frame(txt = tweet) 
d$txt <- as.character(d$txt)
d <- d %>% unnest_tokens(word, txt, token = "words", format = "text") %>%
inner_join(get_sentiments("bing"), by = "word") 
if (length(d$sentiment) > 0){
d <- d %>% count(sentiment) 
if (length(d$sentiment ) > 1) {
d <- d %>% 
spread(sentiment, n, fill = 0) %>%            
mutate(sentiment = positive - negative)
}
else { 
if (d$sentiment[1] == "positive") {
d <- data.frame(negative = 0, positive = d$n[1], sentiment = d$n[1])
}
else {
d <- data.frame(negative = d$n[1], positive = 0, sentiment = -1* (d$n[1]))
}
}
}
else {
d <- tibble(negative = 0, positive = 0, sentiment = 0)
}
}
n <- length(aug1_15$body)
sentiments <- NULL

for (i in 1:n) {
q <- get_sent(aug1_15$body[i])
sentiments <- rbind(sentiments, q)
}
'



