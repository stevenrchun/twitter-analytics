### ABOUT THIS SCRIPT
# This script uses your Twitter Data and will output charts informing you about Engagement Rates, Click Activities etc. by "Time of Day" and "Day of Week".
# You can get your Twitter Data if you have an offical Twitter Ads account: http://ads.twitter.com/user/*/tweets
# The above link only works if you already have a Twitter Ads account.
# More information can be found in the original blog post about this script (by Tomasz Tunguz): http://tomasztunguz.com/twitter-best-practices/

### CONTRIBUTIONS
# Original author: Tomasz Tunguz – https://github.com/ttunguz
# Improved labeling and configuration, extended documentation: Clemens Kofler and Manuel Weiss – https://github.com/clemens + https://github.com/manualwise

### INSTRUCTIONS
# 1. Download this file and put it in the same folder as your tweet_activity_metrics.csv (e.g. /Users/username/Documents/twitter-data/)
# 2. If you need to install R you can download it here: http://cran.r-project.org/bin/
# 3. Go into your installation folder and start R
# 4. You need the following packages to run this script: ggplot2, reshape, plyr, scales. Install them by executing install.packages(c("ggplot2", "reshape", "plyr", "scales")) in the R prompt.
# 5. Edit the CONFIGURATION VARIABLES according to your needs

# Bonus: In each top_n_for_hour or top_n_for_day, test to statistical significance by comparing the result of the top_n function and the result of the matrix t test

### THESE ARE YOUR INSTALLED R PACKAGES
library(ggplot2)
library(reshape)
library(plyr)
library(scales)


### CONFIGURATION VARIABLES
target_time_zone = "America/New_York"
data_directory = "/Users/stevenchun/twitter-analytics/" # Important: This must end with a /
start_hour = 0
end_hour = 23

### FUNCTIONS
full_path = function(filename){
  paste(data_directory, filename, sep="")
}

x_axis_label_hourly = paste("Time of day (", target_time_zone, ")", sep="")
x_axis_label_daily = "Day of week"

#IMPORT DATA
data = read.csv(full_path("truman_data.csv"))
data$time = as.POSIXct(data$time, tz="UTC")
data$time = format(data$time, tz=target_time_zone)
data$hour = as.POSIXlt(strftime(data$time, format="%H:%M"), format="%H:%M")$hour
data = subset(data, hour>=start_hour)
data = subset(data, hour<=end_hour)
## BASICS


##  GENERAL PLOTS FOR HOUR
ggplot(data) + geom_point(aes(data$hour, data$engagement.rate), size=5, alpha=0.7, colour="red", position="jitter") + xlab("") + ylab("Engagement Rate") + ggtitle("Engagement Rate by Time of Day")+ theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.x = element_blank(), plot.title = element_text(size= rel(2))) + theme(legend.text = element_text(size = 18), axis.text = element_text(size=24))+ expand_limits(y=0) +  theme(panel.background = element_rect(fill = 'white'), axis.title.y=element_text(size=24)) + scale_colour_manual(values = c("red", "dodgerblue4")) + theme(legend.position="bottom")+  theme(strip.text.x = element_text(size = 14), strip.background = element_rect(fill='white'))+ theme(text=element_text(family="Helvetica", face="bold"))+ annotate("text", x = Inf, y = -Inf, label = "Truman Project",hjust=1.1, vjust=-1.1, col="gray", cex=6,fontface = "bold", alpha = 0.8) + scale_y_continuous( labels = percent_format())+ scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25), limits=c(1,24))
ggsave(full_path("engagementrate_by_timeofday.png"), dpi=300, width=9, height=6)
                  
##  TIME OF DAY FUNCTIONS
  
top_n_for_hour = function(df, n){
  ordered = df[order(df$average, decreasing=TRUE),][1:n,]
  return (ordered)
}  

time_of_day_t = function(df, field, top_hours){
  x = data.frame(matrix(NA, nrow = 25, ncol = 25))
  range = seq(start_hour+1:end_hour+1)

  colnames(x) = seq(1:25)
  rownames(x) = seq(1:25)

  for (i in range){
    current_hour = subset(df, df$hour == i)
    for (j in range){
      testing_hour = subset(df, df$hour == j)
      if (is.data.frame(testing_hour) && nrow(testing_hour) > 1 && nrow(current_hour)>1){
        x[i, j] = t.test(current_hour[field], testing_hour[field])$p.value  
      }
      else {
        x[i, j] = NA
      }
    }
  }
  x$hour1 = factor(rownames(x))
  x = melt(x)
  x = subset(x, value < 0.05)
  x = subset(x, hour1 %in% top_hours)
  x = subset(x, variable %in% top_hours)
  return (x)
}

designate_top = function(hourly_data, top_hours){
  hourly_data$top = 0
  hourly_data$top[hourly_data$hour %in% top_hours]=1
  return (hourly_data)
}
  
## RETWEETS BY HOUR
rt_by_hour = ddply(data, .(hour), summarise, average=mean(retweets), count_posts = length(Tweet.id))
top_retweet_hours = top_n_for_hour(rt_by_hour, 3)$hour
rt_by_hour = designate_top(rt_by_hour, top_retweet_hours)
retweet_t  = time_of_day_t(data, "retweets", top_retweet_hours)
## 8 and 9 are the best hours
  
ggplot(rt_by_hour) + geom_bar(aes(hour, average, fill=top), stat="identity")+ xlab(x_axis_label_hourly) + ylab("Retweets per Post") + ggtitle("Best Time of Day to Maximize RT") + theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.x = element_blank(), plot.title = element_text(size= rel(2))) + theme(legend.text = element_text(size = 18), axis.text = element_text(size=24))+ expand_limits(y=0) +  theme(panel.background = element_rect(fill = 'white'), axis.title.y=element_text(size=24)) +  scale_fill_gradient2(low="dodgerblue", high="red", mid="dodgerblue") + theme(legend.position="bottom") +  theme(strip.text.x = element_text(size = 14), strip.background = element_rect(fill='white')) + theme(text=element_text(family="Helvetica", face="bold"))+ annotate("text", x = Inf, y = -Inf, label="Truman Project", hjust=1.1, vjust=-1.1, col="gray", cex=6, fontface = "bold", alpha = 0.8) + theme(legend.position="none")+ scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), limits=c(1,24))
ggsave(full_path("maximum_retweets_by_timeofday.png"), dpi=300, width=12, height=9)

## IMPRESSIONS BY HOUR
imp_by_hour = ddply(data, .(hour), summarise, average = mean(impressions), count = length(Tweet.id))
top_imp_hours = top_n_for_hour(imp_by_hour, 5)$hour
imp_by_hour = designate_top(imp_by_hour, top_imp_hours)
imp_t = time_of_day_t(data, "impressions", top_imp_hours)

ggplot(imp_by_hour) + geom_bar(aes(hour, average, fill=top), stat="identity")+ xlab(x_axis_label_hourly) + ylab("Impressions") + ggtitle("Best Time of Day to Maximize Impressions")+ theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.x = element_blank(), plot.title = element_text(size= rel(2))) + theme(legend.text = element_text(size = 18), axis.text = element_text(size=24))+ expand_limits(y=0) +  theme(panel.background = element_rect(fill = 'white'), axis.title.y=element_text(size=24)) + scale_colour_manual(values = c("red", "dodgerblue4")) + theme(legend.position="bottom")+  theme(strip.text.x = element_text(size = 14), strip.background = element_rect(fill='white'))+ theme(text=element_text(family="Helvetica", face="bold"))+  scale_fill_gradient2(low="dodgerblue", high="red", mid="dodgerblue") +annotate("text", x = Inf, y = -Inf, label="Truman Project", hjust=1.1, vjust=-1.1, col="gray", cex=6,fontface = "bold", alpha = 0.8)+ theme(legend.position="none")+ scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), limits=c(1,24))
ggsave(full_path("maximum_impressions_by_timeofday.png"), dpi=300, width=12, height=9)
  
## ENGAGEMENT RATE BY HOUR
er_by_hour = ddply(data, .(hour), summarise, average = mean(engagement.rate))
top_er_hours = top_n_for_hour(er_by_hour, 11)$hour
er_by_hour = designate_top(er_by_hour, top_er_hours)
er_t = time_of_day_t(data, "engagement.rate", top_er_hours)
  
ggplot(er_by_hour) + geom_bar(aes(hour, average, fill=top), stat="identity")+ xlab(x_axis_label_hourly) + ylab("Engagement Rate") + ggtitle("Best Time of Day to Maximize Engagement Rate")+ theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.x = element_blank(), plot.title = element_text(size= rel(2))) + theme(legend.text = element_text(size = 18), axis.text = element_text(size=24))+ expand_limits(y=0) +  theme(panel.background = element_rect(fill = 'white'), axis.title.y=element_text(size=24)) + theme(legend.position="bottom")+  theme(strip.text.x = element_text(size = 14), strip.background = element_rect(fill='white'))+ theme(text=element_text(family="Helvetica", face="bold"))+ annotate("text", x = Inf, y = -Inf, label="Truman Project", hjust=1.1, vjust=-1.1, col="gray", cex=6,fontface = "bold", alpha = 0.8)+ scale_fill_gradient2(low="dodgerblue", high="red", mid="dodgerblue") + theme(legend.position="none")+ scale_y_continuous( labels = percent_format())+ scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), limits=c(1,24))
ggsave(full_path("engagementrate_by_timeofday.png"), dpi=300, width=12, height=9)
  
## URL CLICKS BY HOUR
data$url.ctr = data$url.clicks/data$impressions
url_by_hour = ddply(data, .(hour), summarise, average =  sum(url.clicks)/sum(impressions),url.ctr = sum(url.clicks)/sum(impressions))
top_url_hours = top_n_for_hour(url_by_hour, 4)$hour
url_by_hour = designate_top(url_by_hour, top_url_hours)
url_t = time_of_day_t(data, "url.ctr", top_url_hours)  
  
ggplot(url_by_hour) + geom_bar(aes(hour, average, fill=top), stat="identity")+ xlab(x_axis_label_hourly) + ylab("Click Rate") + ggtitle("Best Time of Day to Maximize Click Rate")+ theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.x = element_blank(), plot.title = element_text(size= rel(2))) + theme(legend.text = element_text(size = 18), axis.text = element_text(size=24))+ expand_limits(y=0) +  theme(panel.background = element_rect(fill = 'white'), axis.title.y=element_text(size=24)) + theme(legend.position="bottom")+  theme(strip.text.x = element_text(size = 14), strip.background = element_rect(fill='white'))+ theme(text=element_text(family="Helvetica", face="bold"))+ annotate("text", x = Inf, y = -Inf, label="Truman Project", hjust=1.1, vjust=-1.1, col="gray", cex=6,fontface = "bold", alpha = 0.8)+ scale_fill_gradient2(low="dodgerblue", high="red", mid="dodgerblue") + theme(legend.position="none")+ scale_y_continuous( labels = percent_format())+ scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), limits=c(1,24))
ggsave(full_path("url_clicks_by_timeofday.png"), dpi=300, width=12, height=9)

## FOLLOWS BY HOUR

follows_by_hour = ddply(data, .(hour), summarise, average =  mean(follows))
top_follows_hours = top_n_for_hour(follows_by_hour, 4)$hour
follows_by_hour = designate_top(follows_by_hour, top_follows_hours)
follows_t = time_of_day_t(data, "follows", top_follows_hours)  
  
ggplot(follows_by_hour) + geom_bar(aes(hour, average, fill=top), stat="identity")+ xlab(x_axis_label_hourly) + ylab("Follows") + ggtitle("Best Time of Day for New Follows")+ theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.x = element_blank(), plot.title = element_text(size= rel(2))) + theme(legend.text = element_text(size = 18), axis.text = element_text(size=24))+ expand_limits(y=0) +  theme(panel.background = element_rect(fill = 'white'), axis.title.y=element_text(size=24)) + theme(legend.position="bottom")+  theme(strip.text.x = element_text(size = 14), strip.background = element_rect(fill='white'))+ theme(text=element_text(family="Helvetica", face="bold"))+ annotate("text", x = Inf, y = -Inf, label = "Truman Project",hjust=1.1, vjust=-1.1, col="gray", cex=6,fontface = "bold", alpha = 0.8)+ scale_fill_gradient2(low="dodgerblue", high="red", mid="dodgerblue") + theme(legend.position="none")+ scale_y_continuous( labels = percent_format())+ scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), limits=c(1,24))
ggsave(full_path("follow_rate_by_timeofday.png"), dpi=300, width=12, height=9)

### DAY OF WEEK ANALYSIS
## DAY OF WEEK FUNCTIONS
data$day = weekdays(as.Date(data$time))
data$weekday = as.POSIXlt(data$time)$wday

weekday_t = function(df, field, top_days){
  x = data.frame(matrix(NA, nrow = 7, ncol = 7))
  colnames(x) = seq(0:6)
  rownames(x) = seq(0:6)
  for (i in 1:7){
    current_day = subset(df, df$weekday == i-1)
    for (j in 1:7){
      testing_day = subset(df, df$day == j-1)
      if (is.data.frame(testing_day) && nrow(testing_day) > 1 && nrow(current_day)>1){
        x[i, j] = t.test(current_day[field], testing_day[field])$p.value  
      }
      else {
        x[i, j] = NA
      }
    }
  }
  x$day = factor(rownames(x))
  x = melt(x)
  x = subset(x, value < 0.05)
  x = subset(x, day %in% top_days)
  x = subset(x, variable %in% top_days)
  return (x)
}

top_n_for_day = function(df, n){
  ordered = df[order(df$average, decreasing=TRUE),][1:n,]
  return (ordered)
}  

 ## PLOT HIGH LEVEL DATA
                  
ggplot(data) + geom_point(aes(data$weekday, data$engagement.rate), size=5, alpha=0.7, colour="dodgerblue", position="jitter") + xlab("") + ylab("Engagement Rate") + ggtitle("Engagement Rate by Day of Week")+ theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.x = element_blank(), plot.title = element_text(size= rel(2))) + theme(legend.text = element_text(size = 18), axis.text = element_text(size=24))+ expand_limits(y=0) +  theme(panel.background = element_rect(fill = 'white'), axis.title.y=element_text(size=24)) + scale_colour_manual(values = c("red", "dodgerblue4")) + theme(legend.position="bottom")+  theme(strip.text.x = element_text(size = 14), strip.background = element_rect(fill='white'))+ theme(text=element_text(family="Helvetica", face="bold"))+ annotate("text", x = Inf, y = -Inf, label = "Truman Project",hjust=1.1, vjust=-1.1, col="gray", cex=6,fontface = "bold", alpha = 0.8) + scale_y_continuous( labels = percent_format())+ scale_x_continuous(breaks=c(1,2,3,4,5,6,7), limits=c(1,7))        
ggplot(data) + geom_boxplot(aes(data$weekday, data$impressions,outlier.color="gray", group=data$weekday), fill="orange", colour="gray",  outlier.colour="gray50", outlier.size=3) + xlab("") + ylab("Impressions") + ggtitle("Impressions by Day of Week")+ theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.x = element_blank(), plot.title = element_text(size= rel(2))) + theme(legend.text = element_text(size = 18), axis.text = element_text(size=24))+ expand_limits(y=0) +  theme(panel.background = element_rect(fill = 'white'), axis.title.y=element_text(size=24)) + scale_colour_manual(values = c("red", "dodgerblue4")) + theme(legend.position="bottom")+  theme(strip.text.x = element_text(size = 14), strip.background = element_rect(fill='white'))+ theme(text=element_text(family="Helvetica", face="bold"))+ annotate("text", x = Inf, y = -Inf, label = "Truman Project",hjust=1.1, vjust=-1.1, col="gray", cex=6,fontface = "bold", alpha = 0.8)+ scale_y_continuous( labels = comma_format())


## URL CLICKS BY DAY
data$url.ctr = data$url.clicks/data$impressions
url_by_day = ddply(data, .(weekday), summarise, average =  sum(url.clicks)/sum(impressions),url.ctr = sum(url.clicks)/sum(impressions), count = length(url.clicks))
top_url_day = top_n_for_day(url_by_day, 7)$weekday
url_by_day = designate_top(url_by_day, top_url_day)
day_url_t = weekday_t(data, "url.ctr", top_url_day)  

ggplot(url_by_day) + geom_bar(aes(weekday, average, fill=top), stat="identity")+ xlab(x_axis_label_daily) + ylab("Click Rate") + ggtitle("Best Day of Week to Maximize Click Rate")+ theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.x = element_blank(), plot.title = element_text(size= rel(2))) + theme(legend.text = element_text(size = 18), axis.text = element_text(size=24))+ expand_limits(y=0) +  theme(panel.background = element_rect(fill = 'white'), axis.title.y=element_text(size=24)) + theme(legend.position="bottom")+  theme(strip.text.x = element_text(size = 14), strip.background = element_rect(fill='white'))+ theme(text=element_text(family="Helvetica", face="bold"))+ annotate("text", x = Inf, y = -Inf, label = "Truman Project",hjust=1.1, vjust=-1.1, col="gray", cex=6,fontface = "bold", alpha = 0.8)+ scale_fill_gradient2(low="dodgerblue", high="red", mid="dodgerblue") + theme(legend.position="none")+ scale_y_continuous( labels = percent_format())+ scale_x_continuous(breaks=c(1,2,3,4,5,6,7), limits=c(1,7))
ggsave(full_path("url_clicks_by_day.png"), dpi=300, width=12, height=9)

## ENGAGEMENT RATE BY DAY
er_by_day = ddply(data, .(weekday), summarise, average =  mean(engagement.rate))
top_er_day = top_n_for_day(er_by_day, 7)$weekday
er_by_day = designate_top(er_by_day, top_er_day)
day_er_t = weekday_t(data, "engagement.rate", top_er_day)  

ggplot(er_by_day) + geom_bar(aes(weekday, average, fill=top), stat="identity")+ xlab(x_axis_label_daily) + ylab("Engagement Rate") + ggtitle("Best Day of Week to Maximize Engagement Rate")+ theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.x = element_blank(), plot.title = element_text(size= rel(2))) + theme(legend.text = element_text(size = 18), axis.text = element_text(size=24))+ expand_limits(y=0) +  theme(panel.background = element_rect(fill = 'white'), axis.title.y=element_text(size=24)) + theme(legend.position="bottom")+  theme(strip.text.x = element_text(size = 14), strip.background = element_rect(fill='white'))+ theme(text=element_text(family="Helvetica", face="bold"))+ annotate("text", x = Inf, y = -Inf, label = "Truman Project",hjust=1.1, vjust=-1.1, col="gray", cex=6,fontface = "bold", alpha = 0.8)+ scale_fill_gradient2(low="dodgerblue", high="red", mid="dodgerblue") + theme(legend.position="none") + scale_y_continuous( labels = percent_format()) + scale_x_continuous(breaks=c(1,2,3,4,5,6,7), limits=c(1,7))
ggsave(full_path("engagementrate_by_day.png"), dpi=300, width=12, height=9)

## IMPRESSIONS BY DAY
imp_by_day = ddply(data, .(weekday), summarise, average =  mean(impressions))
top_imp_day = top_n_for_day(imp_by_day, 7)$weekday
imp_by_day = designate_top(imp_by_day, top_imp_day)
day_imp_t = weekday_t(data, "impressions", top_imp_day)  

ggplot(imp_by_day) + geom_bar(aes(weekday, average, fill=top), stat="identity")+ xlab(x_axis_label_daily) + ylab("Impressions") + ggtitle("Best Day of Week to Maximize Impressions")+ theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.x = element_blank(), plot.title = element_text(size= rel(2))) + theme(legend.text = element_text(size = 18), axis.text = element_text(size=24))+ expand_limits(y=0) +  theme(panel.background = element_rect(fill = 'white'), axis.title.y=element_text(size=24)) + theme(legend.position="bottom")+  theme(strip.text.x = element_text(size = 14), strip.background = element_rect(fill='white'))+ theme(text=element_text(family="Helvetica", face="bold"))+ annotate("text", x = Inf, y = -Inf, label = "Truman Project",hjust=1.1, vjust=-1.1, col="gray", cex=6,fontface = "bold", alpha = 0.8)+ scale_fill_gradient2(low="dodgerblue", high="red", mid="dodgerblue") + theme(legend.position="none") + scale_y_continuous( labels = comma_format()) + scale_x_continuous(breaks=c(1,2,3,4,5,6,7), limits=c(1,7))
ggsave(full_path("maximum_impressions_by_day.png"), dpi=300, width=12, height=9)

### CORRELATIONS
cor(data$impressions, data$engagement.rate)
cor(data$retweets, data$impressions)
cor(data$replies, data$impressions)
data$tweet.length = nchar(as.character(data$Tweet.text[1]))

##Word Count
Sys.setlocale('LC_ALL','C')
stripped <- gsub(",", "", data$Tweet.text)
stripped
stripped <- gsub("\\.", "", stripped)
stripped <- gsub(":", "", stripped)
stripped <- gsub("?", "", stripped)
stripped <- gsub("!", "", stripped)
data$words <- strsplit(as.character(stripped), " ")
vocabulary <- unlist(data$words, recursive = TRUE)
freq<-table(vocabulary)

#Engagement Rate
plot(data$engagement.rate, type = "l", main = "Engagement Rate Over Time", xlab = "Last 208 Tweets (1st is most recent)", ylab = "
Engagement Rate (imp/action)")
tweets <- rep(1:208)
tweets
reg<-lm(data$engagement.rate~tweets)
reg
abline(reg, col="blue")
#Engagement is slightly increasing!

#Impressions
plot(data$impressions, type= "l", main = "Impressions", xlab="Tweets (1 is most recent)", ylab="Impressions")
impReg <-lm(data$impressions ~tweets)
impReg
abline(impReg, col="red")

#Impressions are pretty much static.

