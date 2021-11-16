library('ggplot2')
library(scales)
input <- read.csv('./repdata_data_activity/activity.csv')
impute <- input
# Fill NA = 0 
input$steps[is.na(input$steps)] <- 0

calMean <- sapply(split(input$steps, f= as.factor(input$date)), sum)
dateStore <- list(unique(input$date))

hist(calMean, col = 'pink', breaks = 10, xlab = "Number of steps taken", main = "Histogram of Number of steps")

paste("Mean steps per day:", mean(input$steps), "Standard deviation of steps per day", sd(input$steps))

calMean <- sapply(split(input$steps, f= as.factor(input$interval)), mean)
plot(y= calMean, x=unique(input$interval) , type="l", col='red')
points(y= calMean, x=unique(input$interval))

paste("Max value interval is ", unique(input$interval)[which.max(calMean)])

data.frame(steps=sum(is.na(impute$steps)), 
                      interval=sum(is.na(impute$interval)), 
                   date=sum(is.na(impute$date)))
paste("Total number of NA instances 2304")

for(x in 1:17568) {
  if(is.na(impute[x, 1])==TRUE) {
    impute[x, 1] <- all_mean[which(row.names(all_mean) == impute[x,3]),]
  }
}
head(impute)

       