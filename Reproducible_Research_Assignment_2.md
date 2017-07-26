# Reproducible Research Assignment 2




This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

#### Loading and preprocessing the data

Show any code that is needed to

Load the data (i.e. read.csv())

```r
activity_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
download.file(activity_url, temp)
unzip(temp, "activity.csv")
# note that here I modified your original read.table() which did not work
activityNA <- read.csv("activity.csv", header=TRUE, sep = ",")
activity <- na.omit(activityNA)
unlink(temp)
```

Process/transform the data (if necessary) into a format suitable for your analysis

#### What is mean total number of steps taken per day?

```r
stepsperday <- aggregate(steps~date, activity, sum)
print(paste("Mean total steps taken each day", abs(mean(stepsperday$steps))))
```

```
## [1] "Mean total steps taken each day 10766.1886792453"
```
For this part of the assignment, you can ignore the missing values in the dataset.

#### Calculate the total number of steps taken per day?

```r
totalstepsperday <- sum(activityNA$steps, na.rm = TRUE)
print(paste("Total steps taken each day",totalstepsperday))
```

```
## [1] "Total steps taken each day 570608"
```

If you do not understand the difference between a histogram and a barplot, research the difference between them. 

#### Make a histogram of the total number of steps taken each day?

```r
hist(stepsperday$steps, xlab = "Steps taken each day", ylab = "Count", main= "Histogram of steps taken each day", col = 4)
```

![](Reproducible_Research_Assignment_2_files/figure-html/Histogram-1.png)<!-- -->

#### Calculate and report the mean and median of the total number of steps taken per day?

```r
print(paste("Mean total number of steps taken per day", mean(stepsperday$steps)))
```

```
## [1] "Mean total number of steps taken per day 10766.1886792453"
```

```r
print(paste("Median total number of steps taken per day", median(stepsperday$steps)))
```

```
## [1] "Median total number of steps taken per day 10765"
```

#### What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). 


```r
minuteaverage <- aggregate(steps~interval, data=activity, FUN=mean, na.rm=TRUE)
plot(x = minuteaverage$interval, y = minuteaverage$steps,  type = "l") 
```

![](Reproducible_Research_Assignment_2_files/figure-html/plot-1.png)<!-- -->

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
Imputing missing values


```r
maxsteps <- max(minuteaverage$steps)
for (i in (1:nrow(minuteaverage))) 
{
    if (minuteaverage$steps[i] == maxsteps) {
        minuteintervalmaxstep <- minuteaverage$interval[i]
    }
}
print(paste("5 minute interval on average which contains max no. of steps is",minuteintervalmaxstep)) 
```

```
## [1] "5 minute interval on average which contains max no. of steps is 835"
```


Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


#### Calculate and report the total number of missing values in the dataset? 
(i.e. the total number of rows with NAs)


```r
sum(is.na(activityNA$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_missed_data <- activityNA
for (i in (1:nrow(activityNA))) 
{
    if(is.na(activity_missed_data$steps[i])) 
    { 
        minute_pointer <- activity_missed_data$interval[i] 
        for (j in 1:nrow(minuteaverage))  
        {
            if (minuteaverage$interval[j] == minute_pointer) {
                activity_missed_data$steps[i] <- minuteaverage$steps[j]
               }
        }
    }
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

####Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
ttlstepseachdayNA <- aggregate(steps~date, data=activityNA, FUN=sum, na.rm=TRUE)
hist(ttlstepseachdayNA$steps, main = "Total steps eachday with NA", col = 4)
```

![](Reproducible_Research_Assignment_2_files/figure-html/histtotalsteps-1.png)<!-- -->

```r
meanstepsNA <- mean(ttlstepseachdayNA$steps)
medianstepsNA <- median(ttlstepseachdayNA$steps)

ttlstepseachday <- aggregate(steps~date, data=activity, FUN=sum, na.rm=TRUE)
hist(ttlstepseachday$steps, main = "Total steps each day", col = 4)
```

![](Reproducible_Research_Assignment_2_files/figure-html/histtotalsteps-2.png)<!-- -->

```r
meansteps <- mean(ttlstepseachday$steps)
mediansteps <- median(ttlstepseachday$steps)

meanstepsNA
```

```
## [1] 10766.19
```

```r
medianstepsNA
```

```
## [1] 10765
```

```r
meansteps
```

```
## [1] 10766.19
```

```r
mediansteps
```

```
## [1] 10765
```

####Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

```r
require(lubridate)
```

```
## Loading required package: lubridate
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
weekday <- wday(activityNA$date)
```

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
week <- wday(activityNA$date)
wkday <- week
for (i in 1:nrow(activityNA)) 
{
    if(week[i] == 1 | week[i] == 7) {
      wkday[i] <- "weekend"
    }
    if(week[i] == 2 | week[i] == 3 | week[i] == 4 | week[i] == 5 | week[i] == 6) {
      wkday[i] <- "weekday"
    }
}
activity_missed_data$weekday <-wkday
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
weekday <- grep("weekday",activity_missed_data$weekday)
weekday_days<- activity_missed_data[weekday,]
weekend_days <- activity_missed_data[-weekday,]
```

# What  is the average daily activity pattern?

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken,averaged across all days (yaxis)


```r
minutes_average_weekday <- aggregate(steps~interval, data=weekday_days, FUN=mean, na.rm=TRUE)
minutes_average_weekend <- aggregate(steps~interval, data=weekend_days, FUN=mean, na.rm=TRUE)
plot(x = minutes_average_weekday$interval, y = minutes_average_weekday$steps, type = "l") 
```

![](Reproducible_Research_Assignment_2_files/figure-html/timeseriesplot-1.png)<!-- -->
