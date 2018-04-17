### Code for reading in the dataset and/or processing the data

    temp <- tempfile()
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp, mode="wb")
    data <- unz(temp, "activity.csv")
    activity <- read.csv(data, sep=",", header=T)
    activity$date <- as.Date(activity$date)

    ## Warning in strptime(xx, f <- "%Y-%m-%d", tz = "GMT"): unknown timezone
    ## 'zone/tz/2018c.1.0/zoneinfo/America/Phoenix'

### Histogram of the total number of steps taken each day

    stepsEachDay <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)
    hist(stepsEachDay$steps, col = "orange", main = "Total number steps taken each day", xlab = "Number of steps")

![](Reproducible_Research_Assignment_1_files/figure-markdown_strict/histTotalSteps-1.png)

### Mean and median number of steps taken each day

    meanSteps <- mean(stepsEachDay$steps, na.rm = TRUE)
    medianSteps <- median(stepsEachDay$steps, na.rm = TRUE)

1.  The mean number of steps taken each day are 1.076618910^{4}
2.  The median number of steps taken each day are 10765

### Time series plot of the average number of steps taken

    averageActivity <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)
    plot(averageActivity$interval, averageActivity$steps, col= "orange",  type = "l", xlab = "5-minute interval", ylab = "Average number of steps", main = "Time series Plot" )

![](Reproducible_Research_Assignment_1_files/figure-markdown_strict/timeSeries-1.png)

### The 5-minute interval that, on average, contains the maximum number of steps

    maxInterval <- averageActivity$interval[[which.max(averageActivity$steps)]]

The 5-minute interval corresponding to maximum number of steps, on
average across all days in dataset 835

### Code to describe and show a strategy for imputing missing data

    totalNA <- sum((is.na(activity$steps) | is.na(activity$date) | is.na(activity$interval)))

The total number of missing values in the dataset 2304

#### Create a new dataset that is equal to the original dataset but with the missing data filled in.

    stepsFillNA <- rep(0, 17568)
    activityFillNA <- activity
    cbind(activityFillNA, stepsFillNA)
    MeanStepsPerInterval <- aggregate(steps ~ interval, data = activityFillNA, mean, na.rm=TRUE)
    for (i in 1:nrow(activityFillNA))
    {
            if (is.na(activityFillNA$steps[i]))
            {
               interval_pointer <- activityFillNA$interval[i]
                            for (j in 1:nrow(MeanStepsPerInterval))
                            {
                                    if(MeanStepsPerInterval$interval[j] == interval_pointer) {
                                     activityFillNA$stepsFillNA[i] <- (MeanStepsPerInterval$steps[j])
                                    }
                                    
                            }
            
            }
            else {
                    activityFillNA$stepsFillNA[i] <- activityFillNA$steps[i]
            }
    }

### Histogram of the total number of steps taken each day after missing values are imputed

    stepsEachDayFillNA <- aggregate(stepsFillNA ~ date, activityFillNA, sum, na.rm = TRUE)
    hist(stepsEachDayFillNA$stepsFillNA, col= "orange")

![](Reproducible_Research_Assignment_1_files/figure-markdown_strict/histTotalStepsFillNA-1.png)

    meanStepsFillNA <- mean(stepsEachDayFillNA$stepsFillNA)
    medianStepsFillNA <- median(stepsEachDayFillNA$stepsFillNA)

Mean total number of steps taken per day 1.076618910^{4} Median total
number of steps taken per day 1.076618910^{4}

#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    activityFillNA$day <- weekdays(as.Date(activityFillNA$date))
    for (i in 1:nrow(activityFillNA)) {
            if (activityFillNA$day[i] == "Saturday") {
                    activityFillNA$day[i] = "Weekend"
            }
            else if(activityFillNA$day[i] == "Sunday") {
                    activityFillNA$day[i] = "Weekend"
            }
            else {
                    activityFillNA$day[i] = "Weekday"
            }
    }
    activityFillNA$day <- as.factor(activityFillNA$day)

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

    library(lattice)
    avg_steps_days <- aggregate(stepsFillNA ~ interval + day, activityFillNA, mean)
    xyplot(stepsFillNA ~ interval| day ,data = avg_steps_days, col= "orange", layout=(c(1,2)),type="l",xlab="5 min interval", ylab="Number of steps")

![](Reproducible_Research_Assignment_1_files/figure-markdown_strict/panelPlot-1.png)
