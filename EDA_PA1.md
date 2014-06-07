Exploratory Data Analysis
=========================
# Course Project 1

## Loading all data into a data frame
The following code is used to download the dataset and unzip it. Before reading it unto a dataframe, we will check if there is enough memory in the system. 


```r
uri <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
zipfn <- "household_power_consumption.zip"
unzip(zipfn)
fn <- "household_power_consumption.txt"
```

The following function is used to check if there is enough memory available.


```r
checkSize <- function(fn) {
    finfo <- file.info(fn)                  # get file information
    fsize <- round(finfo[1,1]/(2**20),0)    # file size in MB
    memsize <- memory.limit()               # system memory available for R
    round(fsize / memsize,2)                # report proportion 
}
```

In our case, we have no problems in our system. The file size is just a small proportion of our memory. So, we go on to read it.


```r
checkSize(fn)
```

```
## [1] 0.04
```

```r
hpc <- read.table(fn, sep=";", header=TRUE)
head(hpc,2)
```

```
##         Date     Time Global_active_power Global_reactive_power Voltage
## 1 16/12/2006 17:24:00               4.216                 0.418 234.840
## 2 16/12/2006 17:25:00               5.360                 0.436 233.630
##   Global_intensity Sub_metering_1 Sub_metering_2 Sub_metering_3
## 1           18.400          0.000          1.000             17
## 2           23.000          0.000          1.000             16
```

## Selecting data between required dates
The Date variable is stored as factor, so the following function is defined to simplify the selection of data in the required timeframe.


```r
dat <- function(x) {as.Date(as.character(x), "%d/%m/%Y")}
```

Now the appropriate dates are selected and the rest is removed from the environment in order to free memory. The resulting data set is named hpc, for 'household power consumption'. 


```r
from.date <- as.Date("01-02-2007", "%d-%m-%Y")
to.date <- as.Date("02-02-2007", "%d-%m-%Y")

hpc2 <- hpc[((dat(hpc[,1]) == from.date) | (dat(hpc[,1]) == to.date)),]
head(hpc2[,1:2],2)
```

```
##           Date     Time
## 66637 1/2/2007 00:00:00
## 66638 1/2/2007 00:01:00
```

```r
tail(hpc2[,1:2],2)
```

```
##           Date     Time
## 69515 2/2/2007 23:58:00
## 69516 2/2/2007 23:59:00
```

```r
rm(hpc)
hpc <- hpc2
rm(hpc2)
```

## Plot 1
We convert factors to numerics as needed and proceed to plot. Plots are saved to disc by using the png device and therefore they are not shown in this document, but they are available as separate files.


```r
fac.as.numeric <- function(x) {as.numeric(as.character(x))}
gap <- fac.as.numeric(hpc[,3])

plname <- "plot1.png"
png(plname, height=480, width=480)

xlab="Global Active Power (kilowatts)"
title <- "Global Active Power"
color <- "red"
hist(gap,col=color, xlab=xlab, main=title)

dev.off()
```

## Plot 2
We will now create a new variable TimeStamp to replace Date and Time, since we need to plot all points. We use a new dataframe, just in case we need the old Date and Time variables again.


```r
hpc.dt <- strptime(paste(hpc[,1], hpc[,2]), format="%d/%m/%Y %H:%M:%S")
hpc2 <- cbind(hpc.dt,hpc[,3:9])
colnames(hpc2)[1] <- "TimeStamp"
hpc2$Global_active_power <- gap
```

We are now, ready to plot.

```r
plname <- "plot2.png"
png(plname, height=480, width=480)

ylab <- "Global Active Power (kilowatts)"
with(hpc2, plot(TimeStamp, Global_active_power, xlab="", ylab=ylab, type="l"))

dev.off()
```

## Plot 3
Plot 3 shows all sub meterings together. Before plotting, we convert factors to numeric as usual.


```r
hpc2$Sub_metering_1 <- fac.as.numeric(hpc2$Sub_metering_1)
hpc2$Sub_metering_2 <- fac.as.numeric(hpc2$Sub_metering_2)
```


```r
plname <- "plot3.png"
png(plname, height=480, width=480)

ylabel <- "Energy Sub metering"
with(hpc2, plot(TimeStamp, Sub_metering_1, ylab=ylabel, type="n"))
with(hpc2, lines(TimeStamp, Sub_metering_1))
with(hpc2, lines(TimeStamp, Sub_metering_2,col="red"))
with(hpc2, lines(TimeStamp, Sub_metering_3,col="blue"))
legend("topright",lty=1, pt.cex=1, cex=0.5, seg.len=0.5, 
       col=c("black","red","blue"), 
       legend=colnames(hpc2)[6:8])

dev.off()
```

## Plot 4
Plot 4 is particular because it contains four different graphs. Before plotting, we need to convert factors to numeric, as usual.


```r
hpc2$Voltage <- fac.as.numeric(hpc2$Voltage)
hpc2$Global_reactive_power <- fac.as.numeric(hpc2$Global_reactive_power)
```

In this case, we use the parameter 'mfrow' and the order in which the graphs are plotted to define the position of each graph in the page.


```r
plname <- "plot4.png"
png(plname, height=480, width=480)
par(mfrow = c(2, 2))

# Global Active Power 1/4
with(hpc2, plot(TimeStamp, Global_active_power, xlab="", ylab=ylab, cex.lab=0.75, type="l"))

# Voltage 2/4
with(hpc2, plot(TimeStamp, Voltage, xlab="datetime", cex.lab=0.75, type="l"))

# Energy ub metering 3/4
with(hpc2, plot(TimeStamp, Sub_metering_1, xlab="", ylab="", type = "n"))
title(ylab="Energy Sub metering", xlab="", cex.lab=0.75)
legend("topright", legend=colnames(hpc2)[6:8], col=c("black","blue","red"),
       lty=1, cex=0.5, seg.len=0.5, bty="n", yjust=1,)
with(hpc2, lines(TimeStamp, Sub_metering_1))
with(hpc2, lines(TimeStamp, Sub_metering_2,col="red"))
with(hpc2, lines(TimeStamp, Sub_metering_3,col="blue"))

# Global Reactive Power 4/4
with(hpc2, plot(TimeStamp, Global_reactive_power, type="l",
                xlab="datetime", cex.lab=0.75))
dev.off()
```
