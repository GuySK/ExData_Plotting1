
## ------------------------------------------------------------------------
uri <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
zipfn <- "household_power_consumption.zip"
unzip(zipfn)
fn <- "household_power_consumption.txt"


## ----checkSize, results='hide'-------------------------------------------
checkSize <- function(fn) {
    finfo <- file.info(fn)                  # get file information
    fsize <- round(finfo[1,1]/(2**20),0)    # file size in MB
    memsize <- memory.limit()               # system memory available for R
    round(fsize / memsize,2)                # report proportion 
}


## ----check---------------------------------------------------------------
checkSize(fn)
hpc <- read.table(fn, sep=";", header=TRUE)
head(hpc,2)


## ----dat, results='hide'-------------------------------------------------
dat <- function(x) {as.Date(as.character(x), "%d/%m/%Y")}


## ------------------------------------------------------------------------
from.date <- as.Date("01-02-2007", "%d-%m-%Y")
to.date <- as.Date("02-02-2007", "%d-%m-%Y")

hpc2 <- hpc[((dat(hpc[,1]) == from.date) | (dat(hpc[,1]) == to.date)),]
head(hpc2[,1:2],2)
tail(hpc2[,1:2],2)

rm(hpc)
hpc <- hpc2
rm(hpc2)


## ----Plot1, results='hide'-----------------------------------------------
fac.as.numeric <- function(x) {as.numeric(as.character(x))}
gap <- fac.as.numeric(hpc[,3])

plname <- "plot1.png"
png(plname, height=480, width=480)

xlab="Global Active Power (kilowatts)"
title <- "Global Active Power"
color <- "red"
hist(gap,col=color, xlab=xlab, main=title)

dev.off()


## ----TimeStamp-----------------------------------------------------------
hpc.dt <- strptime(paste(hpc[,1], hpc[,2]), format="%d/%m/%Y %H:%M:%S")
hpc2 <- cbind(hpc.dt,hpc[,3:9])
colnames(hpc2)[1] <- "TimeStamp"
hpc2$Global_active_power <- gap


## ----Plot2,results='hide'------------------------------------------------
plname <- "plot2.png"
png(plname, height=480, width=480)

ylab <- "Global Active Power (kilowatts)"
with(hpc2, plot(TimeStamp, Global_active_power, xlab="", ylab=ylab, type="l"))

dev.off()


## ------------------------------------------------------------------------
hpc2$Sub_metering_1 <- fac.as.numeric(hpc2$Sub_metering_1)
hpc2$Sub_metering_2 <- fac.as.numeric(hpc2$Sub_metering_2)


## ----Plot3,results='hide'------------------------------------------------
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


## ----Plot4.1-------------------------------------------------------------
hpc2$Voltage <- fac.as.numeric(hpc2$Voltage)
hpc2$Global_reactive_power <- fac.as.numeric(hpc2$Global_reactive_power)


## ----Plot4.2,results='hide'----------------------------------------------
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

