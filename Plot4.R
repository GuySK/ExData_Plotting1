# Exploratory Data Analysis - Course Project 1
# Plot 4
# Working directory
setwd("C:/Users/AAB330/Google Drive 2/Training/Johns Hopkins/ExploratoryDataAnalysis/Projects")
#
# Identifying the data set
dir(pattern="^house")
# >"household_power_consumption.txt"
#
# Reading the DS
fn <- dir(pattern="^house")
EPC <- read.table(fn, sep=";", header=TRUE)
# Inspecting the DS
View(EPC)
EPC[1,1]
# > [1] 16/12/2006
# >1442 Levels: 1/1/2007 1/1/2008 1/1/2009 1/1/2010 1/10/2007 1/10/2008 ... 9/9/2010

# convert date and time columns to a new column TimeStamp 
epc.dt <- strptime(paste(EPC[,1], EPC[,2]), format="%d/%m/%Y %H:%M:%S")
head(epc.dt)
EPC <- cbind(epc.dt,EPC[,3:9])
colnames(EPC)[1] <- "TimeStamp"

# Select time frame to plot
dat.low <- strptime("2007-02-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
dat.up <- strptime("2007-02-02 23:59:59", format="%Y-%m-%d %H:%M:%S")
EPC <- EPC[EPC$TimeStamp >= dat.low,]
EPC <- EPC[EPC$TimeStamp <= dat.up,]
View(EPC)

# getting rid of na values
nrow(EPC)
# [1] 3000
EPC <- EPC[!is.na(EPC[,1]),]
nrow(EPC)
# [1] 2880

# Checking there are no more NA values in other columns
for (i in 2:8) { print(sum(is.na(EPC[,i])))}
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0
# [1] 0

# Generate data set for Plot 1
GlobalActivePower <- EPC[!is.na(EPC[,2]),1:2]

# setting up data for  Plot 4.1

# step 1 - casting factors as numeric
fac.as.numeric <- function(x) {as.numeric(as.character(x))}
GlobalActivePower[,2] <- fac.as.numeric(GlobalActivePower[,2])

# setting up data for Plot 4.2
class(EPC$Voltage)
# [1] "factor"
EPC$Voltage <- fac.as.numeric(EPC$Voltage)

# setting up data for plot  4.3
class(EPC$Sub_metering_1)
# [1] "factor"
class(EPC$Sub_metering_2)
# [1] "factor"
class(EPC$Sub_metering_3)
# [1] "numeric"
EPC$Sub_metering_1 <- fac.as.numeric(EPC$Sub_metering_1)
EPC$Sub_metering_2 <- fac.as.numeric(EPC$Sub_metering_2)

# setting up data for Plot 4.4
class(EPC$Global_reactive_power)
# [1] "factor"
EPC$Global_reactive_power <- fac.as.numeric(EPC$Global_reactive_power)

# setting  up graphs distribution on page
par(mfrow = c(2, 2))

# Plotting
# Global Active Power 1/4
with(GlobalActivePower, plot(TimeStamp, Global_active_power, xlab="", ylab=ylab, cex.lab=0.75, type="l"))

# Voltage 2/4
with(EPC, plot(TimeStamp, Voltage, xlab="datetime", cex.lab=0.75, type="l"))

# Energy ub metering 3/4
with(EPC, plot(TimeStamp, Sub_metering_1, xlab="", ylab="", type = "n"))
legend("topright",lty=1, cex=0.75, seg.len=0.5, bty="n", yjust=1, col=c("black","blue","red"), legend=colnames(EPC)[6:8])
title(ylab="Energy Sub metering", xlab="", cex.lab=0.75)
with(EPC, lines(TimeStamp, Sub_metering_1))
with(EPC, lines(TimeStamp, Sub_metering_2,col="red"))
with(EPC, lines(TimeStamp, Sub_metering_3,col="blue"))

# Global reactive power 4/4 
with(EPC, plot(TimeStamp, Global_reactive_power, type="l", xlab="datetime", cex.lab=0.75))

# ----- End of Plot4 ----- #





