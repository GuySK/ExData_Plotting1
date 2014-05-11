# Exploratory Data Analysis - Course Project 1
# Plot 2

# set up Working directory
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

# converting date and time columns to a new column TimeStamp 
epc.dt <- strptime(paste(EPC[,1], EPC[,2]), format="%d/%m/%Y %H:%M:%S")
head(epc.dt)
EPC <- cbind(epc.dt,EPC[,3:9])
colnames(EPC)[1] <- "TimeStamp"

# Selecting time frame to plot
dat.low <- strptime("2007-02-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
dat.up <- strptime("2007-02-02 23:59:59", format="%Y-%m-%d %H:%M:%S")
EPC <- EPC[EPC$TimeStamp >= dat.low,]
EPC <- EPC[EPC$TimeStamp <= dat.up,]
View(EPC)

# Generating data set for Plot 1 and others
GlobalActivePower <- EPC[!is.na(EPC[,2]),1:2]
head(GlobalActivePower)
nrow(GlobalActivePower)

# ----- Plot 2 ------ #
setwd("C:/Users/AAB330/Google Drive 2/Training/Johns Hopkins/ExploratoryDataAnalysis/Projects")
load("C:/Users/AAB330/Google Drive 2/Training/Johns Hopkins/ExploratoryDataAnalysis/Projects/Env.RData")
ylab <- "Global Active Power (kilowatts)"
with(GlobalActivePower, plot(TimeStamp, Global_active_power, xlab="", ylab=ylab, type="l"))
# ----- End of Plot 2 ----- #





