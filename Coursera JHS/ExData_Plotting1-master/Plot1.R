# Project 1

##  Data readin and date change
df <- read.csv("~/household_power_consumption.txt", header=T, stringsAsFactors=F)
df$Date <- as.Date(df$Date, format="%d/%m/%Y")

## Subset 
powa <- subset(df, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
rm(df)
head(powa,2)

## Converting date and time 
datetime <- paste(as.Date(powa$Date), powa$Time)
powa$Datetime <- as.POSIXct(datetime)

#############################################
##Output Plot 1
dev.copy(png, file="plot1.png", height=500, width=500)

hist(powa$Global_active_power, main="Global Active Power", 
     xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red")

dev.off()
