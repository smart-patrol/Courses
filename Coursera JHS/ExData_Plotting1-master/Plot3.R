# Project 1

##  Data readin and date change
df <- read.csv("~/household_power_consumption.txt", header=T, stringsAsFactors=F)
df$Date <- as.Date(df$Date, format="%d/%m/%Y")

## Subsetting the data
powa <- subset(df, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
rm(df)
head(powa,2)

## Converting date and time 
datetime <- paste(as.Date(powa$Date), powa$Time)
powa$Datetime <- as.POSIXct(datetime)

##################################################
## Output Plot 3
dev.copy(png, file="plot3.png", height=500, width=500)

plot(powa$Sub_metering_1~powa$Datetime,type="l",ylab="Energy sub metering",xlab="") 
lines(powa$Sub_metering_2~powa$Datetime,type="l",col="red") 
lines(powa$Sub_metering_3~powa$Datetime,type="l",col="blue") 
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black","red","blue")
       ,cex=0.75,lty="solid", xjust=1 )

dev.off()
