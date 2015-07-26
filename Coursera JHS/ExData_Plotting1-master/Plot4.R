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

######################################################
## Output Plot 4
dev.copy(png, file="plot4.png", height=500, width=500)

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(powa, {
  plot(Global_active_power~Datetime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~Datetime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~Datetime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~Datetime,type="l",col='Red')
  lines(Sub_metering_3~Datetime,type="l",col='Blue')
  legend("topright", col=c("black", "red", "blue"), cex=0.5, lty="solid",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~Datetime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})

dev.off()
