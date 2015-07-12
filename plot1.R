#####
# plot1.R
#####
# programming assignment 1
# explorative data analysis course
#####
# Christoph Baehr
#####

plot1 <- function() {
#
# read data and subset
#
fromtime <- as.POSIXlt(paste("01/02/2007","00:00:00"), 
                    format="%d/%m/%Y %H:%M:%S",
                    tz="GMT",
                    usetz = FALSE)
totime <- as.POSIXlt(paste("02/02/2007","24:00:00"), 
                  format="%d/%m/%Y %H:%M:%S",
                  tz="GMT",
                  usetz = FALSE)
                  
filename <- "household_power_consumption.txt"

# open connection
con <- file(filename, open="r")
# read first line - the headers
headers <- scan(con, what=character(), sep=";", nlines=1, quiet = TRUE)
# initialize 

#
# loop over lines
# break if return value is not data frame - eof is reached
#
repeat   {
        linedata <- try({read.csv(con, sep=";", na.strings="?", nrows=1, 
                                      stringsAsFactors=FALSE, header=FALSE)},
                        TRUE)
        if (class(linedata) != "data.frame") break
        
        # convert date, time
        # tz preset to GMT
        # time needs to be full date/time
        linedata$V2 <- as.POSIXlt(paste(linedata$V1,linedata$V2), 
                                  "%d/%m/%Y %H:%M:%S", 
                                  tz="GMT", 
                                  usetz=FALSE)
        linedata$V1 <- as.POSIXlt(linedata$V1, 
                                  "%d/%m/%Y", 
                                  tz="GMT", 
                                  usetz=FALSE)
        # check if line is in desired date range
        if (linedata$V2 >= fromtime & linedata$V1 < totime)   {
                # check if target data frame exists
                if (exists("alldata"))   {
                        alldata[nrow(alldata)+1,] <- linedata
                }
                # first line to be stored - create data object
                else {
                        alldata <- data.frame(c(linedata[1,]))
                        names(alldata) <- headers
                }
        }   
}
# close
close(con)

#
# plot histogram
#

# output device
png( filename = "plot1.png",
         width = 480, height = 480, units = "px",
         bg = "white")

hist(alldata$Global_active_power, 
     col="red",
     xlab = "Global Active Power (kilowatts)",
     main = "Global Active Power")


dev.off()

}
