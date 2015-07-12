#####
# plot2.R
#####
# programming assignment 1
# explorative data analysis course
#####
# Christoph Baehr
#####

plot2 <- function() {
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
# plot lines diagram
#

# hmm - wat to use the same locale but be able to change back once plotted
loc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# output device
png( filename = "plot2.png",
         width = 480, height = 480, units = "px",
         bg = "white")

# the plot
with(alldata,
     plot(Time, Global_active_power,
          main = "Global Active Power",
          type="l",
          xlab = "",
          ylab = "Global Active Power (kilowatts)"))


dev.off()

# change local back to original value
Sys.setlocale("LC_TIME", loc)

}
