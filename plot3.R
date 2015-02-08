# Constants
file2download <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
dataDir = "./data/"

downloadFile <- function(url = file2download, dir = dataDir) {
  # Check if the data directory does not exist, if it does not, create it
  if(!file.exists(dir)) {dir.create(dir)}

  # Create a temporary ZIP file
  temp <- tempfile(fileext=".zip")
  # Download the file
  download.file(url, temp, method="curl", mode="wb")
  
  # get the name of the first file in the zip archive
  fname = unzip(temp, list=TRUE)$Name[1]
  # unzip the file to the temporary directory
  unzip(temp, files=fname, exdir=dataDir, overwrite=TRUE)
  # Remove the temp file  
  unlink(temp)
  
  # fpath is the full path to the extracted file
  fpath = file.path(dataDir, fname)
}

convertDateAndTimeToPosix <- function(dataFrame, grepString) {
  #subset <- dataFrame[grep(grepString, dataFrame$Date), ]
  dataFrame[grep(grepString, dataFrame$Date), ]
}

readFileToMemory <- function(filename) {
  # Get the whole file to memory (time consuming)
  read.csv(filename, sep = ";", header = TRUE, na.strings = "?", colClasses = c(rep("character", 2), rep("numeric", 7)))  
}

extractSubset <- function(dataFrame) {
  # Extract content for first day
  householdSubset1 <- convertDateAndTimeToPosix(dataFrame, "^1/2/2007")
  # Extract content for second day
  householdSubset2 <- convertDateAndTimeToPosix(dataFrame, "^2/2/2007")
  
  # Join both columns
  rbind(householdSubset1, householdSubset2)
}

mergeDateAndTimeColumns <- function(dataFrame) {
  #Convert all dates to Date Class
  dataFrame$Date <- as.Date(dataFrame$Date , "%d/%m/%Y")
  
  # Order Date column
  dataFrameWithDateTime <- dataFrame[order(dataFrame$Date ),]  
  # Add Datetime column
  dataFrameWithDateTime <- within(dataFrameWithDateTime, Datetime <- as.POSIXct(paste(dataFrameWithDateTime$Date, dataFrameWithDateTime$Time), format="%Y-%m-%d %H:%M:%S"))
  # Remove Date and Time columns
  dataFrameWithDateTime[, -(1:2)]
}

saveMyPlot <- function(filename) {
  dev.copy(png, file = filename)
  dev.off()
  graphics.off()
}

plotCharts <- function(dataFrame) {
  # Plot 3
  plot(dataFrame$Datetime, dataFrame$Sub_metering_1, xlab = "", ylab = "Energy sub metering", type="l")
  lines(dataFrame$Datetime, dataFrame$Sub_metering_2, col = "red")
  lines(dataFrame$Datetime, dataFrame$Sub_metering_3, col = "blue")
  legend("topright", pch = "-", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex = 0.75)
  saveMyPlot("plot3.png")
}

# Get file from the internet and read to memory
rawDataFrame <- readFileToMemory(downloadFile())
# Extract the subset for the dates needed
householdSubset <- extractSubset(rawDataFrame)
# Join the Date and Time columns
householdSubsetDateTime <- mergeDateAndTimeColumns(householdSubset)
# Plot and save Charts
plotCharts(householdSubsetDateTime)

#End of file