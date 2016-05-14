
# CONSTANTS DEFINITIONS

date_col <- 'datetime'
data_url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
downloaded_file <- 'data.zip'
date_format <- "%d/%m/%Y %H:%M:%S"
file_name <- 'household_power_consumption.txt'
begin <- "1/2/2007 00:00:00"
end <- "3/2/2007 00:00:00"
par(mar=(c(4, 4, 4, 1) + 0.1))

# Load data

load_file <- function(file=file_name) read.table(file, header=TRUE, na.strings="?", sep=";")

load_file_from_the_internet <- function() {
    if (! file.exists(downloaded_file))
        download.file(data_url, downloaded_file)
    f <- unz(downloaded_file, file_name)
    load_file(f)
}


# Add posix date

format_date <- function(date)  as.POSIXct(date, format=date_format)
update_date <- function(data) with(data, format_date(paste(Date, Time)))
begin <- format_date(begin)
end <- format_date(end)
add_date_col <- function(data) { 
    data[,date_col] <- update_date(data) 
    data
}

# Filter by date

select_data <- function(data) { 
    dates <- data[,date_col]
    indices <- which(dates >= begin & dates < end)
    data[indices,]
}

# Plot 1

plot_global_active_power_hist <- function(data) {
    hist(data$Global_active_power, xlab="Global active power (kilowatts)", col='red', main="Global Active Power")
}

# Plot 2

plot_global_active_power <- function(data) {
    plot(data[,date_col], data$Global_active_power, xlab="", ylab="Global active power (kilowatts)", main="", type='l')
}

# Plot 3

plot_submeterings <-function(data, box_type="n") {
    dates <- data[,date_col]
    plot(dates, data$Sub_metering_1, xlab="", ylab="Energy sub metering", main="", type='l')
    lines(dates, data$Sub_metering_2, col='red')
    lines(dates, data$Sub_metering_3, col='blue')
    legend("topright", c("Sub_meterings_1", "Sub_meterings_2", "Sub_meterings_3"), col=c("black", "red", "blue"), lwd=c(1,1,1), bty=box_type)
}

# Plot 4.1

plot_voltage <- function(data) {
    plot(data[,date_col], data$Voltage, type='l', xlab='datetime', ylab='Voltage')
}

# Plot 4.2

plot_global_reactive_power <- function(data) {
    plot(data[,date_col], data$Global_reactive_power, type='l', xlab='datetime', ylab='Global reactive power')
}

# Plot 4

plot_collage <- function(data) {
    par(mfrow=c(2,2))
    plot_global_active_power(data)
    plot_voltage(data)
    plot_submeterings(data)
    plot_global_reactive_power(data)
}

# Output to PNG

plot_to_png <- function(data, plot_function, filename, ...) {
    png(filename=filename)
    plot_function(data, ...)
    dev.off()
}

# Plot all as expected

plot_exercise <- function(data) {
    plot_to_png(data, plot_global_active_power_hist , 'plot1.png')
    plot_to_png(data, plot_global_active_power, 'plot2.png')
    plot_to_png(data, plot_submeterings, 'plot3.png', box_type="o")
    plot_to_png(data, plot_collage, 'plot4.png')
}


data <- load_file_from_the_internet()
data <- add_date_col(data)
data <- select_data(data)
plot_to_png(data, plot_global_active_power, 'plot2.png')

