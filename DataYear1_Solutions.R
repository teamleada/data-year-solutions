##################################
### Data Year 1 Solutions in R ###
##################################

################################################################################################################
# Initial Prep Code
td <- read.csv("bike_trip_data.csv", stringsAsFactors = F)

# Chron is a package in R to create datetime objects.
install.packages('chron')
library('chron')

################################################################################################################


# Problem 1: What was the average total time (in minutes) used by a bicycle in the data?

# Comments: The goal of this question was to determine how much wear had occured on the bikes during the
#           period of the data collected. It was not clearly phrased. The solution I intended was 4288.087.

# Rcode
mean(rowsum(td$Duration, td$Bike..)/60)

################################################################################################################

# Problem 2: What was the most popular day by trip frequency in this data set?

# Comments: Many ways to do this as it is essentially a query. Answer: 9/25/13 with 1264 rides.

#Rcode
chardate <- as.character(td$Start.Date)
split <- strsplit(chardate, " ", fixed = TRUE)
days <- sapply(split, "[[", 1)
sort(table(days), decreasing = T)

################################################################################################################

# Problem 3 (easier): Assuming there are 30 bikes per station, find what date the bikes first need to be
# rebalanced. As in, there are zero bikes in a terminal for a customer to rent. Do not take into account
# the Start.Date and End.Date columns.

# Comments: This version was easier because by not including the Start.Date and End.Date columns you could
#           assume that each transaction was already sorted by time. Assuming that you could simply create
#           a counter which +1 for a End.Terminal and -1 for a Start.Terminal.
#           Answer: 9/2/2013 at 11:55, Terminal 54

# Rcode

# Creating 30 bike counter
num_terminals <- length(unique(td$Start.Terminal))
bikes <- 30
bike_counter <- as.data.frame(cbind(rep(bikes,times=num_terminals), sort(unique(td$Start.Terminal))))
colnames(bike_counter) <- c("Num", "Terminal")

# Resets bike_counter to 30
bike_counter <- as.data.frame(cbind(rep(bikes,times=num_terminals), sort(unique(td$Start.Terminal))))
colnames(bike_counter) <- c("Num", "Terminal")

routes_two <- data.frame(td$Start.Terminal, td$End.Terminal)

for(i in 1:nrow(routes_two)) {
  trip <- routes_two[i, ]
  start <- as.integer(trip[1]) #minus 1
  end <- as.integer(trip[2]) #add 1
  row_minus <- which(bike_counter$Terminal == start)
  row_add <- which(bike_counter$Terminal == end)
  bike_counter[row_minus, 1] = bike_counter[row_minus, 1] - 1
  bike_counter[row_add, 1] = bike_counter[row_add, 1] +  1
  # Checks if bike count less than 0
  if(sum(bike_counter$Num <= 0) >= 1) {
    print("Rebalancing Required")
    print(td$Start.Date[i])
    break
  }
}

################################################################################################################


# Problem 3 (harder):  Assuming there are 30 bikes per station, find what date the bikes first need to be
# rebalanced. As in, there are zero bikes in a terminal for a customer to rent. You must take into account
# the Start.Date and End.Date of the transactions.

# Comments: In the harder version you must take into the account for the duration of the bike trip by combining
#           the Start.Date and End.Date and sorting the result. Interestingly because the duration of the bike
#           rides are ~ 4 minutes this only changes the rebalancing time, not the date.
#           Answer: (09/02/13 11:33:00)

# Rcode
# Function that converts Start.Date and End.Date to date time objects
date_time <- function(x) {
  dates <- paste(x, ":00", sep = "")
  time_split <- strsplit(dates, " ", fixed = T)
  days <- sapply(time_split, "[[", 1)
  times <- sapply(time_split, "[[", 2)
  frame <- as.data.frame(cbind(days, times), stringsAsFactors = F)
  final_times <- chron(frame$days, frame$times, format = c(dates = "m/d/y", times = "h:m:s"))
  return(final_times)
}

start_time <- date_time(td$Start.Date)
start_frame <- data.frame(start_time, td$Start.Terminal, rep(0, times = length(td$Start.Date)))
colnames(start_frame) <- c("Date", "Terminal", "Start")

end_time <- date_time(td$End.Date)
end_frame <- data.frame(end_time, td$End.Terminal, rep(1, times = length(td$End.Date)))
colnames(end_frame) <- c("Date", "Terminal", "Start")

final_time <- rbind(start_frame,end_frame)

orders <- order(final_time$Date)
ord_time <- final_time[orders, ]
routes <- ord_time

#Resets counter to 30
bike_counter <- as.data.frame(cbind(rep(bikes,times=num_terminals), sort(unique(td$Start.Terminal))))
colnames(bike_counter) <- c("Num", "Terminal")

for(i in 1:nrow(routes)) {
  terminal <- routes[i, 2]
  dept <- routes[i, 3]
  if (dept == 0) {
    row_minus <- which(bike_counter$Terminal == terminal)
    bike_counter[row_minus, 1] = bike_counter[row_minus, 1] - 1
  } else {
    row_add <- which(bike_counter$Terminal == terminal)
    bike_counter[row_add, 1] = bike_counter[row_add, 1] + 1
  }
  if(sum(bike_counter$Num <= 0) >= 1) {
    print("Rebalancing Required")
    print(routes[i, 1])
    break
  }
}

################################################################################################################

