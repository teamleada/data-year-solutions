setwd("/Users/Brian_Liou/Documents/Startup/Projects/Data_Year_Problems/Week_3/")

# Data Schema http://stat-computing.org/dataexpo/2009/the-data.html
data <- read.csv("2008.csv", nrows= 1000000, header = T, stringsAsFactors = F)

install.packages('data.table')
library('data.table')


##########################
### PROBLEM 1 SOLUTION ###
##########################

match <- unique(data$Dest) %in% unique(data$Origin)
unique(data$Dest)[!match]


##########################
### PROBLEM 2 SOLUTION ###
##########################

delay_data <- data.table(data$ArrDelay, data$DepDelay, data$Origin, data$Dest)
colnames(delay_data) <- c("ArrDelay", "DepDelay", "Origin", "Dest")

airports <- data.frame(unique(data$Dest), rep(0, times = length(unique(data$Dest))), rep(0, times = length(unique(data$Dest))))
colnames(airports) <- c("Airports", "OriginDelays", "DestDelays")

system.time(
  for(i in 1:nrow(delay_data)) {
    if(is.na(delay_data$ArrDelay[i])) {
      
    } else {
      if(delay_data$ArrDelay[i] > 0) {
        origin <- delay_data$Origin[i]
        index <- which(airports$Airports == origin)
        airports$OriginDelays[index] <- airports$OriginDelays[index] + 1
      }
    }
    if(is.na(delay_data$DepDelay[i])) {
      
    } else {
      if(delay_data$DepDelay[i] > 0) {
        dest <- delay_data$Dest[i]
        index_two <- which(airports$Airports == dest)
        airports$DestDelays[index_two] <- airports$DestDelays[index_two] + 1
      }
    }
  }
)

#user  system elapsed 
#180.948   1.398 185.510 

# Total number of flights per airport
airports$Total <- NA
for(i in 1:length(airports$Airports)) {
  term <- airports$Airports[i]
  x <- nrow(data[which(data$Origin == term ), ])
  y <- nrow(data[which(data$Dest == term ), ])
  airports$Total[i] <- x + y
}

airports$Prob <- (airports$OriginDelays + airports$DestDelays) / airports$Total
sorted <- airports[order(airports$Prob), ]
sorted[which(sorted$Total > 10000), ]


##########################
### PROBLEM 3 SOLUTION ###
##########################

one <- data[which(data$DayOfWeek <= 5 & data$DepTime < 1700 & data$DepTime > 501 & data$ArrDelay > 0 & data$DepDelay > 0), ]
one_total <- data[which(data$DayOfWeek <= 5 & data$DepTime < 1700 & data$DepTime > 501), ]
dt_weekday <- table(one$UniqueCarrier)/ table(one_total$UniqueCarrier)

two <- data[which(data$DayOfWeek <= 5 & data$DepTime < 2400 & data$DepTime > 1701 & data$ArrDelay > 0 & data$DepDelay > 0), ]
two_total <- data[which(data$DayOfWeek <= 5 & data$DepTime < 2400 & data$DepTime > 1701), ]
nt_weekday <- table(two$UniqueCarrier)/ table(two_total$UniqueCarrier)

three <- data[which(data$DayOfWeek <= 5 & data$DepTime < 500 & data$DepTime > 0 & data$ArrDelay > 0 & data$DepDelay > 0), ]
three_total <- data[which(data$DayOfWeek <= 5 & data$DepTime < 500 & data$DepTime > 0), ]
re_weekday <- table(three$UniqueCarrier)/ table(three_total$UniqueCarrier)

four <- data[which(data$DayOfWeek > 5 & data$DepTime < 1700 & data$DepTime > 501 & data$ArrDelay > 0 & data$DepDelay > 0), ]
four_total <- data[which(data$DayOfWeek > 5 & data$DepTime < 1700 & data$DepTime > 501), ]
dt_weekend <- table(four$UniqueCarrier)/ table(four_total$UniqueCarrier)

five <- data[which(data$DayOfWeek > 5 & data$DepTime < 2400 & data$DepTime > 1701 & data$ArrDelay > 0 & data$DepDelay > 0), ]
five_total <- data[which(data$DayOfWeek > 5 & data$DepTime < 2400 & data$DepTime > 1701), ]
nt_weekend <- table(five$UniqueCarrier)/ table(five_total$UniqueCarrier)

six <- data[which(data$DayOfWeek > 5 & data$DepTime < 500 & data$DepTime > 0 & data$ArrDelay > 0 & data$DepDelay > 0), ]
six_total <- data[which(data$DayOfWeek > 5 & data$DepTime < 500 & data$DepTime > 0), ]
re_weekend <- table(six$UniqueCarrier)/ table(six_total$UniqueCarrier)

combo <- data.frame(rbind(dt_weekday, nt_weekday, re_weekday, dt_weekend, nt_weekend, re_weekend))

avg <- apply(combo, 2, mean)

final_frame <- rbind(combo, avg)
row.names(final_frame) <- c("DayTime_Weekday", "NightTime_Weekday", "RedEye_Weekday", "DayTime_Weekend", "NightTime_Weekend", "RedEye_Weekend", "Carrier_Avg")

write.csv(final_frame, "Problem_3.csv")