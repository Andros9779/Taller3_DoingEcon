setwd("/Users/jdds/Documents/Doing Econ/Taller 3")


tempdata <- read.csv("NorthenHemis.csv",
                     skip = 1, na.strings = "***")

#tempdata = subset(tempdata, "year" < 2025) 

tempdata$Jan <- ts(tempdata$Jan, 
                   start = c(1880), end = c(2025), frequency = 1)
tempdata$DJF <- ts(tempdata$DJF, 
                   start = c(1880), end = c(2025), frequency = 1) 
tempdata$MAM <- ts(tempdata$MAM, 
                   start = c(1880), end = c(2025), frequency = 1) 
tempdata$JJA <- ts(tempdata$JJA, 
                   start = c(1880), end = c(2025), frequency = 1) 
tempdata$SON <- ts(tempdata$SON, 
                   start = c(1880), end = c(2025), frequency = 1) 
tempdata$J.D <- ts(tempdata$J.D, 
                   start = c(1880), end = c(2025), frequency = 1)

# Set line width and colour
plot(tempdata$Jan, type = "l", col = "blue", lwd = 2,
     ylab = "Annual temperature anomalies", xlab = "Year")

# Add a title
title("Average temperature anomaly in January in the northern hemisphere (1880-2025)")

# Add a horizontal line (at y = 0)
abline(h = 0, col = "darkorange2", lwd = 2)

# Add a label to the horizontal line
text(2025, -0.1, 
     "  1951-1980 average") 


# Set line width and colour
plot(tempdata$DJF, type = "l", col = "blue", lwd = 2,
     ylab = "Annual temperature anomalies", xlab = "Year")

# Add a title
title("Average temperature anomaly in January in the northern hemisphere (1880-2025)")

# Add a horizontal line (at y = 0)
abline(h = 0, col = "darkorange2", lwd = 2)

# Add a label to the horizontal line
text(2000, -0.1, "1951-1980 average") 

