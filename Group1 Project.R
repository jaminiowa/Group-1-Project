

rm(list = ls())               # Clear workspace

library(readr)
library(ggplot2)
library(dplyr)
library(scales)
library(reshape2)

df1 <- read_csv("listings.csv") 
df2 <- read_csv("reviews.csv.gz")
df3 <- read_csv("calendar.csv.gz")


df4 <- read_csv("listings.csv.gz")
#df4$neighbourhood_group_cleansed
#qplot(longitude, latitude, data = df4, color = neighbourhood_group_cleansed, na.rm = TRUE)   #Look at the location of all BnB for all Islands

df <- read_csv("listings.csv")        # Read in Data  22725 rows with 16 variables
#print(sapply(df,class))               # Review the class of each variable

df$neighbourhood_group <- factor(df$neighbourhood_group)



# Overall Picture of all AirBnB
p <- qplot(longitude, latitude, data = df, color = neighbourhood_group, na.rm = TRUE) 
p <- p + scale_color_hue(name = "Island")
p <- p + ggtitle("Overview of AirBNB per Island")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + geom_text(aes(label=ifelse(latitude<(18.93),as.character(neighbourhood_group),'')),hjust=-.1,vjust=-14)
p <- p + geom_text(aes(label=ifelse(latitude>(21.7) & latitude<(21.8),as.character(neighbourhood_group),'')),hjust=-.5,vjust=1)
p <- p + geom_text(aes(label=ifelse(latitude>(21.8) & latitude<(21.87),as.character(neighbourhood_group),'')),hjust=-.5,vjust=1)
p <- p + geom_text(aes(label=ifelse(latitude>(21.09) & latitude<(21.1),as.character(neighbourhood_group),'')),hjust=-.5,vjust=-1)
p

# Remove records with even a single NA  17204 rows with 16 variables
df <- df[complete.cases(df), ]        

df <- mutate(df, current_year = as.integer(format(Sys.Date(), "%Y")))
df$current_year
df <- mutate(df, hosttime = as.integer(format(df4$host_since, "%Y")))
df$hosttime
df <- mutate(df, age = current_year - hosttime)
df$age
class(df$price)
class(df$age)
df$price
df$price <- factor(df$price)
df$price <- as.numeric(df$price)
class(df$price)
qplot(age, data = df, geom = "density", log = "x", fill = neighbourhood_group,
      alpha = I(0.5)) + scale_fill_hue(name = "Island")

qplot(age,data = df, geom = "bar")
qplot(age,data = df, geom = "bar", log = "y", facets = neighbourhood_group ~ .)
qplot(price,data = df, geom = "bar")
qplot(price, age, data = df, geom = "point", log = "y", facets = neighbourhood_group ~ .)
qplot(age,            # The "x" variable
      price,     # The "y" variable
      data = df,      # Our data frame
      geom = "jitter", # The geometry for a scatter plot
      log = "y")      # Log the sale price


# What island has the most AIRBNB
p <- qplot(neighbourhood_group, data = df, geom = "bar", xlab = "Island", ylab = "QTY of BnB",    # Basic bar chart
           fill = I("yellow"),                     # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy
p <- p + ggtitle("QTY of AirBnB per Island")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p

# Overall Average price for AirBNB
mean(df$price)                        #252.6865

# BoxPlots(which island has the most expensive): Facet by island and show avg price
p <- qplot(neighbourhood_group, price, data = df, geom = "boxplot", xlab = "Island", ylab = "Price", log = "y")
p <- p + ggtitle("Avg price per Island")
p

# Overlap(which island has the most expensive): Specify "Island" as a facet
qplot(price, data = df, geom = "density", log = "x", facets = . ~ neighbourhood_group)
# Separate and fill color by city, set 50% transparency
qplot(price, data = df, geom = "density", log = "x", fill = neighbourhood_group,
      alpha = I(0.5)) + scale_fill_hue(name = "Island")


df <- subset(df, price <= 150 & neighbourhood_group == "Hawaii")   # Subset for Hawaii
#mean(df$price, na.rm = TRUE)                                      # Average Price for Hawaii 155.7547



p <- qplot(longitude, latitude, data = df, color = price, na.rm = TRUE)
p <- p + scale_color_gradient( # Update the continuous color gradient
  name = "Price",         # Set the scale name
  low = "blue",               # Set color for low data values
  high = "red")             # Set color for high data values
p

df <- subset(df, price <= 150 & neighbourhood_group == "Hawaii" & longitude < -156)   # Subset for Hawaii
#mean(df$price, na.rm = TRUE)                                      # Average Price for Hawaii 155.7547



p <- qplot(longitude, latitude, data = df, color = price, na.rm = TRUE)
p <- p + scale_color_gradient( # Update the continuous color gradient
  name = "Price",         # Set the scale name
  low = "blue",               # Set color for low data values
  high = "red")             # Set color for high data values
p



pivot3 <- ungroup(df)  %>%
  select(name, room_type)

pivot3




