####################################################################################################################
# Clear workspace
####################################################################################################################
rm(list = ls())                     
####################################################################################################################

####################################################################################################################
#Load the Libraries
####################################################################################################################
# Load packages
suppressPackageStartupMessages(library(readr))                     
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(choroplethr))
suppressPackageStartupMessages(library(choroplethrMaps))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
####################################################################################################################

####################################################################################################################
#Plot all the Hawaiian Islands
####################################################################################################################
# Load population data by county
data(df_pop_county)

# Store the data in a shorter name
dpc <- df_pop_county


# Create, store, and view plot
p <- county_choropleth(dpc, state_zoom = "hawaii")
p


####################################################################################################################
# Initial Read
####################################################################################################################
df <- read_csv("listings.csv")
df4 <- read_csv("listings.csv.gz")

# Print memory usage, 4.6Mb
#print(format(object.size(df), units = "Mb"))

#Modify Columns
my_col_types = cols(
  id = col_integer(),
  name = col_character(),
  host_id = col_integer(),
  host_name = col_character(),
  neighbourhood_group = col_character(),
  neighbourhood = col_character(),
  latitude = col_double(),
  longitude = col_double(),
  room_type = col_character(),
  price = col_integer(),
  minimum_nights = col_integer(),
  number_of_reviews = col_integer(),
  last_review = col_date(format = ""),
  reviews_per_month = col_double(),
  calculated_host_listings_count = col_integer(),
  availability_365 = col_integer()
)

#Final Read
df <- read_csv("listings.csv", col_types = my_col_types)

####################################################################################################################
#Factor Islands into four distinct names
####################################################################################################################
df$neighbourhood_group <- factor(df$neighbourhood_group)        
####################################################################################################################

####################################################################################################################
#Read in a new column into listings.csv for the Review Scores which are ratings
####################################################################################################################
df$reviewscores <- df4$review_scores_rating
temp <- na.omit(df)
temp <- subset(df, reviewscores >= 97)
p < - qplot(reviewscores, data=temp, geom="bar", facets = "neighbourhood_group", xlab = "Review Scores")
####################################################################################################################

####################################################################################################################
#Add a Column into listings.csv for the Age of Ownership
####################################################################################################################
df <- mutate(df, current_year = as.integer(format(Sys.Date(), "%Y")))
df <- mutate(df, hosttime = as.integer(format(df4$host_since, "%Y")))
df <- mutate(df, age = current_year - hosttime)
####################################################################################################################

####################################################################################################################
#Factor the Price and turn it into a number
####################################################################################################################
df$price <- factor(df$price)
df$price <- as.numeric(df$price)
####################################################################################################################

####################################################################################################################
#Add a column for host who are identified as SuperHost
####################################################################################################################
df <- mutate(df, great_host = df4$host_is_superhost)            #Great Hosts per Island
#df <- mutate(df, great_host = factor(as.integer(df4$host_is_superhost == "t")))
df$great_host
class(df$great_host)
df$great_host <- factor(df$great_host)
levels(df$great_host)
levels(df$great_host) <- c("Not a Great Host", "Great Host")

print(levels(df$great_host))
####################################################################################################################

####################################################################################################################
#Add a column for east and west side of island for Hawaii
####################################################################################################################
df <- mutate(df, sideofisland = ifelse(longitude<(-155.5),'East','West'))
df$sideofisland
####################################################################################################################

####################################################################################################################
# Remove records with even a single NA
####################################################################################################################
df <- df[complete.cases(df), ]        
####################################################################################################################

####################################################################################################################
# Create a Table showing AirBnB per Island
####################################################################################################################
table(df$neighbourhood_group)

####################################################################################################################
# Picture of all Islands: Overall Picture of all AirBnB
####################################################################################################################
p <- qplot(longitude, latitude, data = df, color = neighbourhood_group, na.rm = TRUE, xlab="Longitude", ylab="Latitude") 
p <- p + scale_color_hue(name = "Island")
p <- p + ggtitle("Overview of AirBNB per Island")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + geom_text(aes(label=ifelse(latitude<(18.99),as.character(neighbourhood_group),'')),hjust=2,vjust=-7, color = "blue", size= 8)
p <- p + geom_text(aes(label=ifelse(latitude>(21.7) & latitude<(21.8),as.character(neighbourhood_group),'')),hjust=-.5,vjust=1, color = "blue", size = 8)
p <- p + geom_text(aes(label=ifelse(latitude>(21.86) & latitude<(21.87),as.character(neighbourhood_group),'')),hjust=-.2,vjust=1, color = "blue", size = 8)
p <- p + geom_text(aes(label=ifelse(latitude>(21.13) & latitude<(21.14),as.character(neighbourhood_group),'')),hjust=-.5,vjust=4, color = "blue", size = 8)
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p
###################################################################################################################

###################################################################################################################
# Bar Chart: What island has the most AIRBNB
###################################################################################################################
p <- qplot(neighbourhood_group, data = df, geom = "bar", xlab = "Island", ylab = "QTY of BnB",
           fill = I("yellow"),                     # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy
p <- p + ggtitle("QTY of AirBnB per Island")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p
###################################################################################################################

###################################################################################################################
#Bar Chart : Years of Ownership per Island
###################################################################################################################
p <- qplot(age, data = df, geom = "bar", xlab = "Years of Ownership", ylab = "QTY of BnB",
           fill = I("yellow"),                     # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy
p <- p + ggtitle("Overall AVG.Years of Ownership = 3.78 years")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p
mean(df$age)
###################################################################################################################

###################################################################################################################
#Price distribution
###################################################################################################################
p <- qplot(price,data = df, geom = "bar")
p <- p + ggtitle("Price Distribution: AVG =$218")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + scale_x_continuous( # Update x axis
  name = "Price Distribution",     # Update name, i.e., axis title
  labels = dollar)         # Label axis as dollars. Needs library(scales)
p
mean(df$price)
###################################################################################################################

####################################################################################################################
# Create a Table showing Great Hosts per Island
####################################################################################################################
table(df$great_host)


###################################################################################################################
#Plot Great Hosts
###################################################################################################################
#df$great_host

p <- qplot(great_host, data = df, geom = "bar")
p <- p + ggtitle("Total Great Hosts (6975 / 22725)")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + scale_x_discrete( # Update x axis
  name = "Great Host")     # Update name, i.e., axis title
p

#p <- qplot(longitude, latitude, data = df, color = neighbourhood_group, na.rm = TRUE, xlab="Longitude", ylab="Latitude") 
#p <- p + ggtitle("Where are the great hosts per island 6975 / 22725")
#p 

p <- qplot(great_host, data = df, geom = "bar", facets = . ~ neighbourhood_group,
           fill = I("yellow"),                     # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy    
p <- p + ggtitle("Distribution of great Hosts per island")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + scale_x_discrete( # Update x axis
  name = "")     # Update name, i.e., axis title
p <- p + theme(axis.text.x = element_text(angle = 90))
p

#qplot(price, data = df, geom = "density", log = "x", facets = neighbourhood_group ~ great_host)
####################################################################################################################







####################################################################################################################
# Filter for just Hawaii and great Host
####################################################################################################################
df <- subset(df, neighbourhood_group == "Hawaii" & great_host == "t")
p <- qplot(longitude, latitude, data = df, color = neighbourhood_group, na.rm = TRUE, xlab="Longitude", ylab="Latitude") 
p <- p + scale_color_hue(name = "Island")
p <- p + ggtitle("Overview of AirBNB for Hawaii and Great Host")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + geom_text(aes(label=ifelse(latitude<(19.01),as.character(neighbourhood_group),'')),hjust=-.1,vjust=-7, color = "blue", size= 8)
p <- p + geom_text(aes(label=ifelse(latitude>(21.7) & latitude<(21.8),as.character(neighbourhood_group),'')),hjust=-.5,vjust=1, color = "blue", size = 8)
p <- p + geom_text(aes(label=ifelse(latitude>(21.86) & latitude<(21.87),as.character(neighbourhood_group),'')),hjust=-.2,vjust=1, color = "blue", size = 8)
p <- p + geom_text(aes(label=ifelse(latitude>(21.13) & latitude<(21.14),as.character(neighbourhood_group),'')),hjust=-.5,vjust=4, color = "blue", size = 8)
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p
###################################################################################################################

###################################################################################################################
#Bar Chart : Years of Ownership per Island
###################################################################################################################
p <- qplot(age, data = df, geom = "bar", xlab = "Years of Ownership", ylab = "QTY of BnB",
           fill = I("yellow"),                     # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy
p <- p + ggtitle("Years of Ownership for Hawaii = 4.13 years")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
      axis.text.x = element_text(colour="grey20",size=20,face="bold"),
      axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
      axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p
mean(df$age)
###################################################################################################################

###################################################################################################################
#Price distribution
###################################################################################################################
p <- qplot(price,data = df, geom = "bar")
p <- p + ggtitle("Price Distribution for Hawaii: AVG =$170")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + scale_x_continuous( # Update x axis
  name = "Price Distribution",     # Update name, i.e., axis title
  labels = dollar)         # Label axis as dollars. Needs library(scales)
p
mean(df$price)
###################################################################################################################



###################################################################################################################
# BoxPlots: Overall Average price for AirBNB
# BoxPlots(which island has the most expensive): Facet by island and show avg price
###################################################################################################################
mean(df$price)                        #210

p <- qplot(neighbourhood_group, price, data = df, geom = "boxplot", xlab = "Island", ylab = "Price", log = "y")
p <- p + ggtitle("Avg price per Island")
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p
###################################################################################################################

###################################################################################################################
# Overlap(which island has the most expensive): Specify "Island" as a facet
###################################################################################################################
qplot(price, data = df, geom = "density", log = "x", facets = . ~ neighbourhood_group)
# Separate and fill color by city, set 50% transparency
p <- qplot(price, data = df, geom = "density", log = "x", fill = neighbourhood_group,
      alpha = I(0.5)) + scale_fill_hue(name = "Island")
p <- p + ggtitle("Avg price per Island")
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p
###################################################################################################################
#Room Type
###################################################################################################################
df$room_type <- factor(df$room_type)
qplot(room_type, data=df, geom = "bar")
###################################################################################################################  

###################################################################################################################
#Plot Great Hosts
###################################################################################################################


p <- qplot(great_host, data = df, geom = "bar")
p <- p + ggtitle("How many great Hosts are there 6975 / 22725")
p

p <- qplot(longitude, latitude, data = df, color = neighbourhood_group, na.rm = TRUE, xlab="Longitude", ylab="Latitude") 
p <- p + ggtitle("Where are the great hosts per island 6975 / 22725")
p 

p <- qplot(great_host, data = df, geom = "bar", facets = . ~ neighbourhood_group)
p <- p + ggtitle("How many great Hosts are there per island 6975 / 22725")
p

qplot(price, data = df, geom = "density", log = "x", facets = neighbourhood_group ~ great_host)
####################################################################################################################

####################################################################################################################
df <- filter(df, great_host == "t" & price <50, age<=5)
p <- qplot(longitude, latitude, data = df, color = neighbourhood_group, na.rm = TRUE, xlab="Longitude", ylab="Latitude") 
p <- p + ggtitle("Where are the great hosts per island 268 / 22725")
p <- p + geom_text(aes(label=ifelse(latitude>(18) & latitude<(19.047),as.character(neighbourhood_group),'')),hjust=-.1,vjust=-7, color = "blue", size= 8)
p <- p + geom_text(aes(label=ifelse(latitude>(21.65) & latitude<(22),as.character(neighbourhood_group),'')),hjust=-.5,vjust=1, color = "blue", size = 8)
p <- p + geom_text(aes(label=ifelse(latitude>(22),as.character(neighbourhood_group),'')),hjust=-.2,vjust=1, color = "blue", size = 8)
p <- p + geom_text(aes(label=ifelse(latitude>(20.9) & latitude<(21),as.character(neighbourhood_group),'')),hjust=-.3,vjust=1, color = "blue", size = 8)
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + scale_color_hue(name = "Island")
p

pivot3 <- ungroup(df)  %>%
  select(name, room_type, price, last_review, minimum_nights,age) %>%
  arrange(-price,minimum_nights, age)

pivot3
###########################################################################################################################

######################################################################################################################
p <- qplot(longitude, latitude, data = df, color = price, na.rm = TRUE)
p <- p + scale_color_gradient( # Update the continuous color gradient
  name = "Price",         # Set the scale name
  low = "blue",               # Set color for low data values
  high = "red")             # Set color for high data values
p
######################################################################################################################




