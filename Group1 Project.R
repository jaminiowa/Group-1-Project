####################################################################################################################
# Clear workspace
####################################################################################################################
rm(list = ls())                     
####################################################################################################################






####################################################################################################################
#Load the Libraries - Begin
####################################################################################################################
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
#Load the Libraries - End
####################################################################################################################






####################################################################################################################
# Initial Read - Begins
####################################################################################################################
df <- read_csv("listings.csv")
df4 <- read_csv("listings.csv.gz")

df$neighbourhood_group <- factor(df$neighbourhood_group)                                    #Factor Islands into four distinct names
levels(df$neighbourhood_group)[levels(df$neighbourhood_group) == "Honolulu"] <- "Oahu"        #Rename Hawaii to Oahu

df <- mutate(df, super_host = df4$host_is_superhost)                                        #Great Hosts per Island
#df <- mutate(df, great_host = factor(as.integer(df4$host_is_superhost == "t")))
#df$great_host
#class(df$great_host)
df$super_host <- factor(df$super_host)                                                      #Factor great host
#levels(df$great_host)
levels(df$super_host) <- c("Not a Super Host", "Super Host")                                #Change name of great hosts

#print(levels(df$great_host))

df$price <- factor(df$price)                                                                #Factor the Price
df$price <- as.numeric(df$price)                                                            #Turn it into a number

df <- mutate(df, current_year = as.integer(format(Sys.Date(), "%Y")))
df <- mutate(df, hosttime = as.integer(format(df4$host_since, "%Y")))
df <- mutate(df, age = current_year - hosttime)

df$reviewscores <- df4$review_scores_rating

df$bedrooms <- df4$bedrooms

df$bathrooms <- df4$bathrooms

df$price_binned <- cut(df$price, 50*(0:16))

df <- mutate(df, sideofisland = ifelse(longitude<(-155.5),'East','West'))

df$amenities <- df4$amenities

df <- df[complete.cases(df), ]                                                              #Remove records with even a single NA
####################################################################################################################
# Initial Read - Done
####################################################################################################################




####################################################################################################################
#Slide 3 - Begins : Choropleth Plot all the Hawaiian Islands
####################################################################################################################
data(df_pop_county)                                                                         #Load population data by county
dpc <- df_pop_county                                                                        #Store the data in a shorter name
p <- county_choropleth(dpc, state_zoom = "hawaii")                                          #Create, store, and view plot
p
####################################################################################################################
#Slide 3 - Ends : Choropleth Plot all the Hawaiian Islands
####################################################################################################################






####################################################################################################################
#Slide 5 - Begins : # Picture of all Islands: Overall Picture of all AirBnB
####################################################################################################################
table(df$neighbourhood_group)   # Create a Table showing AirBnB per Island

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
####################################################################################################################
#Slide 5 - Ends: Picture of all Islands: Overall Picture of all AirBnB
####################################################################################################################






####################################################################################################################
#Slide 6 - Begins: Bar Chart: What island has the most AIRBNB
####################################################################################################################
p <- qplot(neighbourhood_group, data = df, geom = "bar", xlab = "Island", ylab = "QTY of BnB",
           fill = I("yellow"),                        # Add a light blue fill
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
#Slide 6 - Ends: Picture of all Islands: Overall Picture of all AirBnB
###################################################################################################################






###################################################################################################################
#Slide 9 - Begins: #Create a Table showing Overall Super Hosts
###################################################################################################################
table(df$super_host)

p <- qplot(super_host, data = df, geom = "bar", xlab = "Super Host", ylab = "QTY of Super Hosts",
           fill = I("yellow"),                        # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy
p <- p + ggtitle("Super Hosts Summary across all Islands")
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
#Slide 9 - Ends: #Create a Table showing Overall Super Hosts
###################################################################################################################






###################################################################################################################
#Slide 10 - Begins: #Create a Table showing Super Hosts per Island
###################################################################################################################
p <- qplot(super_host, data = df, geom = "bar", facets = . ~ neighbourhood_group, xlab = "", ylab = "Qty of Super Hosts",
           fill = I("yellow"),                     # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy    
p <- p + ggtitle("Distribution of Super Hosts per Island")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text.x = element_text(angle = 90))
p
###################################################################################################################
#Slide 10 - Ends: #Create a Table showing Super Hosts per Island
###################################################################################################################





###################################################################################################################
#Slide 11 - Begins: Price: #Create a Table showing Price Distribution for Super Hosts
###################################################################################################################
p <- qplot(price, data = df, geom = "bar", facets = . ~ super_host, xlab = "Price", ylab = "Qty of Super Hosts",
           fill = I("yellow"),                     # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy    
p <- p + ggtitle("Distribution of Price")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text.x = element_text(angle = 90))
p <- p + scale_x_continuous(labels = dollar)         # Label axis as dollars. Needs library(scales)
p


p <- qplot(super_host, price, data = df, geom = "boxplot", log = "y",xlab = "", ylab = "Price")
p <- p + ggtitle("Distribution of Price for all Super Hosts")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + scale_y_continuous(labels = dollar,  limits = c(0, 500))         # Label axis as dollars. Needs library(scales)
p

###################################################################################################################
#Slide 11 - Ends: Price: #Create a Table showing Price Distribution for Super Hosts
###################################################################################################################

###################################################################################################################
#Slide 12 - Begins: AGE Distribution for Super Hosts PER ISLAND
###################################################################################################################

df$age <- factor(df$age)

qplot(age, data = df, geom = "bar", facets = neighbourhood_group ~ .)
qplot(age, data = df, geom = "bar", fill = neighbourhood_group)
qplot(neighbourhood_group, age, data = df, geom = "bin2d")

mean(df$age)

df$age <- as.numeric(df$age)
p <- qplot(neighbourhood_group, age, data = df, geom = "boxplot", log = "y",xlab = "", ylab = "Age")
p


p <- qplot(age, neighbourhood_group, data = df, geom = "bar", xlab = "", ylab = "Price")
p <- p + ggtitle("Distribution of Price for Super Hosts")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + scale_y_continuous(labels = dollar,  limits = c(0, 500))         # Label axis as dollars. Needs library(scales)
p

###################################################################################################################
#Slide 12 - Ends: AGE Distribution for Super Hosts PER ISLAND
###################################################################################################################

####################################################################################################################
# Slide 13
####################################################################################################################
####################################################################################################################
#Read in a new column into listings.csv for the Review Scores which are ratings
####################################################################################################################
df$reviewscores <- df4$review_scores_rating
temp <- na.omit(df)
temp <- subset(df, reviewscores >= 97)
qplot(reviewscores, data = df, geom = "density", log = "x", facets = . ~ super_host)
qplot(reviewscores, data = df, geom = "density", log = "x", fill = super_host,
      alpha = I(0.5))
qplot(reviewscores, data = df, geom = "histogram", binwidth = 0.1, log = "x",
      fill = super_host)
qplot(super_host, reviewscores, data = df, geom = "boxplot", log = "y")

mean(df$reviewscores)
####################################################################################################################




####################################################################################################################
# Group Study
####################################################################################################################
#pivot3 <-
#  group_by(df, super_host, price)                     %>%
#  summarize(num_houses = n(), ar = mean(reviewscores))                               %>%
#  dcast(price ~ super_host, value.var = "ar")

#pivot3

df <- subset(df, neighbourhood_group == "Hawaii" & beach == FALSE)

df$price_binned <- cut(df$price, 50*(0:16))
print(levels(df$price_binned))

df <- group_by(df, price_binned, super_host)

# Summarize by count/frequency
summ <- summarize(df, num_houses = n(), arr = mean(reviewscores))            #price/reviewscores/bedrooms/bath/age

# Sale price in the rows, city in the columns, and count in the values
pivot1 <- dcast(summ, price_binned ~ super_host, value.var = "num_houses")    #  switch num_houses with arr
pivot1

# Sale price in the rows, city in the columns, and count in the values
pivot2 <- dcast(summ,price_binned ~ super_host, value.var = "arr")    #  switch num_houses with arr
pivot2

df$amenities
df$beach <- grepl("beachfront & internet & pool", tolower(df$amenities), fixed = TRUE)
table(df$beach)

ocean <- grepl("pool", tolower(df$amenities), fixed = TRUE)
table(ocean)

qplot(super_host, price, geom="boxplot", data=df)
###################################################################################################################
# So far we determined reviews per bin and the number of bnb's per price bin.
# now we would like to bring in bedrooms per price bin
#ultimate goal is to determine if if it better to rent a super host vs non


#df <- group_by(df, super_host, price_binned)


newdf <- subset(df, super_host == "Super Host")
df4$bed
newdf <- group_by(newdf, bedrooms, price_binned)

# Summarize by count/frequency
summ <- summarize(df, num_houses = n(), arr = mean(reviewscores))

# Sale price in the rows, city in the columns, and count in the values
pivot3 <- dcast(summ, price_binned ~ bedrooms, value.var = "num_houses")    #  switch num_houses with arr
pivot3

# Sale price in the rows, city in the columns, and count in the values
pivot4 <- dcast(summ, price_binned ~ bedrooms, value.var = "arr")    #  switch num_houses with arr
pivot4


df <- df[complete.cases(df), ]














####################################################################################################################
# Slide 15 - Filter for just Hawaii
####################################################################################################################
df <- subset(df, neighbourhood_group == "Hawaii" & super_host == "Super Host")

df <- subset(df, neighbourhood_group == "Hawaii")
table(df$super_host)

p <- qplot(longitude, latitude, data = df, color = neighbourhood_group, na.rm = TRUE, xlab="Longitude", ylab="Latitude") 
p <- p + scale_color_hue(name = "Island")
p <- p + ggtitle("Overview of AirBNB for Hawaii and Super Host")
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

p <- qplot(super_host, data = df, geom = "bar", facets = . ~ neighbourhood_group, xlab = "", ylab = "Qty of Super Hosts",
           fill = I("yellow"),                     # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy    
p <- p + ggtitle("Distribution of Super Hosts for Hawaii")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text.x = element_text(angle = 90))
p
####################################################################################################################
# Slide 15 - Filter for just Oahu
####################################################################################################################



###################################################################################################################
#Slide 16 - Begins: Price: #Create a Table showing Price Distribution for Super Hosts and Hawaii
###################################################################################################################
#df <- subset(df, super_host == "Super Host")
p <- qplot(price, data = df, geom = "bar", facets = neighbourhood_group ~ super_host, xlab = "Price", ylab = "Qty of Super Hosts",
           fill = I("yellow"),                     # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy    
p <- p + ggtitle("Distribution of Price")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text.x = element_text(angle = 90))
p <- p + scale_x_continuous(labels = dollar)         # Label axis as dollars. Needs library(scales)
p


p <- qplot(neighbourhood_group, price, data = df, geom = "boxplot", log = "y",xlab = "", ylab = "Price")
p <- p + ggtitle("Distribution of Price for Super Hosts")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + scale_y_continuous(labels = dollar,  limits = c(0, 500))         # Label axis as dollars. Needs library(scales)
p

mean(df$price)
###################################################################################################################
#Slide 16 - Ends: Price: #Create a Table showing Price Distribution for Super Hosts
###################################################################################################################

#age
qplot(age, data = df, geom = "density", log = "x", facets = . ~ super_host)
qplot(age, data = df, geom = "density", log = "x", fill = super_host,
      alpha = I(0.5))
qplot(age, data = df, geom = "histogram", binwidth = 0.1, log = "x",
      fill = super_host)
qplot(super_host, age, data = df, geom = "boxplot", log = "y")

mean(df$age)
###################################################################################################################
#Slide 16 - Ends: #Create a Table showing Super Hosts per Island
###################################################################################################################











#room type
qplot(room_type, data = df, geom = "bar", facets = . ~ super_host)


























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
#property type in gz
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
p <- qplot(longitude, latitude, data = df, color = price, na.rm = TRUE, facets = . ~ super_host)
p <- p + scale_color_gradient( # Update the continuous color gradient
  name = "Price",         # Set the scale name
  low = "blue",               # Set color for low data values
  high = "red")             # Set color for high data values
p
######################################################################################################################




