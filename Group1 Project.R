####################################################################################################################
# Clear workspace
####################################################################################################################
rm(list = ls())                     
####################################################################################################################

####################################################################################################################
#Load the Libraries
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

####################################################################################################################
# Read in Data
####################################################################################################################
df <- read_csv("listings.csv")
df4 <- read_csv("listings.csv.gz")

####################################################################################################################
# Read Islands into new column called Island and rename Honolulu to Oahu
####################################################################################################################
df$Island <- factor(df$neighbourhood_group)                                                 #Factor Islands into four distinct names
levels(df$Island)[levels(df$Island) == "Honolulu"] <- "Oahu"                                #Rename Honolulu to Oahu

####################################################################################################################
# Establish SuperHost
####################################################################################################################
df <- mutate(df, super_host = df4$host_is_superhost)                                        #Great Hosts per Island
df$super_host <- factor(df$super_host)                                                      #Factor great host
levels(df$super_host) <- c("Host", "Superhost")                                            #Change name of great hosts

####################################################################################################################
# Read in Price
####################################################################################################################
df$price <- factor(df$price)                                                                #Factor the Price
df$price <- as.numeric(df$price)                                                            #Turn it into a number

####################################################################################################################
# Establish how long someone has been an Airbnb owner
####################################################################################################################
df <- mutate(df, current_year = as.integer(format(Sys.Date(), "%Y")))
df <- mutate(df, hosttime = as.integer(format(df4$host_since, "%Y")))
df <- mutate(df, age = current_year - hosttime)

####################################################################################################################
# Read in Review Scores
####################################################################################################################
df$reviewscores <- df4$review_scores_rating

####################################################################################################################
# Read in Bedrooms
####################################################################################################################
df$bedrooms <- df4$bedrooms

####################################################################################################################
# Read in Bathrooms
####################################################################################################################
df$bathrooms <- df4$bathrooms

####################################################################################################################
# Cut Price Range into BIN's and Relabel BIN's
####################################################################################################################
df$Price_Range <- cut(df$price, 50*(0:16))
df$Price_Range <- factor(df$Price_Range)
# Rename the bins for user-friendliness
levels(df$Price_Range) <- c("$0-$50", "$50-$100", "$100-$150",
                                  "$150-$200", "$200-$250", "$250-$300", "$300-$350", "$350-$400",
                                  "$400-$450", "$450-$500", "$500-$550", "$550-$600", "$600-$650",
                                  "$650-$700", "$700-$750", "$750-$800")

####################################################################################################################
# Configure Side of Island to East and West
####################################################################################################################
df <- mutate(df, sideofisland = ifelse(longitude<(-155.5),'East','West'))

####################################################################################################################
# Read in Amenities
####################################################################################################################
df$amenities <- df4$amenities

####################################################################################################################
# Read in BeachFront Property's and Re-label levels
####################################################################################################################
df$beachfront <- grepl("beachfront", tolower(df$amenities), fixed = TRUE)
df$beachfront <- factor(df$beachfront)
levels(df$beachfront) <- c("Not BeachFront", "BeachFront")

####################################################################################################################
# Read in Property Type
####################################################################################################################
df$property_type <- as.character(df4$property_type)

####################################################################################################################
# Remove records with even a single NA
####################################################################################################################
df <- df[complete.cases(df), ]                                                              
####################################################################################################################


df <- mutate(df, magnitude = ifelse(super_host == "Superhost" & beachfront == "BeachFront" & Island == "Hawaii" & price <200 & price >99 & bedrooms >0,10000,0))

temp <- subset(df, super_host == "Superhost" & beachfront == "BeachFront" & Island == "Hawaii" & price <200 & price >99 & bedrooms >0)
tempp <- subset(df, Island == "Hawaii")


####################################################################################################################
# Function for TOP 5 Amenities by superhost
####################################################################################################################
top5amenitiesrequestedbysuperhost <- function(x,y) {
  awordsbest <- awords[x:y,]
  awordsbest
}

awords <- subset(df, super_host == "Superhost")
mylist <- strsplit(awords$amenities, "," , fixed = TRUE)
awords <- unlist((mylist))
awords <- sort(table(awords))
awords <- as.data.frame(awords)
funy <- nrow(awords)



####################################################################################################################
# Function for TOP 5 Amenities by Non superhost
####################################################################################################################
top5amenitiesrequestedbynonsuperhost <- function(x,y) {
  aawordsbest <- aawords[x:y,]
  aawordsbest
}

aawords <- subset(df, super_host == "Host")
mylist <- strsplit(aawords$amenities, "," , fixed = TRUE)
aawords <- unlist((mylist))
aawords <- sort(table(aawords))
aawords <- as.data.frame(aawords)
funny <- nrow(aawords)










####################################################################################################################
#Slide 1 - Choropleth Plot of all the Hawaiian Islands
####################################################################################################################
data(df_pop_county)                                                                         #Load population data by county
dpc <- df_pop_county                                                                        #Store the data in a shorter name
p <- county_choropleth(dpc, state_zoom = "hawaii")                                          #Create, store, and view plot
p <- p + ggtitle("Hawaiian Islands")
p <- p + theme(plot.title = element_text(colour="grey20",size=28,face="bold")) + theme(plot.title = element_text(hjust = 0.5))
p <- p + theme(legend.position = "none")
p
####################################################################################################################

####################################################################################################################
#Slide 4 - Picture of all 4 Islands: Overall Picture of all AirBnB
####################################################################################################################
table(df$Island)   # Create a Table showing AirBnB per Island

p <- qplot(longitude, latitude, data = df, color = Island, na.rm = TRUE, xlab="Longitude(degrees)", ylab="Latitude(degrees)") 
p <- p + scale_color_hue(name = "Island")
p <- p + ggtitle("Overview of Airbnb per Island")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + geom_text(aes(label=ifelse(latitude<(18.99),as.character(Island),'')),hjust=2,vjust=-7, color = "blue", size= 10)
p <- p + geom_text(aes(label=ifelse(latitude>(21.7) & latitude<(21.8),as.character(Island),'')),hjust=-.5,vjust=1, color = "blue", size = 10)
p <- p + geom_text(aes(label=ifelse(latitude>(21.86) & latitude<(21.87),as.character(Island),'')),hjust=-.2,vjust=1, color = "blue", size = 10)
p <- p + geom_text(aes(label=ifelse(latitude>(21.13) & latitude<(21.14),as.character(Island),'')),hjust=-.5,vjust=4, color = "blue", size = 10)
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=28,face="bold"),
               axis.text.x = element_text(colour="grey20",size=28,face="bold"),
               axis.text.y = element_text(colour="grey20",size=28,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=28,face="bold"))
p <- p + theme(legend.title = element_blank())
p <- p + theme(legend.position = "none")
p
####################################################################################################################

####################################################################################################################
#Slide 5 - Begins: Bar Chart: What island has the most AIRBNB
####################################################################################################################
p <- qplot(Island, data = df, geom = "bar", xlab = "", ylab = "QTY of Airbnb",
           fill = I("yellow"),                        # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy
p <- p + ggtitle("QTY of Airbnb per Island")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=28,face="bold"))
p <- p + theme(axis.text = element_text(colour="grey20",size=28,face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=28,face="bold"),
               axis.text.x = element_text(colour="grey20",size=28,face="bold"),
               axis.text.y = element_text(colour="grey20",size=28,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=28,face="bold"))
p
###################################################################################################################

###################################################################################################################
#Slide 8 - Begins: Bar Chart: showing Overall Super Hosts
###################################################################################################################
table(df$super_host)

p <- qplot(super_host, data = df, geom = "bar", xlab = "", ylab = "QTY of Superhosts",
           fill = I("yellow"),                        # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy
p <- p + ggtitle("Superhosts Summary for all Islands")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=28,face="bold"))
p <- p + theme(axis.text = element_text(colour="grey20",size=28,face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=28,face="bold"),
               axis.text.x = element_text(colour="grey20",size=28,face="bold"),
               axis.text.y = element_text(colour="grey20",size=28,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=28,face="bold"))
p
###################################################################################################################

###################################################################################################################
#Slide 9 - Begins: Bar Chart showing Super Hosts per Island
###################################################################################################################
df <- group_by(df, Island, super_host)

# Summarize by count/frequency
summ <- summarize(df, num_houses = n())

# Sale price in the rows, city in the columns, and count in the values
pivot1 <- dcast(summ, Island ~ super_host, value.var = "num_houses")    #  switch num_houses with arr
pivot1

p <- qplot(super_host, data = df, geom = "bar", facets = . ~ Island, xlab = "", ylab = "Qty of Superhosts",
           fill = I("yellow"),                     # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy    
p <- p + ggtitle("Distribution of Superhosts per Island")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey28",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey28",size=20,face="bold"),
               axis.text.x = element_text(colour="grey28",size=20,face="bold"),
               axis.text.y = element_text(colour="grey28",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey28",size=20,face="bold"))
p <- p + theme(axis.text.x = element_text(angle = 90))
p

df <- ungroup(df)
###################################################################################################################

###################################################################################################################
#Slide 10 - How many superhosts are in each PRICE bin
###################################################################################################################
df <- group_by(df, Price_Range, super_host)

# Summarize by count/frequency
summ <- summarize(df, num_houses = n())

# Sale price in the rows, city in the columns, and count in the values
pivot2 <- dcast(summ, Price_Range ~ super_host, value.var = "num_houses")
pivot2

df <- ungroup(df)
###################################################################################################################

###################################################################################################################
#Slide 11 - Price Bin Superhost and ratings
###################################################################################################################
df <- group_by(df, Price_Range, super_host)

# Summarize by count/frequency
summ <- summarize(df, num_houses = n(), arr = round(mean(reviewscores), digits=2))

# Sale price in the rows, city in the columns, and count in the values
pivot3 <- dcast(summ, Price_Range ~ super_host, value.var = "arr")
pivot3

df <- ungroup(df)
###################################################################################################################

###################################################################################################################
#Slide 12 - Price Bin Superhost and Average Price
###################################################################################################################
df <- group_by(df, Price_Range, super_host)

# Summarize by count/frequency
summ <- summarize(df, num_houses = n(), arr = round(mean(price), digits=2))

# Sale price in the rows, city in the columns, and count in the values
pivot4 <- dcast(summ, Price_Range ~ super_host, value.var = "arr")
pivot4

df <- ungroup(df)
###################################################################################################################

###################################################################################################################
#Slide 14 - Is Beachfront more expensive
###################################################################################################################
df <- subset(df, super_host == "Superhost")
df <- group_by(df, Price_Range, beachfront)

# Summarize by count/frequency
summ <- summarize(df, num_houses = n(), arr = round(mean(price), digits=2))


# Sale price in the rows, city in the columns, and count in the values
pivot5 <- dcast(summ, Price_Range ~ beachfront, value.var = "arr")    #  switch num_houses with arr
pivot5

df <- ungroup(df)
###################################################################################################################

###################################################################################################################
#Slide 15 - Is Superhost / Beachfront more expensive per island
###################################################################################################################
bfppis <- subset(df, super_host == "Superhost" & beachfront == "BeachFront")
bfppis <- group_by(bfppis, Price_Range, Island)

# Summarize by count/frequency
summ <- summarize(bfppis, num_houses = n(), arr = mean(price))            #price/reviewscores/bedrooms/bath/age

# Sale price in the rows, city in the columns, and count in the values
pivot6 <- dcast(summ, Price_Range ~ Island, value.var = "arr")    #  switch num_houses with arr
pivot6

df <- ungroup(bfppis)
###################################################################################################################


####################################################################################################################
#Slide 18/19 - Begins : # Picture of top5 in Hawaii
####################################################################################################################

p <- qplot(longitude, latitude, data = temp,shape = super_host, size=magnitude,color = super_host,na.rm = TRUE, xlab="Longitude", ylab="Latitude") 
p <- p + scale_color_hue(name = "Island")
p <- p + ggtitle("Location of Top Airbnb in Hawaii")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p

p <- qplot(longitude, latitude, data = tempp,color = super_host,na.rm = TRUE, xlab="Longitude", ylab="Latitude") 
p <- p + scale_color_hue(name = "Island")
p <- p + ggtitle("Location of Top Airbnb in Hawaii")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p
####################################################################################################################


###################################################################################################################
#Slide 20
###################################################################################################################
p <- ggplot(temp, aes(x = reorder(property_type, price, FUN = median), y = price)) + geom_boxplot() + xlab("") +
  ylab("Price")
p <- p + ggtitle("Distribution of Price for BeachFront/Superhosts/[$100:$150]")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))         # Label axis as dollars. Needs library(scales)
p <- p + theme(axis.text.x = element_text(angle = 90))
p <- p + scale_y_continuous(labels = dollar)
p
###################################################################################################################

###################################################################################################################

###################################################################################################################
#Slide 21
###################################################################################################################
mean(temp$price)
mean(temp$age)

p <- qplot(price,age, data = temp, geom = "point", xlab="Price", ylab="Years of being Airbnb Host", size = 20,
           fill = I("yellow"),                     # Add a light blue fill
           color = I("red"),                          # Make color of rectangles red
           alpha = I(0.5))                            # Set 50% transparancy    
p <- p + ggtitle("Distribution of Price vs Years of being Airbnb Host")
p <- p + theme(panel.border = element_rect(linetype = "solid", color = "black", fill = NA))
p <- p + theme(plot.title = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text = element_text(face="bold"))
p <- p + theme(axis.title.y = element_text(colour="grey20",size=20,face="bold"),
               axis.text.x = element_text(colour="grey20",size=20,face="bold"),
               axis.text.y = element_text(colour="grey20",size=20,face="bold"),  
               axis.title.x = element_text(colour="grey20",size=20,face="bold"))
p <- p + theme(axis.text.x = element_text(angle = 90))
p <- p + scale_x_continuous(labels = dollar)         # Label axis as dollars. Needs library(scales)
p <- p + theme(legend.title = element_blank())
p <- p + theme(legend.position = "none")
p

###################################################################################################################
#Slide 23 - Begins: Top 5 in Hawaii
###################################################################################################################
top5 <- select(temp, name, price, property_type,bedrooms,sideofisland) %>%
  group_by(property_type) %>%
  top_n(-5, price) %>%
  arrange(property_type,desc(price))
head(top5, n=20)
###################################################################################################################


###################################################################################################################
#Slide 24: Top 5 Amenities requested by Superhost Function
###################################################################################################################
top5amenitiesrequestedbysuperhost(funy-4,funy)


###################################################################################################################
#Slide 24: Top 5 Amenities requested by Non-Superhost Function
###################################################################################################################
top5amenitiesrequestedbynonsuperhost(funny-4,funny)


