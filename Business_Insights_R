#' Author: Fernando Gamboa 
#' Title: A1 
#' Purpose: Load geospatial data and visualize it
#' Author: Ted Kwartler
#' License: GPL>=3
#' Date: Mar 20, 2023
#'


## Set the working directory
setwd("~/Desktop/R Studio/Hult_R/BAN1_Case_Info/A1_Household_Direct_Mail")

#packages 
install.packages("tidyverse")
install.packages("readr")
install.packages("tidyr")
install.packages("data.table")
install.packages("maps")
install.packages("RColorBrewer")

#Libraries required 
library(readr)
library(ggplot2)
library(maps)
library(ggthemes)
library(leaflet)
library(mapproj)
library(dplyr)
library(tidyr)
library(data.table)
library(RColorBrewer)

#Opening the required files 

consumer_data <- read_csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/consumerData_training15K_studentVersion.csv") #Right join
donations_data <- read_csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/DonationsData_training15K_studentVersion.csv")
household <- read_csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/inHouse_EDA_10k.csv") #Main DataFrame 
magazine_data <- read_csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/magazineData_training15K_studentVersion.csv")
political_data <- read_csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/politicalData_training15K_studentVersion.csv")

#Start of EDA Section 

#Validating Consumer_data 
#Validating that files can open and getting to explore the required columns 
#For consumer dataset first we identify the names of each column 
names(consumer_data)

#Peek into the first 5 rows to understand a little bit more about the data 
#It basically identifies per key: ethnicity, if home owner, and net worth among other characteristics
#We notice several NAs that need to be cleaned 
head(consumer_data)

#Validating Household Data - main database
#As the main database, all the customer information is here per name, last name, age, and location
names(household)

# Notice that First and Last Name don't have to be really split, but there are NAs as well on several columns. 
head(household)

#Household and Consumer data contain the main demographics necessary.

#We merge information with the columns required 
demographics <- left_join(household,consumer_data, by = "tmpID")

##Inspect one more time the new dataframe 
colnames(demographics)  #List of column names
nrow(demographics)  #How many rows are in data frame?
dim(demographics)  #Dimensions of the data frame?
head(demographics)  #See the first 6 rows of data frame
str(demographics)  #See list of columns and data types (numeric, character, etc)
summary(demographics)  #Statistical summary of data

#Validate if there are any missing values 
colSums(is.na(demographics))

#Identify on Gender where is the NA value 
which(is.na(demographics$Gender))

#There is a need to change into boolean Gender so we can count 
demographics$male <- ifelse(demographics$Gender == "M", 1, 0)

colnames(demographics)

#group by the gender to understand the ratio and fill the NA detected
gender_agg <- demographics %>% 
  group_by(Gender) %>% 
  summarise(total_count = n(), .groups = 'drop' )

gender_agg

# We transform the aggregation into a separate df to visualize better 
gender_df <- gender_agg %>% as.data.frame()
gender_df


gender_bar <- gender_df$total_count                      # Extract values
names(gender_bar) <- gender_df$Gender               # Assign names to values
gender_bar                                # Show new data


#We make a barplot to visualize each gender
# Male 42% and Female 58%, therefore it is likely that the missing value is F 
barplot(gender_bar,
        main = "Gender Ratio",
        xlab = "Gender",
        ylab = "Count",
        col = "darkred",
        horiz = FALSE)

#With this information in mind, replace NA in Gender and validate 
demographics$Gender <- ifelse(is.na(demographics$Gender), "F", demographics$Gender)
demographics$male <- ifelse(is.na(demographics$male), 0, demographics$male)
colSums(is.na(demographics))

#There is a need to convert Est Home Value into numeric and fill NAs 
#We also clean Age as it has NA values 
demographics$EstHomeValue <- as.numeric(gsub("\\$", "", demographics$EstHomeValue))
demographics <- demographics %>% 
  mutate(Age = replace_na(Age, mean(Age, na.rm = TRUE))) %>%
  mutate(EstHomeValue = replace_na(EstHomeValue, mean(EstHomeValue, na.rm =TRUE)))

#Age must is in decimals, so it will work better if rounded

demographics$Age <- round(demographics$Age,0)

#Let's find out for Ethnic Description what is the mode 
#English/Welsh is the most common ethnicity 
ethnic <- demographics %>% 
  group_by(EthnicDescription) %>%
  summarise(total_count = n())

ethnic[order(-ethnic$total_count),]

#Replace NA with English/Welsh

demographics$EthnicDescription <- ifelse(is.na(demographics$EthnicDescription), "English/Welsh", demographics$EthnicDescription)

#Homeowner or Renter may be used to refer for mor insights 
#An estimate of 77% of the customers own a house, therefore we replace NA with Likely Homeowner as the probability is high 

owners <- demographics %>% 
  group_by(HomeOwnerRenter) %>%
  summarise(Count = n(),.groups = 'drop' )
owners

#Replace values 

demographics$HomeOwnerRenter <- ifelse(is.na(demographics$HomeOwnerRenter), "Likely Homeowner", demographics$HomeOwnerRenter)


#Validate  one more time that the information has been cleaned correctly 
colSums(is.na(demographics))

#Donations Data 
#The data has Nulls in almost every column exceeding 50% of rows, most of this file's data cannot be employed 
colSums(is.na(donations_data))

#Magazine Data 
#The data has Nulls in almost every column exceeding 50% of rows, most of this file's data cannot be employed
#There is a special cleaning step in insight #4 for required information regarding hobbies/interests 
colSums(is.na(magazine_data))

#Political data 
#The data has Nulls in almost every column exceeding 50% of rows, only Parties Description is employed in insights and has 0 NAs 
colSums(is.na(political_data))


#Insight 1 - Successful Women Wired for Successs 

#Based on Expedian,focus on single women who may be classified as K37
#K37 = Wired for success. 
#First, create new dataframe based on demographics
k37_female <- subset(demographics, select=c("tmpID", "FirstName", "LastName", "Gender","Age","lat","lon","county","city","state","fips", "NetWorth","EstHomeValue"))

#Now,filter only for Females in order to find more insights
k37_female <- filter(k37_female, Gender == "F")
head(k37_female) 
#Review one more time any data that needs cleaning. In this case Age and EstHomeValue 
colSums(is.na(k37_female))

#Now, there is in interest in Liberal women as clients, add this information and clean EstHomeValue
k37_female <- left_join(k37_female,political_data[,c("tmpID","PartiesDescription")], by = "tmpID")

#It is time to select more filters for age,estimated home value as net worth and Lifestyle 
  k37_female <- k37_female %>% 
    filter(Age %inrange%  c(23, 30) ) %>%
    filter(NetWorth == "$50000-99999") %>% 
    filter(PartiesDescription == "Democratic")

#Review states where most of the young, wealthy and liberal women live.
#Notice that most IDs indicate that the top states are MI, CA, AR, CO, IL
unique(k37_female$state)

liberal_states <- aggregate(tmpID ~ state, data = k37_female, FUN = length)
liberal_states <- liberal_states[order(-liberal_states$tmpID),]
liberal_states

# Subset data to selected states
liberal_ENC <- k37_female[k37_female$state %in% c("Michigan","California","Arizona", "Colorado", "Illinois"),]

# A basic map library
map()
dev.off()
map('usa')	# national boundaries
dev.off()
map("state", interior = FALSE)
dev.off()
map("state", interior = T)
dev.off()
map('county', interior = T) # reminder clear graphics device
dev.off()
map('state', region = c("Michigan","California","Arizona", "Colorado", "Illinois"))
points(liberal_ENC$lon,liberal_ENC$lat, col='blue')
dev.off()

# Examine the map data
head(map_data('state'))
us <- fortify(map_data('state'), region = 'region')
head(us)

# More familiar ggplot interface
gg <- ggplot() + 
  geom_map(data  =  us, 
           map = us,
           aes(x = long, y = lat, map_id = region, group = group, fill = liberal_ENC), 
           fill = 'white', color = 'black', linewidth = 0.25) + 
  scale_fill_manual(values = c("liberal_ENC" = "blue"),
                    name = "Political Affiliation",
                    labels = c("Liberal_ENC")) +
  coord_map('albers', lat0 = 39, lat1 = 45) +
  theme_map()
gg

# Add points layer to point out States where most liberal women live
#Notice that Michigan hast the most population, we should focus on this state
gg +
  geom_point(data  = liberal_ENC, 
             aes(x = lon, y=lat), 
             color = 'blue', alpha=0.5) 

# County and single state for Michigan 
counties <- map_data("county")
MIcounty <- subset(counties, region == "Michigan")
head(MIcounty)
onlyMI   <- subset(liberal_ENC,liberal_ENC$state=='Michigan')

# State and county outlines for Michigan
ggMI <- ggplot() + 
  geom_map(data  =  MIcounty, map = MIcounty,
           aes(x = long, y = lat, 
               map_id = region, group = group), 
           fill = 'white', color = 'blue', linewidth = 0.25) + 
  coord_map('albers', lat0 = 39, lat1 = 45) +
  theme_map()

# Examine
ggMI

# Add points layer
ggMI +
  geom_point(data = onlyMI, 
             aes(x = lon, y = lat), color = 'blue', alpha=0.5) 

# Leaflet layers using %>% pipe
mplot<- leaflet(data=onlyMI) %>%
  addTiles() %>%
  addMarkers( popup = paste("Loc:", onlyMI$Location, "<br>",
                            "SqFt:", onlyMI$Sq..Feet,"<br>",
                            "Type:", onlyMI$Type),
              clusterOptions = markerClusterOptions()) 
mplot

#For Michigan, what is the Average Home Value for K_37 persona? 

net_female <- k37_female %>% 
  filter(state == "Michigan") %>%
  group_by(county) %>% 
  summarize(mean_home_value = mean(EstHomeValue))
net_female

#Visual representation of average home value per net worth in women 
#Washtenaw County and Wexford show that the Wired for Success women in the most expensive real estates are located here 
ggplot(net_female, aes(x = county, y = mean_home_value, fill = county)) +
  geom_col() +
  labs(x = "County", y = "Avg Home Value", title = "Avg Home Value by County per Wired for Success Women  (Michigan)") +
  theme_bw() + 
  scale_y_continuous(labels = scales::comma) 


#Insight 2 Pet Owners (Cats and Dogs) 

#Create the filter for each type of pet
pets_df <- demographics %>% 
  filter(CatOwner == "Yes"| DogOwner == "Yes") %>%
  as.data.frame()

#We sort Age column 
pets_df <- pets_df[order(pets_df$Age),] 

#Create bins for ages in order to identify each group 
pets_df <- pets_df %>%
  mutate(age_range = findInterval(Age, c(20, 30, 40, 50, 60, 70, 80, 90, 100)))

#Group by Age
pets_hist <- pets_df %>% group_by(age_range) %>% 
  summarise(avg_spending = mean(y_householdSpend),.groups = "drop") %>%
  as.data.frame()
  
#Time to create a visualization to see any possible correlations between Age and Spending
# scatter plot
ggplot(pets_hist, aes(x = age_range, y = avg_spending)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Age intervals and Avg Houshold Spending",
       x = "Age Intervals", y = "Avg Spending")

#Let's see in what type of property they live, so we can cater to this clients
property_type <- pets_df %>% 
  group_by(PropertyType) %>% 
  summarise(count = n())
property_type


#Creating visualization barplot shows the most common property type
ggplot(property_type, aes(x = PropertyType, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Property Type", y = "Count", 
       title = "Pet Owners Property Type")

#Let's see in how many people own a house, so we can cater to this clients as well
home_owner <- pets_df %>% 
  group_by(HomeOwnerRenter) %>% 
  summarise(count = n())
home_owner

#Creating visualization barplot shows that most of the customers that own or rent
ggplot(home_owner, aes(x = HomeOwnerRenter, y = count)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x = "Home Owner or Renter", y = "Count", 
       title = "Pet Owners Home Owners vs Renters")

# Insight #3 Average household expenses for young couples (25-35) with children and ethnic group  
young_couples <- demographics %>% 
  filter(Age %inrange%  c(25, 35) ) %>%
  filter(PresenceOfChildrenCode %in% c("Likely to have a child", "Modeled Likely to have a child"))

#Summarize the age groups and average spending per group 
yc_table <- young_couples %>% 
  group_by(Age) %>% 
  summarise( Avg_HouseSpend = mean(y_householdSpend),
          Avg_EstHomeValue = mean(EstHomeValue))
yc_table

#Average Monthly spending per family 
mean(yc_table$Avg_HouseSpend)

#Average Est. Home Value  spending per family 
mean(yc_table$Avg_EstHomeValue)

#Create a visualization of the average spending and home value per age 
#Average Household Spending for Young Couples with Children
ggplot(yc_table, aes(x=Age, y=Avg_HouseSpend)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Average Household Spending by Young Adult Age",
       x="Age",
       y="Average Household Spending")

#Average Estimated Home Value for Young Couples with Children
ggplot(yc_table, aes(x=Age, y=Avg_EstHomeValue)) +
  geom_bar(stat="identity", fill="skyblue") +
  labs(title="Average Estimated Home Value by Young Adult Age",
       x="Age",
       y="Average Estimated Home Value")

#Group by Ethnic group and count them to identify which group is having the most families 
ethnic_families <- young_couples %>% 
  group_by(EthnicDescription) %>%
  summarise(total_count = n())
ethnic_families

#Order and visualize the table 
#We find out that the most common Ethnicitiy is English/Welsh, so we will replace NA with them 
ethnic_families <- ethnic_families[order(-ethnic_families$total_count),]
ethnic_families

#Ethnic Welsh families represent 48.71% of the total ethnic distribution 
ethnic_families[1,2]/sum(ethnic_families$total_count)

#Visualization for the top 5 families with highest Ethnicity 
top5 <- ethnic_families %>% 
  top_n(5, total_count) # select the top 5 rows by total count

ggplot(top5, aes(x = reorder(EthnicDescription, total_count), y = total_count)) +
  geom_bar(stat = "identity", fill = "tan1") +
  labs(x = "Ethnic Description", y = "Total Count", 
       title = "Top 5 Ethnic Descriptions in Young Couples with Children") +
  theme_bw()

#Filter the top #1 ethnicity to understand spending habits and home value 
English_Welsh <- young_couples %>% 
  filter(EthnicDescription == "English/Welsh")
English_Welsh

#Do value calculation 
EW_summary <- English_Welsh %>% 
  group_by(Age) %>% 
  summarise( Avg_HouseSpend = mean(y_householdSpend),
             Avg_EstHomeValue = mean(EstHomeValue))
EW_summary


#Mean English Welsh Avg Household Expending 
mean(EW_summary$Avg_HouseSpend)

#Mean English Welsh Avg Est. Home Value 
mean(EW_summary$Avg_EstHomeValue)

#Create Average Household Spending Visualization 
ggplot(EW_summary, aes(x=Age, y=Avg_HouseSpend)) +
  geom_bar(stat="identity", fill="tomato") +
  labs(title="English/Welsh Average Household Spending by Young Adult Age",
       x="Age",
       y="Average Household Spending")

#Create Estimated Home Value Visualization 
ggplot(EW_summary, aes(x=Age, y=Avg_EstHomeValue)) +
  geom_bar(stat="identity", fill="coral") +
  scale_y_continuous(labels=dollar_format()) +  # Format y-axis labels as currency
  labs(title="English/Welsh Average Estimated House Value by Young Adult Age",
       x="Age",
       y="Average Estimated Home Value")

#Highest House Espending for 32 years old Welsh Family 
EW_summary[8,]

#Insight 4
#Networth by occupation to classify the best paying job and most common net worth

#NetWorth has some NAs so wthere is a need to filter it out 
demographics <- demographics %>% drop_na(NetWorth)

#Net Worth Income vs Occupation 
Worthy_df <- aggregate(NetWorth ~ OccupationIndustry,data = demographics, FUN = length, na.action = na.omit)
Worthy_df <- Worthy_df[order(-Worthy_df$NetWorth), ]
Worthy_df

#Index the rows to filter out Uknown and Other as it only creates noise 
Worthy_df <- Worthy_df[3:19,]

# Create the stacked bar chart
ggplot(Worthy_df, aes(x = reorder(OccupationIndustry, NetWorth), y = NetWorth)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Occupations") +
  ylab("Count of Net Worth") +
  ggtitle("Net Worth count by Occupation Industry") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

#Notice that Management is the top occupation for possible customers, let's see the distribution of Net Worth 
Management_df <- demographics %>% 
  filter(OccupationIndustry == "Management") %>%
  group_by(NetWorth) %>%
  summarise(OccupationIndustry = n())

#Within the Management Occupation 
Management_df <- Management_df[order(-Management_df$OccupationIndustry), ]
Management_df 

#Visualization 
ggplot(Management_df, aes(x = reorder(NetWorth, OccupationIndustry), y = OccupationIndustry)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  xlab("Net Worth ranges") +
  ylab("Count of  Management Jobs") +
  ggtitle("Management jobs count by Net Worth Range") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()
 
#Managers Hobbies, it is convenient for the loyalty program to know what these customers enjoy

#filter only to management occupation
Management_hobbies <- demographics %>% 
  filter(OccupationIndustry == "Management")

#selected hobbies from magazines
selected_magazines <- subset(magazine_data, 
                            select=c("tmpID","GardeningMagazineInHome", "CulinaryInterestMagazineInHome", "HealthFitnessMagazineInHome", "DoItYourselfMagazineInHome"))

#drop null data 
selected_magazines <- selected_magazines %>% drop_na()

#join infortmation into a single table 
Management_hobbies <- left_join(Management_hobbies, selected_magazines ,by = "tmpID")


#Replace NA values with "0" and convert everything to numeric
Management_hobbies$GardeningMagazineInHome <- as.numeric(gsub("[^0-9.]", "", Management_hobbies$GardeningMagazineInHome))
Management_hobbies$GardeningMagazineInHome[is.na(Management_hobbies$GardeningMagazineInHome)] <- 0

Management_hobbies$CulinaryInterestMagazineInHome <- as.numeric(gsub("[^0-9.]", "", Management_hobbies$CulinaryInterestMagazineInHome))
Management_hobbies$CulinaryInterestMagazineInHome[is.na(Management_hobbies$CulinaryInterestMagazineInHome)] <- 0

Management_hobbies$HealthFitnessMagazineInHome <- as.numeric(gsub("[^0-9.]", "", Management_hobbies$HealthFitnessMagazineInHome))
Management_hobbies$HealthFitnessMagazineInHome[is.na(Management_hobbies$HealthFitnessMagazineInHome)] <- 0

Management_hobbies$DoItYourselfMagazineInHome <- as.numeric(gsub("[^0-9.]", "", Management_hobbies$DoItYourselfMagazineInHome))
Management_hobbies$DoItYourselfMagazineInHome[is.na(Management_hobbies$DoItYourselfMagazineInHome)] <- 0


#Summarise the hobbies that people who work in management have 
Management_p2 <- Management_hobbies %>% 
  group_by(OccupationIndustry)  %>%
  summarise(
        Gardening = sum(GardeningMagazineInHome),
        Culinary = sum(CulinaryInterestMagazineInHome),
        Health = sum(HealthFitnessMagazineInHome),
        DIY = sum(DoItYourselfMagazineInHome)
      )
Management_p2

#Create a new dataframe to visualize a stacked bar
Hobbies <- factor(c("Gardening","Culinary", "Health","DIY"))

Count <- c(14, 10, 39, 26)

# Combine the two columns into a dataframe
stacked_bar <- data.frame(Hobbies, Count)
stacked_bar

# Define a custom color palette
my_colors <- c("deepskyblue", "skyblue", "blue", "darkblue")

# Create the stacked bar chart with custom colors
ggplot(stacked_bar, aes(x = "", y = Count, fill = Hobbies)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = my_colors) +
  coord_flip() + 
  theme_bw() +
  labs(title = "Hobbies that Management Prefer", x = NULL, y = "Count") +
  guides(fill = guide_legend(reverse = TRUE))


