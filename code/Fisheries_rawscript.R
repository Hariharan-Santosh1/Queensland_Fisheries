library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)
library(usethis)
# This is all the packages we need for now 

dataset1<- read_csv("data/Number_caught.csv")
dataset1
dataset2  <- read_csv("data/Biological_information.csv")
dataset2
# This is the data we will be using for our analysis.
# The first dataset contains information about the number of fish caught
# The second dataset contains biological information about the fish.

TigerShark_Queensland <- dataset1 %>%
  filter(CommonName == "TIGER SHARK") %>%
  select(Year,Month,NumberCaught,BeachName,'Alive/Deceased',Area) %>%
  group_by(Year,Month,BeachName,Area) 
# The code  filters the dataset1 to only include rows where the CommonName is "TIGER SHARK" and selects specific columns for analysis. It then groups the data by Year, Month, BeachName, and Area.
  

Overall_dataset1_Quantity_Townsville <- dataset1 %>%
  filter (Area == "Townsville") %>%
  select(Year,Month,CommonName,NumberCaught,BeachName,Area)
# This code filters the dataset1 to only include rows where the Area is "Townsville" and selects specific columns for analysis.

TigerShark_dataset1_Townsville <- Overall_dataset1_Quantity_Townsville %>%
  filter (CommonName == "TIGER SHARK")%>%
  select(Year,Month,CommonName,NumberCaught,BeachName,Area)
# This code filters the Overall_dataset1_Quantity_Townsville to only include rows where the CommonName is "TIGER SHARK" and selects specific columns for analysis.

TigerShark_2cities <- TigerShark_Queensland %>%
  filter(Area == "Townsville" | Area == "Cairns") %>%
  select(Year,Month,NumberCaught,BeachName,'Alive/Deceased',Area) 


Average_Queensland <- TigerShark_Queensland %>%
  rename(Alive_Deceased = 'Alive/Deceased') %>%
  group_by(Area,Year,Alive_Deceased) %>%
  summarise(AverageCaught = mean(NumberCaught), .groups = "drop")
ggplot(data = Average_Queensland, mapping = 
          aes(x = Area, y = AverageCaught, fill = Alive_Deceased)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Number of Tiger Sharks Caught in Queensland by Area and Status", x = "Area", y = "Average Number Caught") +
  theme_light() +
  theme(legend.position = "top",axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(data = TigerShark_Queensland, mapping = aes(x =Area,y = NumberCaught,fill = Area )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Tiger Sharks Caught in Queensland by Area", x = "Area", y = "Number Caught") +
  theme(legend.position = "none")

ggplot(data= TigerShark_Queensland, mapping = aes( x = Area, y = NumberCaught, color = Area))+
         geom_boxplot(fill = "steelblue") +
         labs(title = "Distribution of Tiger Sharks Caught in Queensland by Area", x = "Area", y = "Number Caught") +
         theme_minimal() +
         theme(legend.position = "none")





# 22
ggplot(data = TigerShark_dataset1_Townsville,mapping = aes(x =BeachName,y = NumberCaught,fill = BeachName )) +
  geom_bar(stat = "identity",fill = "steelblue" ) +
  labs(title = "Number of Tiger Sharks Caught in Townsville by Beach", x = "Beach Name", y = "Number Caught") +
  theme_minimal() +
  theme(legend.position = "none")
  
# This code creates a bar plot using ggplot2 to visualize the number of Tiger Sharks caught in Townsville by beach. The x-axis represents the month, the y-axis represents the number caught, and the bars are filled based on the beach name. The plot is faceted by beach name for better visualization. 

TigerShark_dataset1_Townsville %>%
  group_by(BeachName, Year) %>%
  summarise(AvgCaught = mean(NumberCaught), .groups = "drop") %>%
  ggplot(mapping = aes(x = Year, y = AvgCaught, colour = BeachName, group = BeachName)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Number of Tiger Sharks Caught per Year in Townsville by Beach",
       x = "Year", y = "Average Number Caught") +
  theme_minimal() +
  theme(legend.position = "none")  
 
  

Overall_dataset1_Quantity_Cairns <- dataset1 %>%
  filter (Area == "Cairns") %>%
  select(Year,Month,CommonName,NumberCaught,BeachName,Area)
  