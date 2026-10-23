library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)
library(usethis)
library(janitor) # Janitor is a package that makes it easy to clean data in R. 
# It has functions for cleaning column names, removing empty rows and columns, and more.

# This is all the packages we need for now
#---------------------------------------------------------#
 # Queensland Fisheries - Shark Control Program (1996-2026)
dataset1<- read_csv("data/Number_caught.csv")
dataset1
dataset2  <- read_csv("data/Biological_information.csv")
dataset2
# This is the data we will be using for our analysis.
# The first dataset contains information about the number of fish caught
# The second dataset contains biological information about the fish.
#---------------------------------------------------------#
Cleaned <- dataset1 %>%
  clean_names()
# Names of the columns have been cleaned to make them easier to work with.
#---------------------------------------------------------#
Non_Target <- Cleaned %>%
   filter(species_group != "SHARK", gear %in% c("Net","Drum"),year < 2026 ) %>% # Filter the data to 
  # include only non-target species, gear types "Net" and "Drum", and years less than 2026
  mutate(
    number_caught = as.numeric(number_caught), # Convert number_caught to numeric 
    species_group = str_to_title(species_group), # Convert species_group to title case
    gear = recode(gear, "Drum" = "Drumline"),
    common_name = str_squish(str_remove(common_name,".\\*")), # Remove any text after the 
    # first period in common_name and remove extra spaces
  ) %>%
   select(year,area,gear,species_group,common_name,alive_deceased,number_caught) %>%
   group_by(year,area,gear,species_group,common_name,alive_deceased) 
# The Non_Target dataset contains information about the number 
# of non-target species caught in the shark control program.

#---------------------------------------------------------#
Deaths_total <- Non_Target %>%
  filter(alive_deceased == "Deceased") %>% # Filter the data to include only deceased animals
  mutate(
   Non_Shark_Groups = recode(species_group,
                        Mammal = "Marine Mammals",
                        Turtle = "Marine Turtles",
                        Other = "Rays, fish and other species") ) %>% # Create a new column called 
  #Non_Shark_Groups that  groups species into three categories
  group_by(Non_Shark_Groups,gear) %>%
  summarise(total_deaths = sum(number_caught, na.rm = TRUE), .groups = "drop") %>% # Summarise the data 
  # to get the total number of deaths for each Non_Shark_Group and gear type also 
  #removes any missing values in the number_caught column and drops the grouping structure after summarising
  pivot_wider(names_from = gear, values_from = total_deaths, values_fill = 0) # Pivot the data to create
# a wider format, with separate columns for each gear type 

plot1 <- ggplot(data = Deaths_total,
       aes(x = Non_Shark_Groups, y = Net, fill = Non_Shark_Groups)) + # Create a bar plot of the 
  # total number of deaths for each Non_Shark_Group and gear type
  geom_col() +
  geom_text(aes(label = Net), vjust = -0.5) + # Add text labels to the bars showing the 
  # total number of deaths for each Non_Shark_Group and gear type
  scale_fill_manual(values = c("#534AB7", "#1D9E75", "#BA7517")) + # Set the fill colors for the bars
  labs(
    title    = "Total non-target animal deaths in the Shark Control Program, 1996-2025",
    subtitle = "Deaths of animals other than sharks, by gear type and species group",
    x        = NULL,
    y        = "Number of Deceased Animals",
    fill     = NULL,
    caption  = "Source: Queensland Fisheries QFISH Shark Control Program data. Animals released alive are not counted."
  ) +
  theme_minimal(base_size = 12) + # Set the theme to minimal with a base font size of 12
  theme(
    legend.position      = "none",
    plot.title           = element_text(face = "bold", size = rel(1.3)), # Set the title to bold and
    # increase the font size
    plot.title.position  = "plot", # Set the title position to the top of the plot
    plot.subtitle        = element_text(colour = "grey35", margin = margin(b = 10)), # Set the subtitle color 
    # to grey and add a margin below it
    plot.caption         = element_text(colour = "grey50", size = rel(0.75), hjust = 0), # Set the caption color
    # to grey, decrease the font size, and left-align it
    panel.grid.minor     = element_blank(), # Remove the minor grid lines from the plot
    panel.grid.major.x   = element_blank() # Remove the major grid lines from the x-axis of the plot
  )
ggsave ("Figures/Total_Non_Target_Deaths.png",plot1, width = 9, height = 6,dpi = 400)
# the Deaths_yearly dataset contains information about the total 
# number of deaths for each species group and gear type from 1996 to 2025. 
# The data is grouped by species group and gear type,
# and the total number of deaths is calculated for each group. 
# The data is then pivoted to create a wider format, with separate columns 
# for each gear type.
#---------------------------------------------------------#
Deaths_1995_to_2025 <- Non_Target %>%
  filter(alive_deceased == "Deceased") %>% # Filter the data to include only deceased animals
  mutate(
    Non_Shark_Groups = recode(species_group, # Create a new column called Non_Shark_Groups 
                              # that groups species into three categories
                              Mammal = "Marine Mammals",
                              Turtle = "Marine Turtles",
                              Other = "Rays, fish and other species") ) %>%
  group_by(year,Non_Shark_Groups,gear) %>% # Group the data by year, Non_Shark_Groups, and gear type
  summarise(total_deaths = sum(number_caught, na.rm = TRUE), .groups = "drop") %>%
  complete( year =  1996:2025, Non_Shark_Groups, gear, fill = list(total_deaths = 0)) # this line of code creates a complete 
# dataset with all combinations of year, Non_Shark_Groups, and gear, 
# filling in any missing values with 0 for total_deaths.
# This ensures that the plot will show all years and groups,
# even if there were no deaths recorded for some combinations.

ggplot(data = Deaths_1995_to_2025,
       aes(x = year, y = total_deaths, color = Non_Shark_Groups)) +
  geom_line(linewidth = 1, lineend = "round") + # Add a line plot to show the trend of total 
  # deaths over time for each Non_Shark_Group
  geom_point(size = 1.8) + # Add points to the line plot to show the 
  # total deaths for each year and Non_Shark_Group
  facet_wrap(~ gear, labeller = as_labeller(c(Net = "Nets", Drumline = "Drumlines")), # Create separate plots for 
             # each gear type (Nets and Drumlines) with custom labels
             scales = "free_y") +
  scale_color_manual(values = c("#534AB7", "#1D9E75", "#BA7517")) + # Set the colors for each Non_Shark_Group
  scale_x_continuous(
    breaks = c(1996,2006,2016,2025), # Set the x-axis breaks to show only the years 
    # 1996, 2006, 2016, and 2025
    labels = c("1996","2006","2016","2025"),# Set the x-axis labels to show
    # only the years 1996, 2006, 2016, and 2025
  ) +
  labs(
    title    = "Non-target animal deaths in the Shark Control Program, 1996-2025",
    subtitle = "Deaths of animals other than sharks, by gear type and species group",
    x        = "Year",
    y        = "Number of Deceased Animals",
    color    = NULL,
    caption  = "Source: Queensland Fisheries QFISH Shark Control Program data. Animals released alive are not counted."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = "top",
    legend.justification = "left",
    plot.title           = element_text(face = "bold", size = rel(1.3)), # Set the title to bold 
    # and increase the font size
    plot.title.position  = "plot",
    plot.subtitle        = element_text(colour = "grey35", margin = margin(b = 10)), # Set the subtitle color 
    # to grey and add a margin below it
    plot.caption         = element_text(colour = "grey50", size = rel(0.75), hjust = 0),# Set the caption color to grey
    # and decrease the font size, and left-align it
    strip.text           = element_text(face = "bold", hjust = 0), # Set the facet labels to bold and left-align them
    panel.grid.minor     = element_blank(), # Remove the minor grid lines from the plot
    panel.grid.major.x   = element_blank(),# Remove the major grid lines from the x-axis of the plot
    panel.spacing        = unit(1.5, "lines") # Set the spacing between the facets to 1.5 lines
  )
# The above code creates a line plot showing the number of non-target animal deaths in the Shark Control Program from 1996 to 2025.
# The data is grouped by gear type (Nets and Drumlines) and species group (Marine Mammals, Marine Turtles, Rays, fish and other species).
# The plot includes a title, subtitle, axis labels, and a caption indicating the source of the data. 
# The theme is set to minimal with customizations for legend position, title formatting,
# and panel spacing.
#---------------------------------------------------------#
Percentage_death <- Cleaned %>%
  filter(alive_deceased == "Deceased",
         gear %in% c("Net","Drum"),year < 2026 ) %>% # Filter the data to include only deceased animals,
  # gear types "Net" and "Drum", and years less than 2026
  group_by(year,gear) %>%
  mutate(
    gear = recode(gear, "Drum" = "Drumline"), # Recode the gear column to change "Drum" to "Drumline"
    target = if_else(species_group == "SHARK", "Target", "Non_Target"), # Create a new column called target 
    # that indicates whether the species is a target species (shark) or a non-target species
    total_death = sum(number_caught, na.rm = TRUE) # Calculate the total number of deaths for each 
    # year and gear type
  ) %>%
  group_by(year,gear,target,total_death) %>%
  summarise(deaths = sum(number_caught, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = target, values_from = deaths, values_fill = 0) %>% # Pivot the data to create a
  # wider format, with separate columns for Target and Non_Target deaths
  mutate( share = Non_Target/ (Target + Non_Target) * 100) # Calculate the share of deaths that
# were not sharks as a percentage of total deaths

Percentage_longer <- Percentage_death %>%
  pivot_longer(cols = c(Non_Target, share), names_to = "measure", values_to = "Value") %>% # Pivot the data 
  # to create a longer format, with separate rows for Non_Target 
  # deaths and share of deaths that were not sharks
  mutate(measure = recode(measure, # Recode the measure column to provide more 
                          # descriptive labels for the two measures
                          Non_Target = "Non-target animals that died per year",
                          share      = "Share of deaths that were not sharks (%)")) # Recode the measure column
# to provide more descriptive labels for the two measures

ggplot (data = Percentage_longer,
       aes(x = gear, y = Value, fill = gear)) +
  geom_boxplot(width =0.5, alpha = 0.75, outlier.shape = NA) + # Create a boxplot to show the 
  # distribution of Non_Target deaths and share of deaths that were not sharks for each gear type
  geom_jitter ( width = 0.12, alpha = 0.4) + # Add jittered points to the boxplot to show 
  # the individual data points for each gear type
  geom_text(data = Percentage_longer %>% # Add text labels to the plot to show the median values 
              # for each gear type and measure
              group_by(gear, measure) %>%
              summarise(med = median(Value), .groups = "drop") %>% # Calculate the median value for each gear 
              # type and measure
              mutate(label = if_else(str_detect(measure, "Share"),
                                     paste0("Median: ", round(med, 1), "%"), # Create a label for the median
                                     #value that includes a percentage sign if 
                                     # the measure is "Share of deaths that were not sharks (%)"
                                     paste0("Median: ", round(med, 1)))), # Create a label for the median value 
            #that does not include a percentage sign if the measure is "Non-target animals that died per year"
            aes(x = gear, y = med, label = label),
            inherit.aes = FALSE, # Do not inherit the aesthetics from the main ggplot call
            nudge_x = 0.33, hjust = 0, size = 3.5, colour = "grey20") + # Nudge the text labels to the right, 
  # left-align them, set the font size and color
  scale_x_discrete(labels = c(Net = "Nets", Drumline = "Drumlines")) + # Set the x-axis labels to show 
  # "Nets" and "Drumlines" instead of "Net" and "Drumline"
  scale_fill_manual(values = c(Net = "#534AB7", Drumline = "#1D9E75")) + # Set the fill colors for the boxplots 
  # to match the colors used in the previous plots 
  labs(
    fill = "Gear Type",
    title    = "Nets kill far more non target animals than Drumlines",
    subtitle = "Each point represents a year (1996-2025), Non-Target animals that died per year and the share of deaths that were not sharks (%)",
    x        = NULL,
    y        = NULL,
    caption  = "Source: Queensland Fisheries QFISH Shark Control Program data. Animals released alive are not counted."
  ) +
  facet_wrap(~ measure, scales = "free_y") + # Create separate plots for each measure
  # (Non-target animals that died per year and Share of deaths that were not sharks (%)) with free y-axis scales
  theme_minimal(base_size = 12)+
  theme(legend.position = "none")+
  theme(
    legend.position      = "top",
    legend.justification = "left",
    plot.title           = element_text(face = "bold", size = rel(1.3)), # Set the title to bold and 
    # increase the font size
    plot.title.position  = "plot", 
    plot.subtitle        = element_text(colour = "grey35", margin = margin(b = 10)), # Set the subtitle color
    # to grey and add a margin below it
    plot.caption         = element_text(colour = "grey50", size = rel(0.75), hjust = 0), # Set the caption color to 
    # grey, decrease the font size, and left-align it
    strip.text           = element_text(face = "bold", hjust = 0), # Set the facet labels to bold and left-align them
    panel.grid.minor     = element_blank(), # Remove the minor grid lines from the plot
    panel.grid.major.x   = element_blank(), # Remove the major grid lines from the x-axis of the plot
    panel.spacing        = unit(1.5, "lines") # Set the spacing between the facets to 1.5 lines
  )
# The above code creates a boxplot with jittered points to visualize 
# the number of non-target animal deaths and the share of deaths that 
# were not sharks for each gear type (Nets and Drumlines) from 1996 to 2025.
# The data is grouped by gear type and measure (Non-target animals that died per year
# and Share of deaths that were not sharks (%)).
#---------------------------------------------------------#
Years_netted <- Cleaned %>% # Create a new dataset called Years_netted that contains 
  # the number of years each area was netted
  filter(gear == "Net", year < 2026) %>% # Filter the data to include only netted areas 
  # and years less than 2026
  group_by(area) %>%
  summarise(years_netted = max(year) - min(year) + 1) # Calculate the number of years each
# area was netted by subtracting the minimum year from the maximum year and adding 1

Area_deaths <- Non_Target %>%
  filter(gear == "Net", alive_deceased == "Deceased") %>% # Filter the data to include only
  # netted areas and deceased animals
  group_by(area) %>%
  summarise(Total = sum(number_caught, na.rm = TRUE), .groups = "drop") # Calculate the total number of 
# deaths for each area by summing the number_caught column and removing any missing values

Area_table <- Area_deaths %>%
  left_join(Years_netted, by = "area") %>% # Join the Area_deaths dataset with the 
  # Years_netted dataset by area
  filter(years_netted >= 10) %>% # Filter the data to include only areas 
  # that were netted for 10 or more years 
  mutate(
         Deaths_per_year = round(Total / years_netted, 1)) %>% # Calculate the average number
  # of deaths per year for each area
  arrange(desc(Deaths_per_year)) # Arrange the data in descending order of Deaths_per_year

ggplot(
  data = Area_table,
  mapping = aes(x = Total, y = fct_reorder(area, Total), fill = area) # Create a horizontal bar plot 
  # to show the total number of deaths for each area,
) +
  geom_col() +
  geom_text(
    aes(label = paste0(Total, " (", Deaths_per_year, " per year)")), # Add text labels to the bars 
    # showing the total number of deaths and the average number of deaths per year for each area
    hjust = -0.05, size = 3.5 # Nudge the text labels to the right and set the font size
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) + # Expand the x-axis to add some space 
  # to the right of the bars for the text labels
  scale_fill_manual(values = c("#534AB7", "#1D9E75", "#BA7517", "#E69F00", "#F8766D",
                               "#000000")) + # Set the fill colors for the bars to match the 
  # colors used in the previous plots
  labs(
    title    = "Total non-target animal deaths in nets from 1996-2025",
    subtitle = "Deaths of animals other than sharks in shark nets, 
    by area (areas netted for 10 or more years)",
    x        = "Number of Deceased Animals",
    y        = NULL,
    fill     = NULL,
    caption  = "Source: Queensland Fisheries QFISH Shark Control Program data. 
    Animals released alive are not counted."
  ) +
  theme_minimal(base_size = 12) + # Set the theme to minimal with a base font size of 12
  theme(
    legend.position      = "none",
    plot.title           = element_text(face = "bold", size = rel(1.3)), # Set the title to bold and 
    # increase the font size
    plot.title.position  = "plot",
    plot.subtitle        = element_text(colour = "grey35", margin = margin(b = 10)), # Set the subtitle
    #color to grey and add a margin below it
    plot.caption         = element_text(colour = "grey50", size = rel(0.75), hjust = 0),# Set the caption color 
    # to grey, decrease the font size, and left-align it
    panel.grid.minor     = element_blank(), # Remove the minor grid lines from the plot
    panel.grid.major.y   = element_blank() # Remove the major grid lines from the y-axis of the plot
  )

#-------------------------------------------------------------------------------------------#
