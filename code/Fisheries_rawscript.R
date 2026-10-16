library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)
library(usethis)
library(janitor)
# This is all the packages we need for now 
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
   filter(species_group != "SHARK", gear %in% c("Net","Drum"),year < 2026 ) %>%
  mutate(
    number_caught = as.numeric(number_caught),
    species_group = str_to_title(species_group),
    gear = recode(gear, "Drum" = "Drumline"),
    common_name = str_squish(str_remove(common_name,".\\*")),
  ) %>%
   select(year,area,gear,species_group,common_name,alive_deceased,number_caught) %>%
   group_by(year,area,gear,species_group,common_name,alive_deceased) 
# The Non_Target dataset contains information about the number 
# of non-target species caught in the shark control program.
#---------------------------------------------------------#
Deaths_total <- Non_Target %>%
  filter(alive_deceased == "Deceased") %>%
  mutate(
   Non_Shark_Groups = recode(species_group,
                        Mammal = "Marine Mammals",
                        Turtle = "Marine Turtles",
                        Other = "Rays, fish and other species") ) %>%
  group_by(Non_Shark_Groups,gear) %>%
  summarise(total_deaths = sum(number_caught, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = gear, values_from = total_deaths, values_fill = 0) 

ggplot(data = Deaths_total,
       aes(x = Non_Shark_Groups, y = Net, fill = Non_Shark_Groups)) +
  geom_col() +
  geom_text(aes(label = Net), vjust = -0.5) +
  scale_fill_manual(values = c("#534AB7", "#1D9E75", "#BA7517")) +
  labs(
    title    = "Total non-target animal deaths in the Shark Control Program, 1996-2025",
    subtitle = "Deaths of animals other than sharks, by gear type and species group",
    x        = NULL,
    y        = "Number of Deceased Animals",
    fill     = NULL,
    caption  = "Source: Queensland Fisheries QFISH Shark Control Program data. Animals released alive are not counted."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = "none",
    plot.title           = element_text(face = "bold", size = rel(1.3)),
    plot.title.position  = "plot",
    plot.subtitle        = element_text(colour = "grey35", margin = margin(b = 10)),
    plot.caption         = element_text(colour = "grey50", size = rel(0.75), hjust = 0),
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank()
  )
# the Deaths_yearly dataset contains information about the total 
# number of deaths for each species group and gear type from 1996 to 2025. 
# The data is grouped by species group and gear type,
# and the total number of deaths is calculated for each group. 
# The data is then pivoted to create a wider format, with separate columns for each gear type.
#---------------------------------------------------------#
Deaths_1995_to_2025 <- Non_Target %>%
  filter(alive_deceased == "Deceased") %>%
  mutate(
    Non_Shark_Groups = recode(species_group,
                              Mammal = "Marine Mammals",
                              Turtle = "Marine Turtles",
                              Other = "Rays, fish and other species") ) %>%
  group_by(year,Non_Shark_Groups,gear) %>%
  summarise(total_deaths = sum(number_caught, na.rm = TRUE), .groups = "drop") %>%
  complete( year =  1996:2025, Non_Shark_Groups, gear, fill = list(total_deaths = 0))

ggplot(data = Deaths_1995_to_2025,
       aes(x = year, y = total_deaths, color = Non_Shark_Groups)) +
  geom_line(linewidth = 1, lineend = "round") +
  geom_point(size = 1.8) +
  facet_wrap(~ gear, labeller = as_labeller(c(Net = "Nets", Drumline = "Drumlines")),
             scales = "free_y") +
  scale_color_manual(values = c("#534AB7", "#1D9E75", "#BA7517")) +
  scale_x_continuous(
    breaks = c(1996,2006,2016,2025),
    labels = c("1996","2006","2016","2025"),
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
    plot.title           = element_text(face = "bold", size = rel(1.3)),
    plot.title.position  = "plot",
    plot.subtitle        = element_text(colour = "grey35", margin = margin(b = 10)),
    plot.caption         = element_text(colour = "grey50", size = rel(0.75), hjust = 0),
    strip.text           = element_text(face = "bold", hjust = 0),
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank(),
    panel.spacing        = unit(1.5, "lines")
  )
# The above code creates a line plot showing the number of non-target animal deaths in the Shark Control Program from 1996 to 2025.
# The data is grouped by gear type (Nets and Drumlines) and species group (Marine Mammals, Marine Turtles, Rays, fish and other species).
# The plot includes a title, subtitle, axis labels, and a caption indicating the source of the data. 
# The theme is set to minimal with customizations for legend position, title formatting,
# and panel spacing.
#---------------------------------------------------------#
Percentage_death <- Cleaned %>%
  filter(alive_deceased == "Deceased",
         gear %in% c("Net","Drum"),year < 2026 ) %>%
  group_by(year,gear) %>%
  mutate(
    gear = recode(gear, "Drum" = "Drumline"),
    target = if_else(species_group == "SHARK", "Target", "Non_Target"),
    total_death = sum(number_caught, na.rm = TRUE)
  ) %>%
  group_by(year,gear,target,total_death) %>%
  summarise(deaths = sum(number_caught, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = target, values_from = deaths, values_fill = 0) %>%
  mutate( share = Non_Target/ (Target + Non_Target) * 100) 

Percentage_longer <- Percentage_death %>%
  pivot_longer(cols = c(Non_Target, share), names_to = "measure", values_to = "Value") %>%
  mutate(measure = recode(measure,
                          Non_Target = "Non-target animals that died per year",
                          share      = "Share of deaths that were not sharks (%)"))

ggplot (data = Percentage_longer,
       aes(x = gear, y = Value, fill = gear)) +
  geom_boxplot(width =0.5, alpha = 0.75, outlier.shape = NA) +
  geom_jitter ( width = 0.12, alpha = 0.4) +
  geom_text(data = Percentage_longer %>%
              group_by(gear, measure) %>%
              summarise(med = median(Value), .groups = "drop") %>%
              mutate(label = if_else(str_detect(measure, "Share"),
                                     paste0("Median: ", round(med, 1), "%"),
                                     paste0("Median: ", round(med, 1)))),
            aes(x = gear, y = med, label = label),
            inherit.aes = FALSE,
            nudge_x = 0.33, hjust = 0, size = 3.5, colour = "grey20") +
  scale_x_discrete(labels = c(Net = "Nets", Drumline = "Drumlines")) +
  scale_fill_manual(values = c(Net = "#534AB7", Drumline = "#1D9E75")) +
  labs(
    title    = "Nets kill far more non target animals than Drumlines",
    subtitle = "Each point represents a year (1996-2025), Non-Target animals that died per year and the share of deaths that were not sharks (%)",
    x        = NULL,
    y        = NULL,
    caption  = "Source: Queensland Fisheries QFISH Shark Control Program data. Animals released alive are not counted."
  ) +
  facet_wrap(~ measure, scales = "free_y") +
  theme_minimal(base_size = 12)+
  theme(legend.position = "none")+
  theme(
    legend.position      = "top",
    legend.justification = "left",
    plot.title           = element_text(face = "bold", size = rel(1.3)),
    plot.title.position  = "plot",
    plot.subtitle        = element_text(colour = "grey35", margin = margin(b = 10)),
    plot.caption         = element_text(colour = "grey50", size = rel(0.75), hjust = 0),
    strip.text           = element_text(face = "bold", hjust = 0),
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank(),
    panel.spacing        = unit(1.5, "lines")
  )
# The above code creates a boxplot with jittered points to visualize 
# the number of non-target animal deaths and the share of deaths that 
# were not sharks for each gear type (Nets and Drumlines) from 1996 to 2025.
# The data is grouped by gear type and measure (Non-target animals that died per year
# and Share of deaths that were not sharks (%)).
#---------------------------------------------------------#