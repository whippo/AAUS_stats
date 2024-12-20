#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# AAUS Stats Summary                                                          ##
# Data are current as of 2024-12-18                                           ##
# Data source: American Academy of Underwater Sciences                        ##
# R code prepared by Ross Whippo                                              ##  
# Last updated 2024-12-18                                                    ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# Exploratory summary statistics of AAUS stats submissions

# Required Files (check that script is loading latest version):
# ResultsGrid_ExportData.csv

# Associated Scripts:
# AAUS_Stats_Summary.qmd

# TO DO 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# MANIPULATE DATA                                                              +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                             ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library("GGally")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Stats2023 <- read_csv("data/ResultsGrid_ExportData.csv")
Stats2022 <- read_csv("data/ResultsGrid_ExportData (1).csv")
Stats2021 <- read_csv("data/ResultsGrid_ExportData (2).csv")
Stats2020 <- read_csv("data/ResultsGrid_ExportData (3).csv")
Stats2019 <- read_csv("data/ResultsGrid_ExportData (4).csv")

Stats5year <- Stats2019 %>%
  bind_rows(Stats2020,
            Stats2021,
            Stats2022,
            Stats2023)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

colnames(Stats5year)

# how many air dives per OM in last 5 years?
Stats5year %>%
  ggplot() +
  geom_line(aes(x = Year, y = log10(`# Air Dives` + 1), color = OM)) +
  theme_bw() +
  theme(legend.position="none") +
  geom_smooth(aes(x = Year, y = log10(`# Air Dives` + 1)),
              method = "loess", color = "black",
              se = FALSE, size = 5)

# how many dives total per OM in the last 5 years?
Stats5year %>%
  mutate(totalDives = rowSums(across(c(`# Air Dives`,
                          `# MIxed Gas Dives`,
                          `# Nitrox Dives`)))) %>%
  ggplot() +
  geom_line(aes(x = Year, y = totalDives, color = OM)) +
  theme_bw() +
  theme(legend.position="none") +
  geom_smooth(aes(x = Year, y = totalDives),
              method = "loess", color = "black",
              se = FALSE, size = 5)

# how many dives of each mode were there this year?
Stats2023 %>%
  pivot_longer(cols = c(`# Hookah Dives`,
                        `# Open Circuit Dives`,
                        `# Rebreather Dives`,
                        `# Surface Supplied Dives`),
               names_to = "Mode",
               values_to = "Dives") %>%
  ggplot() +
  geom_col(aes(x = Mode, y = Dives))

# how many minutes of each mode were there this year?
Stats2023 %>%
  pivot_longer(cols = c(`Hookah Dive Time (min)`,
                        `Open Circuit Dive Time (min)`,
                        `Rebreather Dive Time (min)`,
                        `Surface Supplied Dive Time (min)`),
               names_to = "Mode",
               values_to = "Time") %>%
  ggplot() +
  geom_col(aes(x = Mode, y = log10(Time)))

# pairs plot 5 years
Stats5year %>%
  group_by(Year = as.character(Year)) %>%
  summarise(across(c("Total Divers",
                        "# Air Dives",
                        "# Air Divers",
                        "Air Dive Time (min)",
                        "# Nitrox Dives",
                        "# Nitrox Divers",
                        "Nitrox Dive Time (min)",
                     "Total  Dive Incidents",
                     "# 0-30ft Dives",
                     "#31-60ft Dives",
                     "#61-100ft Dives",
                     "# 131-150ft Dives",
                     "# 151-190ft Dives",
                     "# 191+ft Dives"), 
                   ~sum(., na.rm = T))) %>%
ggpairs(columns = 2:14)

# What is our 5 year incident rate (per dive)?
totalDives <- Stats5year %>%
  mutate(totalDives = rowSums(across(c(`# Air Dives`,
                                       `# MIxed Gas Dives`,
                                       `# Nitrox Dives`)))) %>%
  mutate(totalIncidents = `Total  Dive Incidents`)
# incidents per dive
sum(totalDives$totalIncidents)/sum(totalDives$totalDives)
# dives per incident
sum(totalDives$totalDives)/sum(totalDives$totalIncidents)

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####