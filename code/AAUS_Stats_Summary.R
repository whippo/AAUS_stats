#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# AAUS Stats Summary                                                          ##
# Script Created 2024-12-18                                                   ##
# Data source: American Academy of Underwater Sciences                        ##
# R code prepared by Ross Whippo                                              ##  
# Last updated 2025-10-05                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# Exploratory summary statistics of AAUS stats submissions

# Required Files (check that script is loading latest version):
# ResultsGrid_ExportData.csv

# Associated Scripts:
# AAUS_Stats_Summary.qmd

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

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(scales)
# library(ggpubr)
library(viridis)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# individual years
Stats2025 <- read_csv("data/AAUS_Stats_2025.csv")
Stats2024 <- read_csv("data/AAUS_Stats_2024.csv")
Stats2023 <- read_csv("data/AAUS_Stats_2023.csv")
Stats2022 <- read_csv("data/AAUS_Stats_2022.csv")
Stats2021 <- read_csv("data/AAUS_Stats_2021.csv")
Stats2020 <- read_csv("data/AAUS_Stats_2020.csv")
Stats2019 <- read_csv("data/AAUS_Stats_2019.csv")
Stats2018 <- read_csv("data/AAUS_Stats_2018.csv")
Stats2017 <- read_csv("data/AAUS_Stats_2017.csv")
Stats2016 <- read_csv("data/AAUS_Stats_2016.csv")
Stats2015 <- read_csv("data/AAUS_Stats_2015.csv")
Stats2014 <- read_csv("data/AAUS_Stats_2014.csv")
Stats2013 <- read_csv("data/AAUS_Stats_2013.csv")
Stats2012 <- read_csv("data/AAUS_Stats_2012.csv")
Stats2011 <- read_csv("data/AAUS_Stats_2011.csv")
Stats2010 <- read_csv("data/AAUS_Stats_2010.csv")
Stats2009 <- read_csv("data/AAUS_Stats_2009.csv")
Stats2008 <- read_csv("data/AAUS_Stats_2008.csv")
Stats2007 <- read_csv("data/AAUS_Stats_2007.csv")
Stats2006 <- read_csv("data/AAUS_Stats_2006.csv")
Stats2005 <- read_csv("data/AAUS_Stats_2005.csv")
Stats2004 <- read_csv("data/AAUS_Stats_2004.csv")
Stats2003 <- read_csv("data/AAUS_Stats_2003.csv")
Stats2002 <- read_csv("data/AAUS_Stats_2002.csv")
Stats2001 <- read_csv("data/AAUS_Stats_2001.csv")
Stats2000 <- read_csv("data/AAUS_Stats_2000.csv")
Stats1999 <- read_csv("data/AAUS_Stats_1999.csv")
Stats1998 <- read_csv("data/AAUS_Stats_1998.csv")

# single import
StatsALLyear <- read_csv("data/All_Stats.csv")

# remove duplicates
dupes <- StatsALLyear %>%
  unite(OM_year, OM, Year, sep = "_") %>%
  filter(duplicated(OM_year))

StatsALLfiltered <- StatsALLyear %>%
  distinct()

rm(dupes, StatsALLyear)

# data by reporting period
Original10 <- StatsALLfiltered %>%
  filter(Year %in% c(1998:2007))

New10 <- StatsALLfiltered %>%
  filter(Year %in% c(2008:2017))


# incidents by category type
incident_cats <- read_csv("data/Incident_categories.csv")

# Industry Rates
industry <- read_csv("data/industry_rates.csv")

# actual DCI incidence
DCI_known <- read_csv("data/DCI_report.csv")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ALL STAT SUMMARY                                                          ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### dives per year and incident rates Dardeau 2007
incidents <- StatsALLfiltered %>%
  select(Year, `Total  Dive Incidents`) %>%
  group_by(Year) %>%
  summarise(incidents = sum(`Total  Dive Incidents`))

DCI_cats <- incident_cats %>%
  group_by(Year) %>%
  count(Type) %>%
  pivot_wider(
    names_from = Type,
    values_from = n
  )
DCI_cats[is.na(DCI_cats)] <- 0
DCI_cats <- DCI_cats %>%
  mutate(DCI_BT = sum(Hyperbaric, Barotrauma)) %>%
  mutate(DCI_cases = Hyperbaric)

divers <- StatsALLfiltered %>%
  select(Year, `Total Divers`) %>%
  group_by(Year) %>%
  summarise(divers = sum(`Total Divers`))

dives <- StatsALLfiltered %>%
  select(Year, `# Science Dives`, `#T/P Dives`) %>%
  mutate(TotalDives = `# Science Dives` + `#T/P Dives`) %>%
  group_by(Year) %>%
  summarise(TotalDives = sum(TotalDives))

minutes <- StatsALLfiltered %>%
  select(Year, `Science Dive Time (min)`, `T/P Dive Time (min)`) %>%
  mutate(TotalMins = `Science Dive Time (min)` + `T/P Dive Time (min)`) %>%
  group_by(Year) %>%
  summarise(mins = sum(TotalMins))

incident_rates <- divers %>%
  left_join(incidents) %>%
  left_join(DCI_cats) %>%
  left_join(dives) %>%
  left_join(DCI_known) %>%
  left_join(minutes) %>%
  mutate(DCI_cases = ifelse(is.na(DCI_likely), DCI_cases, DCI_likely)) %>%
  mutate(rate = (incidents / divers) * 100) %>%
  mutate(DCI_BT_rate = (DCI_BT / divers) * 100) %>%
  mutate(DCI_rate = (DCI_cases / divers) * 100)

# Total dives
DiveNew <- New10 %>%
  select(Year, `# Science Dives`, `#T/P Dives`) %>%
  mutate(TotalDives = `# Science Dives` + `#T/P Dives`) %>%
  group_by(Year) %>%
  summarise(TotalDives = sum(TotalDives))
DiveNew %>%
  summarise(sum(TotalDives))
DiveNew %>%
  summarise(mean(TotalDives))
DiveNew %>%
  summarise(median(TotalDives))
DiveNew %>%
  summarise(sd(TotalDives))
DivePlot <- DiveNew %>%
  ggplot(aes(x = Year, y = TotalDives)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_discrete("", labels = NULL, limits = c(2008:2017)) +
  ylab(label = "Total Annual Dives") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(r = 7)))

# of OMs
New10 %>%
  select(OM) %>%
  distinct(OM) %>%
  count(OM) %>%
  summarise(sum(n))
# 172


# mean divers, sd divers per year
NewDivers <- New10 %>%
  select(Year, `Total Divers`) %>%
  group_by(Year) %>%
  summarise(n = sum(`Total Divers`))
NewDivers %>%
  summarise(mean(n))
NewDivers %>%
  summarise(median(n))
NewDivers %>%
  summarise(sd(n))
DiversPlot <- NewDivers %>%
  ggplot(aes(x = Year, y = n)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_discrete("", labels = NULL, limits = c(2008:2017)) +
  ylab(label = "Total Annual Divers") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(r = 17)))


# mean minutes, sd minutes per year
NewMins <- New10 %>%
  select(Year, `Science Dive Time (min)`, `T/P Dive Time (min)`) %>%
  mutate(TotalMins = `Science Dive Time (min)` + `T/P Dive Time (min)`) %>%
  group_by(Year) %>%
  summarise(n = sum(TotalMins))
NewMins %>%
  summarise(mean(n))
NewMins %>%
  summarise(median(n))
NewMins %>%
  summarise(sd(n))
MinPlot <- NewMins %>%
  ggplot(aes(x = Year, y = n)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_discrete(limits = c(2008:2017)) +
  ylab(label = "Total Annual Dive Minutes") +
  theme_minimal()

ggarrange(DivePlot, DiversPlot, MinPlot,
  nrow = 3, ncol = 1,
  labels = c("a", "b", "c"),
  hjust = c(-10, -9, -10),
  vjust = 3
)

# total incidents in reporting window
incident_rates %>%
  filter(Year %in% c(2008:2017)) %>%
  select(incidents) %>%
  sum()
# 83

# total pressure-related incidents
incident_rates %>%
  filter(Year %in% c(2008:2017)) %>%
  select(DCI_BT) %>%
  sum()
# 45

# total DCI/suspected DCI
incident_rates %>%
  filter(Year %in% c(2008:2017)) %>%
  select(DCI_cases) %>%
  sum()
# 21

# DCI/barotrauma count
incident_rates %>%
  filter(Year %in% c(2008:2017)) %>%
  select(Year, DCI_BT)

# DCI/barotrauama rates
incident_rates %>%
  filter(Year %in% c(2008:2017)) %>%
  select(Year, DCI_BT_rate)

# DCI only count
incident_rates %>%
  filter(Year %in% c(2008:2017)) %>%
  select(Year, DCI_cases)

# DCI only rates
incident_rates %>%
  filter(Year %in% c(2008:2017)) %>%
  select(Year, DCI_rate)

# all rates by year
incident_rates %>%
  filter(Year %in% c(1998:2017)) %>%
  select(Year, rate)


# y = all incidents, pressure-related, DCI
# x = number of dives | Year | minutes | divers

YearAll <- incident_rates %>%
  filter(Year %in% c(1998:2017)) %>%
  ggplot(aes(x = Year, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_discrete("", labels = NULL, limits = c(1998:2017)) +
  ylab(label = "Incidence Rate - All Injuries") +
  theme_minimal()
YearAll <- annotate_figure(YearAll,
  top = text_grob(bquote(R^2 * "= 0.002"),
    vjust = 2,
    hjust = -1
  )
)

lmYearAll <- lm(rate ~ Year, data = incident_rates)
summary(lmYearAll)


YearPress <- incident_rates %>%
  filter(Year %in% c(1998:2017)) %>%
  ggplot(aes(x = Year, y = DCI_BT_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_discrete("", labels = NULL, limits = c(1998:2017)) +
  ylab(label = "Incidence Rate - Pressure-Related Injuries") +
  theme_minimal()
YearPress <- annotate_figure(YearPress,
  top = text_grob(bquote(R^2 * "= 0.39"),
    vjust = 2,
    hjust = -1
  )
)

lmYearPress <- lm(DCI_BT_rate ~ Year, data = incident_rates)
summary(lmYearPress)

YearDCI <- incident_rates %>%
  filter(Year %in% c(1998:2017)) %>%
  ggplot(aes(x = Year, y = DCI_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_discrete(limits = c(1998:2017), labels = c(
    "1998", "", "", "", "",
    "", "2004", "", "", "",
    "", "", "2010", "", "",
    "", "", "", "2016",
    ""
  )) +
  ylab(label = "Incidence Rate - DCI") +
  theme_minimal()
YearDCI <- annotate_figure(YearDCI,
  top = text_grob(bquote(R^2 * "= 0.53"),
    vjust = 2,
    hjust = -1
  )
)

lmYearDCI <- lm(DCI_rate ~ Year, data = incident_rates)
summary(lmYearDCI)

DivesAll <- incident_rates %>%
  filter(Year %in% c(1998:2017)) %>%
  ggplot(aes(x = TotalDives, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous("", labels = NULL) +
  scale_x_continuous("", labels = NULL) +
  theme_minimal()
DivesAll <- annotate_figure(DivesAll,
  top = text_grob(bquote(R^2 * "= 0.04"),
    vjust = 2,
    hjust = -1
  )
)

lmDivesAll <- lm(rate ~ TotalDives, data = incident_rates)
summary(lmDivesAll)

DivesPress <- incident_rates %>%
  filter(Year %in% c(1998:2017)) %>%
  ggplot(aes(x = TotalDives, y = DCI_BT_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous("", labels = NULL) +
  scale_x_continuous("", labels = NULL) +
  theme_minimal()
DivesPress <- annotate_figure(DivesPress,
  top = text_grob(bquote(R^2 * "= 0.08"),
    vjust = 2,
    hjust = -1
  )
)

lmDivesPress <- lm(DCI_BT_rate ~ TotalDives, data = incident_rates)
summary(lmDivesPress)

DivesDCI <- incident_rates %>%
  filter(Year %in% c(1998:2017)) %>%
  ggplot(aes(x = TotalDives, y = DCI_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous("", labels = NULL) +
  scale_x_continuous("Total Annual Number Of Dives") +
  theme_minimal()
DivesDCI <- annotate_figure(DivesDCI,
  top = text_grob(bquote(R^2 * "= 0.30"),
    vjust = 2,
    hjust = -1
  )
)

lmDivesDCI <- lm(DCI_rate ~ TotalDives, data = incident_rates)
summary(lmDivesDCI)

DiversAll <- incident_rates %>%
  filter(Year %in% c(1998:2017)) %>%
  ggplot(aes(x = divers, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous("", labels = NULL) +
  scale_x_continuous("", labels = NULL) +
  theme_minimal()
DiversAll <- annotate_figure(DiversAll,
  top = text_grob(bquote(R^2 * "= 0.06"),
    vjust = 2,
    hjust = -1
  )
)

lmDiversAll <- lm(rate ~ divers, data = incident_rates)
summary(lmDiversAll)

DiversPress <- incident_rates %>%
  filter(Year %in% c(1998:2017)) %>%
  ggplot(aes(x = divers, y = DCI_BT_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous("", labels = NULL) +
  scale_x_continuous("", labels = NULL) +
  theme_minimal()
DiversPress <- annotate_figure(DiversPress,
  top = text_grob(bquote(R^2 * "= 0.25"),
    vjust = 2,
    hjust = -1
  )
)

lmDiversPress <- lm(DCI_BT_rate ~ divers, data = incident_rates)
summary(lmDiversPress)

DiversDCI <- incident_rates %>%
  filter(Year %in% c(1998:2017)) %>%
  ggplot(aes(x = divers, y = DCI_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous("", labels = NULL) +
  xlab(label = "Total Annual Number Of Divers") +
  theme_minimal()
DiversDCI <- annotate_figure(DiversDCI,
  top = text_grob(bquote(R^2 * "= 0.46"),
    vjust = 2,
    hjust = -1
  )
)

lmDiversDCI <- lm(DCI_rate ~ divers, data = incident_rates)
summary(lmDiversDCI)

ggarrange(YearAll, DivesAll, DiversAll,
  YearPress, DivesPress, DiversPress,
  YearDCI, DivesDCI, DiversDCI,
  ncol = 3,
  nrow = 3,
  labels = c(
    "a", "b", "c",
    "d***", "e", "f**",
    "g***", "h**", "i***"
  ),
  hjust = c(
    -3, -1, -1,
    -1, -1, -1,
    -1, -1, -1
  ),
  vjust = c(
    1, 1, 1,
    1, 1, 1,
    1, 1, 1
  )
)


industry %>%
  ggplot(aes(x = Year, y = Rate, color = Industry)) +
  geom_point() +
  geom_line() +
  scale_color_viridis(discrete = TRUE, option = "turbo") +
  ylab(label = "Incidence Rate") +
  theme_minimal()

Allview <- incident_rates %>%
  filter(Year %in% c(1998:2017)) %>%
  select(Year, rate, DCI_BT_rate, DCI_rate)


# 10 year periods stats

# DCI slopes
summary(incident_rates %>%
  filter(Year %in% c(1998:2007)) %>%
  lm(DCI_cases ~ Year, data = .))
coef(incident_rates %>%
  filter(Year %in% c(1998:2007)) %>%
  lm(DCI_cases ~ Year, data = .))

summary(incident_rates %>%
  filter(Year %in% c(2008:2017)) %>%
  lm(DCI_cases ~ Year, data = .))
coef(incident_rates %>%
  filter(Year %in% c(2008:2017)) %>%
  lm(DCI_cases ~ Year, data = .))

# mean DCI rates
incident_rates %>%
  filter(Year %in% c(1998:2007)) %>%
  select(Year, DCI_rate) %>%
  summarise(mean(DCI_rate))
# 0.13

incident_rates %>%
  filter(Year %in% c(2008:2017)) %>%
  select(Year, DCI_rate) %>%
  summarise(mean(DCI_rate))
# 0.04

rate_period <- incident_rates %>%
  filter(Year %in% c(1998:2017)) %>%
  select(Year, DCI_rate, DCI_BT_rate) %>%
  mutate(period = case_when(
    Year == 1998 ~ "one",
    Year == 1999 ~ "one",
    Year == 2000 ~ "one",
    Year == 2001 ~ "one",
    Year == 2002 ~ "one",
    Year == 2003 ~ "one",
    Year == 2004 ~ "one",
    Year == 2005 ~ "one",
    Year == 2006 ~ "one",
    Year == 2007 ~ "one",
    Year == 2008 ~ "two",
    Year == 2009 ~ "two",
    Year == 2010 ~ "two",
    Year == 2011 ~ "two",
    Year == 2012 ~ "two",
    Year == 2013 ~ "two",
    Year == 2014 ~ "two",
    Year == 2015 ~ "two",
    Year == 2016 ~ "two",
    Year == 2017 ~ "two"
  ))
t.test(DCI_rate ~ period, data = rate_period)
t.test(DCI_BT_rate ~ period, data = rate_period)


# minutes figure
incident_rates %>%
  filter(Year %in% 1998:2017) %>%
  select(mins, rate, DCI_BT_rate, DCI_rate) %>%
  pivot_longer(rate:DCI_rate, names_to = "Category", values_to = "Incidence") %>%
  ggplot(aes(x = mins, y = Incidence, color = Category)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis(
    discrete = TRUE, option = "plasma", end = 0.8,
    labels = c("DCI/Barotrauma", "DCI", "All Injuries")
  ) +
  xlab(label = "Total Minutes") +
  theme_minimal()

# incidence per 10000 dives means
# mean DCI rates
incident_rates %>%
  filter(Year %in% c(1998:2007)) %>%
  select(DCI_cases, DCI_BT, TotalDives) %>%
  mutate(ratePress = 10000 / (TotalDives / DCI_BT)) %>%
  mutate(rateDCI = 10000 / (TotalDives / DCI_cases)) %>%
  summarise(across(ratePress:rateDCI, mean))
# 0.13

incident_rates %>%
  filter(Year %in% c(2008:2017)) %>%
  select(DCI_cases, DCI_BT, TotalDives) %>%
  mutate(ratePress = 10000 / (TotalDives / DCI_BT)) %>%
  mutate(rateDCI = 10000 / (TotalDives / DCI_cases)) %>%
  summarise(across(ratePress:rateDCI, mean))
# 0.13



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2025 STATS REPORT                                                         ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Stats5year <- StatsALLfiltered |>
  filter(Year > 2020)


# of OMs reporting by year
Stats5year %>%
  unite(OM_year, OM, Year, sep = "_") %>%
  distinct(OM_year) %>%
  separate_wider_delim(OM_year, delim = "_", names = c("OM", "Year")) %>%
  mutate(OMnum = 1) %>%
  ggplot() +
  stat_summary(aes(x = Year, y = OMnum, group = 1),
    fun = sum,
    geom = "line",
    size = 2,
    color = "#414487FF"
  ) +
  coord_cartesian(ylim = c(120, 170)) +
  labs(y = "Number of OMs Reporting") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18, margin = margin(r = 15))
  )

Stats5year %>%
  unite(OM_year, OM, Year, sep = "_") %>%
  distinct(OM_year) %>%
  separate_wider_delim(OM_year, delim = "_", names = c("OM", "Year")) %>%
  mutate(OMnum = 1) %>%
  group_by(Year) %>%
  summarise(sum(OMnum))

# of divers logging dives
Stats5year %>%
  unite(OM_year, OM, Year, sep = "_") %>%
  distinct(OM_year, .keep_all = TRUE) %>%
  separate_wider_delim(OM_year, delim = "_", names = c("OM", "Year")) %>%
  ggplot() +
  stat_summary(aes(x = Year, y = `Total Divers`, group = 1),
    fun = sum,
    geom = "line",
    size = 2,
    color = "#414487FF"
  ) +
  labs(y = "Total Number of Divers") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18, margin = margin(r = 15))
  )

Stats5year %>%
  group_by(Year) %>%
  summarise(sum(`Total Divers`))

# Dives logged by type
Stats5year %>%
  select(Year, `# Science Dives`, `#T/P Dives`) %>%
  pivot_longer(`# Science Dives`:`#T/P Dives`, names_to = "Dive Purpose") %>%
  group_by(Year, `Dive Purpose`) %>%
  summarise(`Number of Dives` = sum(value)) %>%
  ggplot() +
  geom_col(aes(x = Year, y = `Number of Dives`, fill = `Dive Purpose`),
    position = "dodge"
  ) +
  scale_fill_viridis(
    discrete = TRUE,
    option = "G",
    labels = c("Science", "Training/Proficiency"),
    begin = 0.4,
    end = 0.8
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18, margin = margin(r = 15))
  )

Stats5year %>%
  select(Year, `# Science Dives`, `#T/P Dives`) %>%
  pivot_longer(`# Science Dives`:`#T/P Dives`, names_to = "Dive Purpose") %>%
  group_by(Year, `Dive Purpose`) %>%
  summarise(`Number of Dives` = sum(value))

# dive minutes by type
Stats5year %>%
  select(Year, `Science Dive Time (min)`, `T/P Dive Time (min)`) %>%
  pivot_longer(`Science Dive Time (min)`:`T/P Dive Time (min)`, names_to = "Dive Purpose") %>%
  group_by(Year, `Dive Purpose`) %>%
  summarise(`Dive Minutes` = sum(value)) %>%
  ggplot() +
  geom_col(aes(x = Year, y = `Dive Minutes`, fill = `Dive Purpose`),
    position = "dodge"
  ) +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis(
    discrete = TRUE,
    option = "G",
    labels = c("Science", "Training/Proficiency"),
    begin = 0.4,
    end = 0.8
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18, margin = margin(r = 15))
  )

Stats5year %>%
  select(Year, `Science Dive Time (min)`, `T/P Dive Time (min)`) %>%
  pivot_longer(`Science Dive Time (min)`:`T/P Dive Time (min)`, names_to = "Dive Purpose") %>%
  group_by(Year, `Dive Purpose`) %>%
  summarise(`Dive Minutes` = sum(value))


# Dive incident prevalence
Stats5year %>%
  select(Year, `# Science Dives`, `#T/P Dives`, `Total  Dive Incidents`) %>%
  mutate(`Total Dives` = `# Science Dives` + `#T/P Dives`) %>%
  group_by(Year) %>%
  summarise(across(`Total  Dive Incidents`:`Total Dives`, sum)) %>%
  mutate(`Incident Rate (per 10,000 dives)` = `Total  Dive Incidents` / (`Total Dives` / 10000)) %>%
  ggplot() +
  geom_line(aes(x = Year, y = `Incident Rate (per 10,000 dives)`), size = 2, color = "#414487FF") +
  scale_color_viridis(
    discrete = TRUE,
    option = "G"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18, margin = margin(r = 15))
  )

Stats5year %>%
  select(Year, `# Science Dives`, `#T/P Dives`, `Total  Dive Incidents`) %>%
  mutate(`Total Dives` = `# Science Dives` + `#T/P Dives`) %>%
  group_by(Year) %>%
  summarise(across(`Total  Dive Incidents`:`Total Dives`, sum)) %>%
  mutate(`Incident Rate (per 10,000 dives)` = `Total  Dive Incidents` / (`Total Dives` / 10000))

# Dives by Mode
Stats5year %>%
  select(Year, `# Open Circuit Dives`, `# Hookah Dives`, `# Rebreather Dives`, `# Surface Supplied Dives`) %>%
  pivot_longer(`# Open Circuit Dives`:`# Surface Supplied Dives`, names_to = "Dive Mode") %>%
  group_by(Year, `Dive Mode`) %>%
  summarise(`Total Dives` = sum(value)) %>%
  ggplot() +
  geom_col(aes(x = Year, y = `Total Dives`, fill = `Dive Mode`),
    position = "dodge", width = 0.66
  ) +
  scale_y_continuous(labels = comma, trans = "log10") +
  scale_fill_viridis(
    discrete = TRUE,
    option = "H",
    labels = c("Hookah", "Open Circuit", "Rebreather", "Surface Supply"),
    begin = 0.2,
    end = 0.8
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18, margin = margin(r = 15))
  )

Stats5year %>%
  select(Year, `# Open Circuit Dives`, `# Hookah Dives`, `# Rebreather Dives`, `# Surface Supplied Dives`) %>%
  pivot_longer(`# Open Circuit Dives`:`# Surface Supplied Dives`, names_to = "Dive Mode") %>%
  group_by(Year, `Dive Mode`) %>%
  summarise(`Total Dives` = sum(value))

# Dives by Gas
Stats5year %>%
  select(Year, `# Air Dives`, `# Nitrox Dives`, `# MIxed Gas Dives`) %>%
  pivot_longer(`# Air Dives`:`# MIxed Gas Dives`, names_to = "Dive Gas") %>%
  group_by(Year, `Dive Gas`) %>%
  summarise(`Total Dives` = sum(value)) %>%
  ggplot() +
  geom_col(aes(x = Year, y = `Total Dives`, fill = `Dive Gas`),
    position = "dodge", width = 0.75
  ) +
  scale_y_continuous(labels = comma, trans = "log10") +
  scale_fill_viridis(
    discrete = TRUE,
    option = "A",
    labels = c("Air", "Mixed Gas", "Nitrox"),
    begin = 0.2,
    end = 0.8
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18, margin = margin(r = 15))
  )

Stats5year %>%
  select(Year, `# Air Dives`, `# Nitrox Dives`, `# MIxed Gas Dives`) %>%
  pivot_longer(`# Air Dives`:`# MIxed Gas Dives`, names_to = "Dive Gas") %>%
  group_by(Year, `Dive Gas`) %>%
  summarise(`Total Dives` = sum(value))

# Dives by Environment
Stats5year %>%
  select(
    Year, `# Aquarium Dives`, `# Bluewater Dives`, `# IcePolar Dives`, `# Overhead Dives`,
    `# ReqDecomp Dives`
  ) %>%
  pivot_longer(`# Aquarium Dives`:`# ReqDecomp Dives`, names_to = "Specialized Environment") %>%
  group_by(Year, `Specialized Environment`) %>%
  summarise(`Total Dives` = sum(value)) %>%
  ggplot() +
  geom_col(aes(x = Year, y = `Total Dives`, fill = `Specialized Environment`),
    position = "dodge", width = 0.6
  ) +
  scale_y_continuous(labels = comma, trans = "log10") +
  scale_fill_viridis(
    discrete = TRUE,
    option = "G",
    labels = c("Aquarium", "Bluewater", "Ice/Polar", "Overhead", "Req. Decomp."),
    begin = 0.1,
    end = 0.9
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18, margin = margin(r = 15))
  )

Stats5year %>%
  select(
    Year, `# Aquarium Dives`, `# Bluewater Dives`, `# IcePolar Dives`, `# Overhead Dives`,
    `# ReqDecomp Dives`
  ) %>%
  pivot_longer(`# Aquarium Dives`:`# ReqDecomp Dives`, names_to = "Specialized Environment") %>%
  group_by(Year, `Specialized Environment`) %>%
  summarise(`Total Dives` = sum(value))

# Dives by Depth
Stats5year %>%
  select(
    Year, `# 0-30ft Dives`, `#31-60ft Dives`, `#61-100ft Dives`, `# 101-130ft Dives`,
    `# 131-150ft Dives`, `# 151-190ft Dives`, `# 191+ft Dives`
  ) %>%
  pivot_longer(`# 0-30ft Dives`:`# 191+ft Dives`, names_to = "Depth") %>%
  mutate(Depth = factor(Depth, levels = c(
    "# 0-30ft Dives",
    "#31-60ft Dives",
    "#61-100ft Dives",
    "# 101-130ft Dives",
    "# 131-150ft Dives",
    "# 151-190ft Dives",
    "# 191+ft Dives"
  ))) %>%
  group_by(Year, `Depth`) %>%
  summarise(`Total Dives` = sum(value)) %>%
  ggplot() +
  geom_col(aes(x = Year, y = `Total Dives`, fill = `Depth`)) +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis(
    discrete = TRUE,
    option = "G",
    labels = c("0-30ft", "31-60ft", "61-100ft", "101-130ft", "131-150ft", "151-190ft", "190+ft"),
    begin = 0.9,
    end = 0.1
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18, margin = margin(r = 15))
  )

Stats5year %>%
  select(
    Year, `# 0-30ft Dives`, `#31-60ft Dives`, `#61-100ft Dives`, `# 101-130ft Dives`,
    `# 131-150ft Dives`, `# 151-190ft Dives`, `# 191+ft Dives`
  ) %>%
  pivot_longer(`# 0-30ft Dives`:`# 191+ft Dives`, names_to = "Depth") %>%
  mutate(Depth = factor(Depth, levels = c(
    "# 0-30ft Dives",
    "#31-60ft Dives",
    "#61-100ft Dives",
    "# 101-130ft Dives",
    "# 131-150ft Dives",
    "# 151-190ft Dives",
    "# 191+ft Dives"
  ))) %>%
  group_by(Year, `Depth`) %>%
  summarise(`Total Dives` = sum(value))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2024 Stats                                                                ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# identify OMs that submitted on time for 2024
OM_list_2025 <- read_csv("data/OM_list_2025.csv")
OMs_unique <- data.frame(unique(OM_list_2025$Organization))

# ontime
Stats_2024_ontime <- read_csv("data/Stats_2024_ontime.csv")
Ontime_unique <- data.frame(unique(Stats_2024_ontime$Company))

# late (april 1)
Stats_2024_late <- read_csv("data/Stats_2024_late.csv")
Late_unique <- data.frame(unique(Stats_2024_late$Company))

# very late (april 2)
Stats_2024_verylate <- read_csv("data/Stats_2024_verylate.csv")
Very_unique <- data.frame(unique(Stats_2024_verylate))

# late as of March 31
late1 <- data.frame(setdiff(OM_list_2025$Organization, Ontime_unique$unique.Stats_2024_ontime.Company.))

# late as of April 1
late2 <- data.frame(setdiff(OM_list_2025$Organization, Late_unique$unique.Stats_2024_late.Company.))

# late as of April 2
late3 <- data.frame(setdiff(OM_list_2025$Organization, Very_unique$Company))


setdiff(
  late1$setdiff.OM_list_2025.Organization..Ontime_unique.unique.Stats_2024_ontime.Company..,
  late2$setdiff.OM_list_2025.Organization..Late_unique.unique.Stats_2024_late.Company..
)

setdiff(
  late1$setdiff.OM_list_2025.Organization..Ontime_unique.unique.Stats_2024_ontime.Company..,
  late3$Company
)

write_csv(late1, "data/lateOMs2024stats.csv")
write_csv(late3, "data/verylateOMs2024stats.csv")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

colnames(Stats5year)

# how many air dives per OM in last 5 years?
Stats5year %>%
  ggplot() +
  geom_line(aes(x = Year, y = log10(`# Air Dives` + 1), color = OM)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_smooth(aes(x = Year, y = log10(`# Air Dives` + 1)),
    method = "loess", color = "black",
    se = FALSE, size = 5
  )

# how many dives total per OM in the last 5 years?
Stats5year %>%
  mutate(totalDives = rowSums(across(c(
    `# Air Dives`,
    `# MIxed Gas Dives`,
    `# Nitrox Dives`
  )))) %>%
  ggplot() +
  geom_line(aes(x = Year, y = totalDives, color = OM)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_smooth(aes(x = Year, y = totalDives),
    method = "loess", color = "black",
    se = FALSE, size = 5
  )

# how many dives of each mode were there this year?
Stats2023 %>%
  pivot_longer(
    cols = c(
      `# Hookah Dives`,
      `# Open Circuit Dives`,
      `# Rebreather Dives`,
      `# Surface Supplied Dives`
    ),
    names_to = "Mode",
    values_to = "Dives"
  ) %>%
  ggplot() +
  geom_col(aes(x = Mode, y = Dives))

# how many minutes of each mode were there this year?
Stats2023 %>%
  pivot_longer(
    cols = c(
      `Hookah Dive Time (min)`,
      `Open Circuit Dive Time (min)`,
      `Rebreather Dive Time (min)`,
      `Surface Supplied Dive Time (min)`
    ),
    names_to = "Mode",
    values_to = "Time"
  ) %>%
  ggplot() +
  geom_col(aes(x = Mode, y = log10(Time)))

# pairs plot 5 years
Stats5year %>%
  group_by(Year = as.character(Year)) %>%
  summarise(across(
    c(
      "Total Divers",
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
      "# 191+ft Dives"
    ),
    ~ sum(., na.rm = T)
  )) %>%
  ggpairs(columns = 2:14)

# What is our 5 year incident rate (per dive)?
totalDives <- Stats5year %>%
  mutate(totalDives = rowSums(across(c(
    `# Air Dives`,
    `# MIxed Gas Dives`,
    `# Nitrox Dives`
  )))) %>%
  mutate(totalIncidents = `Total  Dive Incidents`)
# incidents per dive
sum(totalDives$totalIncidents) / sum(totalDives$totalDives)
# dives per incident
sum(totalDives$totalDives) / sum(totalDives$totalIncidents)

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

# FIU all stats check
sort(unique(StatsALLfiltered$OM))

FIU <- StatsALLfiltered |>
  filter(OM == "Florida International University")

str(FIU)

## Plot various metrics

# number of divers
FIU |>
  ggplot() +
  geom_point(aes(x = Year, y = `Total Divers`))

# dives by type
FIU |>
  ggplot(aes(x = Year)) +
  geom_point(aes(y = `# Air Dives`), color = "blue") +
  geom_point(aes(y = `# Nitrox Dives`), color = "yellow") +
  geom_point(aes(y = `# Science Dives`), color = "red") +
  geom_point(aes(y = `#T/P Dives`), color = "green") 

# divers versus dives
FIU |>
  ggplot(aes(x = Year)) +
  geom_point(aes(y = `# Air Dives`), color = "blue") +
  geom_point(aes(y = `# Air Divers`), color = "lightblue") +
  geom_point(aes(y = `Air Dive Time (min)`), color = "darkblue") 

# time versus depths
sum(FIU$`# 191+ft Dives`)

FIU |>
  select(Year,
         `Air Dive Time (min)`,
         `Nitrox Dive Time (min)`,
         `# 0-30ft Dives`,
         `#31-60ft Dives`,
         `#61-100ft Dives`,
         `# 101-130ft Dives`,
         `# 131-150ft Dives`,
         `# 151-190ft Dives`,
         `# 191+ft Dives`) |>
  pivot_longer(cols = `# 0-30ft Dives`:`# 191+ft Dives`,
               names_to = "depth",
               values_to = "dives") |>
  mutate(depth = factor(depth, levels = c("# 0-30ft Dives",
                                      "#31-60ft Dives",
                                      "#61-100ft Dives",
                                      "# 101-130ft Dives",
                                      "# 131-150ft Dives",
                                      "# 151-190ft Dives",
                                      "# 191+ft Dives"
                                      ))) |>
  mutate(totalTime = `Air Dive Time (min)` + `Nitrox Dive Time (min)`) |>
  ggplot(aes(x = Year)) +
    geom_point(aes(y = dives, fill = depth), shape = 21, size = 3) +
    scale_fill_viridis(discrete = TRUE) +
  theme_bw()

# average of total divers to total dives
FIU |>
  select(Year, `Total Divers`, `# Science Dives`, `#T/P Dives`) |>
  mutate(DiveDiverAvg = (`# Science Dives` + `#T/P Dives`)/`Total Divers`) |>
  ggplot(aes(x = Year)) +
  geom_point(aes(y = DiveDiverAvg))

# MOTE 2024 Issue

mote <- rawStats |>
  filter(OM == "Mote Marine Laboratory and Aquarium")

# Stats for the DCI Incidence Rates AAUS Page --------------

incident_summary <- StatsALLfiltered |>
  filter(Year > 1997) |>
  select(Year, 
         OM, 
         `Total Divers`, 
         `# Science Dives`, 
         `#T/P Dives`,
         `Total  Dive Incidents`) |>
  mutate(`Total Dives` = `# Science Dives` + `#T/P Dives`) |>
  group_by(Year) |>
  summarise(`OMs Reporting` = n(),
            `Total Divers` = sum(`Total Divers`),
            `Total Person-Dives` = sum(`Total Dives`),
            `Total Dive Incidents` = sum(`Total  Dive Incidents`))

DCI <- tibble(`Number Of DCI/Suspected DCI` = c(2,3,4,3,5,2,3,3,4,4,0,4,2,4),
              Year = seq(1998, 2011, 1))

# incidents by category type
incident_cats <- read_csv("data/Incident_categories.csv")
incident_recent <- incident_cats |>
  filter(Year > 2011) |>
  filter(Type == "Hyperbaric") |>
  mutate(Total = 1) |>
  group_by(Year) |>
  summarise(`Number Of DCI/Suspected DCI` = sum(Total)) |>
  bind_rows(tibble(Year = c(2014, 2017, 2018, 2020, 2021, 2022, 2023),
                   `Number Of DCI/Suspected DCI` = c(0,0,0,0,0,0,0))) |>
  bind_rows(DCI)

incident_tab <- incident_summary %>%
  left_join(incident_recent)

write_csv(incident_tab, "~/Desktop/DCI_incidence_rates.csv")

sum(incident_tab$`Number Of DCI/Suspected DCI`)
sum(incident_tab$`Total Person-Dives`)

61*(10000/3203402)


