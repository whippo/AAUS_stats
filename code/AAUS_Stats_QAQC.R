#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# AAUS Stats QAQC                                                             ##
# Data are current as of 2024-12-18                                           ##
# Data source: American Academy of Underwater Sciences                        ##
# R code prepared by Ross Whippo                                              ##  
# Last updated 2024-12-18                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# Script to detect anomalies in AAUS statistics submitted by OMs

# Required Files (check that script is loading latest version):
# ResultsGrid_ExportData.csv

# Associated Scripts:
# AAUS_Stats_Summary.R
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
# LOAD PACKAGES & FUNCTIONS                                                 ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)

source("functions/dive_diver_mismatch.R")
source("functions/solo_divers.R")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rawStats <- read_csv("data/ResultsGrid_ExportData.csv")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# QC DATA                                                                   ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dive_diver_mismatch(rawStats)

solo_divers(rawStats)

# Identify OMs with single divers



############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####



