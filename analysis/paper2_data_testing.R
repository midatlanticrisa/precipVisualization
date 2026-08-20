##########################################################################
# Copyright 2024 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Created: 
#
# This script....
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
##########################################################################

# install.packages("reshape")
library(reshape)
library(report)
library(ggplot2)
library(car)
library(multcomp)
library(FSA)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(vioplot)
library(coin)
library(rcompanion)
library(TeachingDemos)
library(cowplot)
library(gridGraphics)
library(scales)
library(tidytext)
library(gt)
library(webshot2)
library(tidyverse) # conflicts
library(wordcloud)
library(ggpubr)
# For ordered logit regression
require(foreign)
require(MASS)
# install.packages("Hmisc")
require(Hmisc)
require(reshape2)
# Use the lmtest package to run Likelihood Ratio Tests
library(lmtest)
library(VGAM)

# Proportional odds ratio testing
# install.packages("gofcat")
# install.packages("brant")
library(gofcat)
library(brant)
library(nnet)
library(ResourceSelection)

# Read in the survey data
source("scripts/readSurvey.R")

# Source the functions
source("scripts/surveyFunctions.R")
source("scripts/put_fig_letter.R")

# Read in the answers/ our interpretations to the questions
ans = read.csv("data/answer_sheet.csv")

# Read in climate scoring
clim = read.csv("data/climate_scoring.csv")

# Read in SUS table scoring
susTab = read.csv("data/susTable.csv", skip=2, header=TRUE)

# Create some color palettes
graphcol = brewer.pal(3, "YlGnBu")
rdYlGn = brewer.pal(6, "RdYlGn")
seqCols = brewer.pal(4, "OrRd")
yesNoCols = c("#cab2d6", "#6a3d9a")

##########################################################################
# Set plotting dimensions
# Original width	      Final width*
#                       Picas	Inches	Centimeters
# One column	          19	  3.2	    8
# 2/3 page width	      27	  4.5	    11.4
# Two columns	          33	  5.5	    14
# More than two columns	39	  6.5	    16.5

single_column = 3.2
med23_column = 4.5
double_column = 5.5
maximum_width = 6.5
column_height = 2.7
double_height = column_height * 2

##########################################################################
# Goodness of fit calculation ---------------------------------------------
HL_goodfit = function(outcome, vglm_model){
  obs_used   <- as.numeric(outcome) 
  pred_probs <- fitted(vglm_model)     
  
  # Verify alignment
  cat("obs length:", length(obs_used), "\n")
  cat("pred rows:", nrow(pred_probs), "\n")
  cat("Any NAs in obs_used:", any(is.na(obs_used)), "\n")
  cat("Any NAs in pred_probs:", any(is.na(pred_probs)), "\n")
  
  # Number of thresholds = number of outcome levels minus 1
  n_thresholds <- ncol(pred_probs) - 1
  
  # Hosmer-Lemeshow test at each cumulative threshold
  # Non-significant p (> 0.05) indicates adequate calibration at that threshold
  # Non-significant (p > 0.05) = no evidence of poor fit = good
  # Significant (p < 0.05) = evidence that predicted and observed probabilities disagree = bad
  for(k in 1:n_thresholds) {
    binary_obs <- as.integer(obs_used <= k)
    pred_cum   <- rowSums(pred_probs[, 1:k, drop = FALSE])
    hl         <- hoslem.test(binary_obs, pred_cum, g = 10)
    cat("Threshold", k, ": chi-sq =", round(hl$statistic, 3),
        ", df =", hl$parameter,
        ", p =", round(hl$p.value, 3), "\n")
  }
  cat("\nBonferroni corrected alpha:", round(0.05 / n_thresholds, 4), "\n")
}
##########################################################################
# Overall accuracy
##########################################################################
# Overall accuracy --------------------------------------------------------

# Calculate the overall accuracy for each survey response and group by graph type
oaAF = calcAccuracy(name="Area_Freq", var="AF", id="")
oaAI = calcAccuracy(name="Area_Int", var="AI", id=".1")
oaBF = calcAccuracy(name="Bar_freq", var="BF", id=".2")
oaBI = calcAccuracy(name="Bar_int", var="BI", id=".3")
oaXF = calcAccuracy(name="Box_freq", var="XF", id=".4")
oaXI = calcAccuracy(name="Box_int", var="XI", id=".5")

# -------------------------------------------------------------------------
# Is there a difference in overall accuracy across 3 graph types? 
#   Which group is different?
area = c(oaAF$tot, oaAI$tot)
bar = c(oaBF$tot, oaBI$tot)
box = c(oaXF$tot, oaXI$tot)

# Create a dataframe for One-way ANOVA
overAcc = data.frame(val = c(area,bar,box), 
                     name = c(rep("Area", length(area)), rep("Bar", length(bar)), 
                              rep("Box", length(box))))
overAcc$name = factor(overAcc$name, levels = c("Bar", "Box", "Area"))

accGroup = ifelse(overAcc$val <= 3, "Below average", overAcc$val)
accGroup = ifelse(overAcc$val >= 10, "Above average", accGroup)
accGroup = ifelse(overAcc$val >= 4 & overAcc$val <= 9, "Average", accGroup)

##########################################################################
# Climate
##########################################################################

redBlue = brewer.pal(4, "RdBu")

climAF = calcClimLit(name="Area_Freq", id="")
climAI = calcClimLit(name="Area_Int", id=".1")
climBF = calcClimLit(name="Bar_freq", id=".2")
climBI = calcClimLit(name="Bar_int", id=".3")
climXF = calcClimLit(name="Box_freq", id=".4")
climXI = calcClimLit(name="Box_int", id=".5")

concerned = c(climAF$tot, climAI$tot, climBF$tot, climBI$tot, climXF$tot, climXI$tot)

concernedGroup = ifelse(concerned <= -3, "Lower literacy", concerned)
concernedGroup = ifelse(concerned >= 3, "Higher literacy", concernedGroup)
concernedGroup = ifelse(concerned >= -2 & concerned <= 2, "Neutral", concernedGroup)

##########################################################################
# Political
##########################################################################
political = as.numeric(c(split.by.block$Area_Freq$DEM7_AF, split.by.block$Area_Int$DEM7_AI,
                         split.by.block$Bar_freq$DEM7_BF, split.by.block$Bar_int$DEM7_BI,
                         split.by.block$Box_freq$DEM7_XF, split.by.block$Box_int$DEM7_XI))

politicalGroup = ifelse(political <= 3, "Liberal", political)
politicalGroup = ifelse(political >= 7, "Conservative", politicalGroup)
politicalGroup = ifelse(political >= 4 & political <= 6, "Neutral", politicalGroup)

##########################################################################
# Age
##########################################################################
age = 2024 - as.numeric(c(split.by.block$Area_Freq$DEM3_AF, split.by.block$Area_Int$DEM3_AI, 
                          split.by.block$Bar_freq$DEM3_BF, split.by.block$Bar_int$DEM3_BI, 
                          split.by.block$Box_freq$DEM3_XF, split.by.block$Box_int$DEM3_XI))

ageGroup = ifelse(age <= 34, "18-34", age)
ageGroup = ifelse(age >= 55, "55+", ageGroup)
ageGroup = ifelse(age >= 35 & age <= 54, "35-54", ageGroup)

##########################################################################
# Location
##########################################################################
demStates = c(split.by.block$Area_Freq$DEM1_AF, split.by.block$Area_Int$DEM1_AI, 
              split.by.block$Bar_freq$DEM1_BF, split.by.block$Bar_int$DEM1_BI, 
              split.by.block$Box_freq$DEM1_XF, split.by.block$Box_int$DEM1_XI)

# https://en.wikipedia.org/wiki/List_of_regions_of_the_United_States
# U.S. Census Bureau–designated regions and divisions
division = demStates
# Region 1: Northeast (New England and Middle Atlantic)
division = ifelse(division == "Connecticut" | division == "Maine" | division == "Massachusetts" |
                    division == "New Hampshire" | division == "Rhode Island" | division == "Vermont", 
                  "New England", division)
division = ifelse(division == "New Jersey" | division == "New York" | division == "Pennsylvania", 
                  "Middle Atlantic", division)
# Region 2: Midwest (East North Central and West North Central)
division = ifelse(division == "Illinois" | division == "Indiana" | division == "Michigan" |
                    division == "Ohio" | division == "Wisconsin", "East North Central", division)
division = ifelse(division == "Iowa" | division == "Kansas" | division == "Minnesota" |
                    division == "Missouri" | division == "Nebraska" | division == "North Dakota" | 
                    division == "South Dakota", "West North Central", division)
# Region 3: South (South Atlantic, East South Central, and West South Central)
division = ifelse(division == "Delaware" | division == "District of Columbia" | 
                    division == "Florida" | division == "Georgia" | division == "Maryland" | 
                    division == "North Carolina" | division == "South Carolina" | 
                    division == "Virginia" | division == "West Virginia", "South Atlantic", division)
division = ifelse(division == "Alabama" | division == "Kentucky" | division == "Mississippi" |
                    division == "Tennessee", "East South Central", division)
division = ifelse(division == "Arkansas" | division == "Louisiana" | division == "Oklahoma" |
                    division == "Texas", "West South Central", division)
# Region 4: West (Mountain and Pacific)
division = ifelse(division == "Arizona" | division == "Colorado" | division == "Idaho" |
                    division == "Montana" | division == "Nevada" | division == "New Mexico" | 
                    division == "Utah" | division == "Wyoming", "Mountain", division)
division = ifelse(division == "Alaska" | division == "California" | division == "Hawaii" |
                    division == "Oregon" | division == "Washington", "Pacific", division)

region = division
# Region 1: Northeast (New England and Middle Atlantic)
region = ifelse(region == "New England" | region == "Middle Atlantic", "Northeast", region)
# Region 2: Midwest (East North Central and West North Central)
region = ifelse(region == "East North Central" | region == "West North Central", 
                "Midwest", region)
# Region 3: South (South Atlantic, East South Central, and West South Central)
region = ifelse(region == "South Atlantic" | region == "East South Central" | 
                  region == "West South Central", "South", region)
# Region 4: West (Mountain and Pacific)
region = ifelse(region == "Mountain" | region == "Pacific", "West", region)

##########################################################################
# Protective decisions
##########################################################################

# Extract the protective answers for each respondent
ptabAF = protectiveTable(name="Area_Freq", id="")
ptabAI = protectiveTable(name="Area_Int", id=".1")
ptabBF = protectiveTable(name="Bar_freq", id=".2")
ptabBI = protectiveTable(name="Bar_int", id=".3")
ptabXF = protectiveTable(name="Box_freq", id=".4")
ptabXI = protectiveTable(name="Box_int", id=".5")
ptab = rbind(ptabAF, ptabAI, ptabBF, ptabBI, ptabXF, ptabXI)
ptab$graph = c(rep("Area", nrow(ptabAF) + nrow(ptabAI)), 
               rep("Bar", nrow(ptabBF) + nrow(ptabBI)),
               rep("Box", nrow(ptabXF) + nrow(ptabXI)))
ptab$clim = concernedGroup
ptab$clim = factor(ptab$clim, levels = c("Lower literacy", "Neutral", 
                                         "Higher literacy"))
ptab$political = politicalGroup
ptab$political = factor(ptab$political, levels = c("Conservative", "Neutral", 
                                                   "Liberal"))
ptab$acc = accGroup
ptab$acc = factor(ptab$acc, levels = c("Below average", "Average", 
                                       "Above average"))

ptab$age = ageGroup
ptab$age = factor(ptab$age, levels = c("18-34", "35-54", "55+"))

ptab$region = region
ptab$region = factor(ptab$region, levels = c("West", "South", "Midwest", "Northeast"))
# - -----------------------------------------------------------------------

proAF = calcProtective(name="Area_Freq", id="")
proAI = calcProtective(name="Area_Int", id=".1")
proBF = calcProtective(name="Bar_freq", id=".2")
proBI = calcProtective(name="Bar_int", id=".3")
proXF = calcProtective(name="Box_freq", id=".4")
proXI = calcProtective(name="Box_int", id=".5")

protab = rbind(proAF, proAI, proBF, proBI, proXF, proXI)
protab$graph = c(rep("Area", nrow(proAF) + nrow(proAI)), 
                 rep("Bar", nrow(proBF) + nrow(proBI)),
                 rep("Box", nrow(proXF) + nrow(proXI)))
protab$CHAL1 = protab$CHAL1+1
protab$graph = factor(protab$graph, levels = c("Bar", "Box", "Area"))
protab$clim = concernedGroup
protab$clim = factor(protab$clim, levels = c("Lower literacy", "Neutral", 
                                             "Higher literacy"))
protab$political = politicalGroup
protab$political = factor(protab$political, levels = c("Conservative", "Neutral", 
                                                       "Liberal"))
protab$acc = accGroup
protab$acc = factor(protab$acc, levels = c("Below average", "Average", 
                                           "Above average"))

protab$age = ageGroup
protab$age = factor(protab$age, levels = c("18-34", "35-54", "55+"))

protab$region = region
protab$region = factor(protab$region, levels = c("West", "South", "Midwest", "Northeast"))

# Read in notes with primary and secondary codes ----------------------------------------
# Primary coding
prochoices = read.csv("data/protectiveTable_5june2024.csv")

# secondary coding
topics = read.csv("data/codebook.csv")

# Add groupings
prochoices$graph = prochoices$name
prochoices$graph = ifelse(prochoices$graph == "Area_Freq" | prochoices$graph == "Area_Int", 
                          "Area", prochoices$graph)
prochoices$graph = ifelse(prochoices$graph == "Bar_freq" | prochoices$graph == "Bar_int", 
                          "Bar", prochoices$graph)
prochoices$graph = ifelse(prochoices$graph == "Box_freq" | prochoices$graph == "Box_int", 
                          "Box", prochoices$graph)
prochoices$clim = concernedGroup
prochoices$clim = factor(prochoices$clim, levels = c("Lower literacy", "Neutral", 
                                                     "Higher literacy"))
prochoices$political = politicalGroup
prochoices$political = factor(prochoices$political, levels = c("Conservative", "Neutral", 
                                                               "Liberal"))
prochoices$acc = accGroup
prochoices$acc = factor(prochoices$acc, levels = c("Below average", "Average", 
                                                   "Above average"))

prochoices$age = ageGroup
prochoices$age = factor(prochoices$age, levels = c("18-34", "35-54", "55+"))

prochoices$region = region
prochoices$region = factor(prochoices$region, levels = c("West", "South", "Midwest", "Northeast"))

# Decisions ---------------------------------------------------------------
# create bar plot data
scen_dec = table(protab$CHAL1)
scen_dec = as.data.frame((scen_dec/sum(scen_dec))*100)
colnames(scen_dec) = c("Val", "Perc")
scen_dec$name = "Decision"
driveops = c("do nothing","protect 2 events/yr", "protect 4 events/yr",
             "pave the driveway\n(120 events)")

scen_decflood = table(protab$CHAL2)
scen_decflood = as.data.frame((scen_decflood/sum(scen_decflood))*100)
colnames(scen_decflood) = colnames(scen_dec)[1:2]
scen_decflood$name = "Decision"

driveCols = brewer.pal(nrow(scen_dec), "OrRd")

# Risk Perception 
likeops = data.frame(ans = c("exceptionally unlikely", 
                             "very unlikely",
                             "unlikely",
                             "about as likely as not",
                             "likely",
                             "very likely",
                             "virtually certain"), val = 1:7)
likCols = brewer.pal(nrow(likeops), "PuBu")

# Evaluate risk perception
ptab$CHAL1_likVal = likeops$val[match(ptab$CHAL1_lik, likeops$ans)]
ptab$CHAL2_likVal = likeops$val[match(ptab$CHAL2_lik, likeops$ans)]
scen_lik = table(ptab$CHAL1_likVal, useNA = "ifany")
scen_lik = as.data.frame((scen_lik/sum(scen_lik))*100)
colnames(scen_lik) = c("Val", "Perc")
scen_lik$name = "Risk perception"

scen_likflood = table(ptab$CHAL2_likVal, useNA = "ifany")
scen_likflood = as.data.frame((scen_likflood/sum(scen_likflood))*100)
colnames(scen_likflood) = colnames(scen_lik)[1:2]
scen_likflood$name = "Risk perception"

# Subjective confidence
confidence = data.frame(ans = c("not at all confident", "not very confident", 
                                "moderately confident",
                                "very confident", "extremely confident"), val = 1:5)
conCols = brewer.pal(nrow(confidence), "RdPu")

# Evaluate confidence
ptab$CHAL1_conVal = confidence$val[match(ptab$CHAL1_con, confidence$ans)]
ptab$CHAL2_conVal = confidence$val[match(ptab$CHAL2_con, confidence$ans)]
scen_con = table(ptab$CHAL1_conVal)
scen_con = as.data.frame((scen_con/sum(scen_con))*100)
colnames(scen_con) = c("Val", "Perc")
scen_con$name = "Confidence"

##########################################################################
# Demographics
##########################################################################
# Is there a correlation between accuracy and:
#   Income, Political preference, Employment, Education, Race, Age, Gender, 
#   Location (multiple linear regression model/ kruskal-Wallis?)
#   linear regression (38 tests = 1+ 1+ 6 + 6 +6 + 1 + 3+ 14)(PA)

# Income ------------------------------------------------------------------
# options(max.print=1164)
income = c(split.by.block$Area_Freq$DEM8_AF, split.by.block$Area_Int$DEM8_AI,
           split.by.block$Bar_freq$DEM8_BF, split.by.block$Bar_int$DEM8_BI,
           split.by.block$Box_freq$DEM8_XF, split.by.block$Box_int$DEM8_XI)

# income[grep("\\.00$", income)]
# grep("\\.00$", income)
# Remove trailing ".00" c(102, 218, 226, 446,745, 934)
income[c(102, 251, 259, 530, 892, 1116)] = gsub("\\.\\d{2}", "", income[c(102, 251, 259, 530, 892, 1116)])
income[990] = 62.5 #"50.00-75.00" # 808

# Reformat some answers
# c(64, 120, 349, 612, 683, 788, 826)
income[c(64, 120, 346, 433, 726, 830, 935, 1008)] = NA
income[419] = mean(c(56000, 69000)) # 56000/69000 335
income[1081] = mean(c(35000, 50000)) # 35-50000
income[546] = 50000 # 50k #462
income[650] = 1000 # 1000 yearly #536

# remove +
income = gsub("\\+", "", income)
# Remove ,
income = gsub(",", "", income)
# Remove .
income = gsub("\\.", "", income)
# Remove $
income = gsub("\\$", "", income)

income = as.numeric(income)

incGroup = ifelse(income <= 24999, "Less than 25,000", income)
incGroup = ifelse(income >= 150000, "150,000 or more", incGroup)
incGroup = ifelse(income >= 25000 & income <= 49999, "25,000-49,999", incGroup)
incGroup = ifelse(income >= 50000 & income <= 74999, "50,000-74,999", incGroup)
incGroup = ifelse(income >= 75000 & income <= 99999, "75,000-99,999", incGroup)
incGroup = ifelse(income >= 100000 & income <= 149999, "100,000-149,999", incGroup)

incGroup = factor(incGroup, levels = c("Less than 25,000", "25,000-49,999", "50,000-74,999",
                                       "75,000-99,999", "100,000-149,999", "150,000 or more"))

inc_3Group = ifelse(income <= 49999, "<$50k", income)
inc_3Group = ifelse(income >= 100000, "$100k+", inc_3Group)
inc_3Group = ifelse(income >= 50000 & income <= 99999, "$50k-$99k", inc_3Group)

inc_3Group = factor(inc_3Group , levels = c("<$50k", "$50k-$99k", "$100k+"))

# Gender ------------------------------------------------------------------
gender = c(split.by.block$Area_Freq$DEM2_AF, split.by.block$Area_Int$DEM2_AI, 
           split.by.block$Bar_freq$DEM2_BF, split.by.block$Bar_int$DEM2_BI, 
           split.by.block$Box_freq$DEM2_XF, split.by.block$Box_int$DEM2_XI)

# Race and Ethnicity ------------------------------------------------------
latino = c(split.by.block$Area_Freq$DEM4.1_AF., split.by.block$Area_Int$DEM4.1_AI., 
           split.by.block$Bar_freq$DEM4.1_BF., split.by.block$Bar_int$DEM4.1_BI., 
           split.by.block$Box_freq$DEM4.1_XF., split.by.block$Box_int$DEM4.1_XI.)

races_list = c(split.by.block$Area_Freq$DEM4.2_AF, split.by.block$Area_Int$DEM4.2_AI, 
               split.by.block$Bar_freq$DEM4.2_BF, split.by.block$Bar_int$DEM4.2_BI, 
               split.by.block$Box_freq$DEM4.2_XF, split.by.block$Box_int$DEM4.2_XI)

white = ifelse(grepl("White or Caucasian", races_list), 1, 0)

black = ifelse(grepl("Black or African American", races_list), 1, 0)

indian = ifelse(grepl("American Indian/Native American or Alaska Native", races_list), 1, 0)

asian = ifelse(grepl("Asian", races_list), 1, 0)

islander = ifelse(grepl("Native Hawaiian or Other Pacific Islander", races_list), 1, 0)

# Search for Other with no space on either side or a preceeding or trailing comma. This ensures
# "Other Pacific Islander" is not matched
other = ifelse(grepl("^Other$|,Other|Other,", races_list), 1, 0)

prefer = ifelse(grepl("Prefer not to say", races_list), 1, 0)

# Bridging Methods
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2831381/
# https://www.texas-air.org/assets/pdf/TAIR%20DAC%20Race%20Mapping-Bridging.pdf
# https://nces.ed.gov/pubs2008/rediguide/app_c2.asp
DEMrace<- data.frame("white" = white,
                     "black" = black,
                     "indian" = indian,
                     "asian" = asian,
                     "islander" = islander,
                     "other" = other,
                     "prefer" = prefer)

multirace = which(rowSums(DEMrace) > 1)

smallest = c("black", "indian", "black", rep("indian", 4), "black", rep("indian", 2), "islander",
             "indian", "other", rep("indian", 2), "black", "indian", "other", "islander", rep("indian", 2), 
             "islander", "black", "other", "black", "other", "indian", rep("asian", 3), rep("indian", 2), 
             "islander", "black", rep("indian", 2))

largest = c(rep("white", 5), rep("black", 2), rep("white", 11), "black", rep("white", 11), 
            "black", "white", "asian", rep("white", 3))

bridgerace = DEMrace
bridgerace_tot = ifelse(bridgerace$white == 1, "white", 0)
bridgerace_tot = ifelse(bridgerace_tot == 0 & bridgerace$black == 1, "black", bridgerace_tot)
bridgerace_tot = ifelse(bridgerace_tot == 0 & bridgerace$indian == 1, "indian", bridgerace_tot)
bridgerace_tot = ifelse(bridgerace_tot == 0 & bridgerace$asian == 1, "asian", bridgerace_tot)
bridgerace_tot = ifelse(bridgerace_tot == 0 & bridgerace$islander == 1, "islander", bridgerace_tot)
bridgerace_tot = ifelse(bridgerace_tot == 0 & bridgerace$other == 1, "other", bridgerace_tot)
bridgerace_tot = ifelse(bridgerace_tot == 0 & bridgerace$prefer == 1, "prefer", bridgerace_tot)

# Bridging Method: Whole assignment
# Person is assigned completely to one category
# largest group
race_largest = bridgerace_tot
race_largest[multirace] = largest

# smallest group
race_smallest = bridgerace_tot
race_smallest[multirace] = smallest

# Bridging Method: Fractional assignment
# Person assigned partially to each selected group
fracDemrace = DEMrace/rowSums(DEMrace)

# Employment --------------------------------------------------------------
work = c(split.by.block$Area_Freq$DEM6_AF, split.by.block$Area_Int$DEM6_AI, 
         split.by.block$Bar_freq$DEM6_BF, split.by.block$Bar_int$DEM6_BI, 
         split.by.block$Box_freq$DEM6_XF, split.by.block$Box_int$DEM6_XI)

# Education ---------------------------------------------------------------
edu = c(split.by.block$Area_Freq$DEM5_AF, split.by.block$Area_Int$DEM5_AI, 
        split.by.block$Bar_freq$DEM5_BF, split.by.block$Bar_int$DEM5_BI, 
        split.by.block$Box_freq$DEM5_XF, split.by.block$Box_int$DEM5_XI)

# Combine -----------------------------------------------------------------

DEMrespcensus<- data.frame("region"= region,
                           "division" = division,
                           "state"= demStates,
                           "gender"= gender,
                           "age"= age,
                           "latino" = latino,
                           "racesmall" = race_smallest,
                           "racelarge" = race_largest,
                           "edu"= edu,
                           "work"=work,
                           "politics"= political,
                           "money"=income,
                           "clim" = concerned,
                           "climGroup" = concernedGroup)

##############################################################################
all.size.df<- as.data.frame(cbind(overAcc, DEMrespcensus, protab[,1:2]))

# Base cases:
# Working full-time
# White
# High school diploma or GED
# Not latino
# Male
# Texas / South / South Atlantic
all.size.df$work = factor(all.size.df$work, levels = c("Working full-time", 
                                                       "Other", 
                                                       "Retired", "Student", 
                                                       "A homemaker or stay-at-home parent", 
                                                       "Unemployed and looking for work",
                                                       "Working part-time"))

all.size.df$racelarge = factor(all.size.df$racelarge, levels = c("white", "black", 
                                                                 "asian", "indian", 
                                                                 "islander", "other",
                                                                 "prefer"))

all.size.df$edu = factor(all.size.df$edu, levels = c("High school diploma or GED", 
                                                     "Some high school or less", 
                                                     "Some college, but no degree", 
                                                     "Associates or technical degree",
                                                     "Bachelor’s degree", 
                                                     "Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS etc.)",
                                                     "Prefer not to say"))

all.size.df$gender = factor(all.size.df$gender, levels = c("Male", 
                                                           "Female", 
                                                           "Non-binary / third gender", 
                                                           "Prefer to self-describe",
                                                           "Prefer not to say"))

all.size.df$name = factor(all.size.df$name, levels = c("Bar", "Box", "Area"))

all.size.df$edunum = factor(all.size.df$edu, 
                            labels = 1:6,
                            levels = c("Some high school or less",
                                       "High school diploma or GED", 
                                       "Some college, but no degree",
                                       "Associates or technical degree",
                                       "Bachelor’s degree",
                                       "Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS etc.)"))

allstates = unique(all.size.df$state)
baseStates = c("Texas", allstates[-which(allstates == "Texas")])
all.size.df$state = factor(all.size.df$state, levels = baseStates)

allregion = unique(all.size.df$region)
baseregion = c("South", allregion[-which(allregion == "South")])
all.size.df$region = factor(all.size.df$region, levels = baseregion)

alldivision = unique(all.size.df$division)
basedivision = c("South Atlantic", alldivision[-which(alldivision == "South Atlantic")])
all.size.df$division = factor(all.size.df$division, levels = basedivision)

all.size.df<- as.data.frame(cbind(all.size.df, ptab$CHAL1_likVal, ptab$CHAL2_likVal, ptab$CHAL1_conVal,
                                  ptab$CHAL2_conVal))

## Test assumptions
###########################################################################
# Test assumptions for LINEAR REGRESSION ----------------------------------
summary(all.size.df)

combined = all.size.df

# Rescale income to thousands (max is 700,000) while the rest are on the scale of 1 to 10.
combined$money = combined$money/1000 # in thousands

# Set EDU as a category
combined$educat = ifelse(combined$edu == "Prefer not to say", NA, as.character(combined$edu))

# There are NAs in money and edu. Remove the NA values from the dataset
combined_rm = combined[-which(is.na(combined$educat) | is.na(combined$money)), ]

combined_rm$educat = factor(combined_rm$educat, levels = c("High school diploma or GED", 
                                                                 "Some high school or less", 
                                                                 "Some college, but no degree", 
                                                                 "Associates or technical degree",
                                                                 "Bachelor’s degree", 
                                                                 "Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS etc.)"))

# Combine categories with small subgroup sizes
# Combine the first two categories on confidence (not at all confident and not very confident)
combined_rm$con1 = ifelse((combined_rm$`ptab$CHAL1_conVal` - 1) == 0, 1, (combined_rm$`ptab$CHAL1_conVal` - 1))
combined_rm$con2 = ifelse((combined_rm$`ptab$CHAL2_conVal` - 1) == 0, 1, (combined_rm$`ptab$CHAL2_conVal` - 1))

# Combine the first two categories on likelihood (exceptionally unlikely and very unlikely)
combined_rm$lik1 = ifelse((combined_rm$`ptab$CHAL1_likVal` - 1) == 0, 1, (combined_rm$`ptab$CHAL1_likVal` - 1))
combined_rm$lik2 = ifelse((combined_rm$`ptab$CHAL2_likVal` - 1) == 0, 1, (combined_rm$`ptab$CHAL2_likVal` - 1))

# # Combine all gender categories as "Other" except Male and Female
# combined$genderOther = ifelse((combined$gender != "Male" & combined$gender != "Female"), "Other", 
#                               as.character(combined$gender))
# combined$genderOther = factor(combined$genderOther, levels = c("Male", "Female", "Other"))

# Combine all gender categories as "Non-male" except Male
combined_rm$gender_collapsed <- ifelse(combined_rm$gender == "Male", "Male", "Non-male")
combined_rm$gender_collapsed = factor(combined_rm$gender_collapsed, ordered =FALSE)
combined_rm$gender_collapsed <- relevel(combined_rm$gender_collapsed, ref = "Male")

# Combine all race categories as "Other" except White, black, and asian
combined_rm$raceOther = ifelse((combined_rm$racelarge != "white" & combined_rm$racelarge != "black" & combined_rm$racelarge != "asian"), 
                            "Other", as.character(combined_rm$racelarge))
combined_rm$raceOther = factor(combined_rm$raceOther, levels = c("white", "black", "asian", "Other"))

# Covert data format into factors for running regressions
combined_rm$CHAL1 = factor(combined_rm$CHAL1, levels = 1:4, ordered=TRUE, labels = c("do nothing",
                                                                 "protect 2 events/yr", 
                                                                 "protect 4 events/yr",
                                                                 "pave the driveway\n(120 events)"))
combined_rm$CHAL2 = factor(combined_rm$CHAL2, ordered =TRUE, levels = 0:1)
combined_rm$con1 = factor(combined_rm$con1, ordered =TRUE, levels = 1:4)
combined_rm$con2 = factor(combined_rm$con2, ordered =TRUE, levels = 1:4)
combined_rm$lik1 = factor(combined_rm$lik1, ordered =TRUE, levels = 1:6)
combined_rm$lik2 = factor(combined_rm$lik2, ordered =TRUE, levels = 1:6)

# Scale these variables for consistency and Hessian conditioning
combined_rm$money_s    <- scale(combined_rm$money)    # 0-700, by far the worst
combined_rm$age_s      <- scale(combined_rm$age)      # 17-87
combined_rm$clim_s     <- scale(combined_rm$clim)     # -7 to 7
combined_rm$politics_s <- scale(combined_rm$politics) # 0-10
combined_rm$val_s     <- scale(combined_rm$val)     # 1 to 15

# Separate the dataset for the likelihood questions. The Likelihood questions were added 
# after the soft launch. 
all(which(is.na(combined_rm$lik1)) == which(is.na(combined_rm$lik2))) # confirm we are missing the same responses for both scenarios
combined_likelihood = combined_rm[-which(is.na(combined_rm$lik1)), ]

# REGRESSIONS #############################################################
# FIS protection measures (Binary yes or no) ------------------------------
# Logistic regression model using the glm (generalized linear model) function.
# First, we convert rank to a factor to indicate that rank should be treated as a categorical variable.

fis_PRO_logit <- glm(CHAL2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                 educat + work + politics_s + money_s + clim_s, data = combined_rm, family = "binomial")
# Check the result of our model
summary(fis_PRO_logit)
# write.csv(summary(fis_PRO_logit)[["coefficients"]], "paper2/SuppTab_FISprotection_LOGIT.csv")

# DWS protection measures (multinomial logistic regression) ---------------

# Set the outcome of "do nothing" as our baseline. Must be unordered
combined_rm$CHAL1_re <- factor(combined_rm$CHAL1, order=FALSE)
combined_rm$CHAL1_re <- relevel(combined_rm$CHAL1_re, ref = "do nothing")

# Make sure we use the nnet multinom() and not the mgcv version
dws_PRO_mlr <- nnet::multinom(CHAL1_re ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                   educat + work + politics_s + money_s + clim_s, data = combined_rm)
summary(dws_PRO_mlr)

# Check the Z-score for the model (wald Z)
z_dws_PRO_mlr <- summary(dws_PRO_mlr)$coefficients/summary(dws_PRO_mlr)$standard.errors

# 2-tailed z test
p_dws_PRO_mlr <- (1 - pnorm(abs(z_dws_PRO_mlr), 0, 1)) * 2

# extract the coefficients from the model and exponentiate
coef_dws_PRO_mlr <- coef(dws_PRO_mlr)
exp_dws_PRO_mlr <- exp(coef(dws_PRO_mlr))

# Print a clean version with tidy
tidy_dws_PRO_mlr <- tidy(dws_PRO_mlr, conf.int = TRUE)
write.csv(tidy_dws_PRO_mlr, row.names = TRUE, "paper2/SuppTab_DWSprotection_MLR.csv")
write.csv(exp_dws_PRO_mlr, row.names = TRUE, "paper2/SuppTab_DWSprotection_MLR_OddsRationExpcoefficient.csv")
write.csv(coef_dws_PRO_mlr, row.names = TRUE, "paper2/SuppTab_DWSprotection_MLR_coefficient.csv")
write.csv(p_dws_PRO_mlr, row.names = TRUE, "paper2/SuppTab_DWSprotection_MLR_pvalues.csv")
write.csv(summary(dws_PRO_mlr)$standard.errors, row.names = TRUE, "paper2/SuppTab_DWSprotection_MLR_sErrors.csv")

# Test the goodness of fit
# rmNA_combined = combined$CHAL1[-(which(is.na(combined$asedunum) | is.na(combined$money)))]
# chisq.test(rmNA_combined, predict(dws_PRO_mlr))

# library("DescTools")
# # Calculate the R Square
# PseudoR2(dws_PRO_mlr, which = c("CoxSnell","Nagelkerke","McFadden"))

# Use the lmtest package to run Likelihood Ratio Tests
# vars = c("val_s", "name", "region", "gender_collapse", "age_s", "latino", "raceOther",  
#       "educat", "work", "politics_s", "money_s", "clim_s")
# lik_ratios = lapply(X = vars, function(X){ lrtest(dws_PRO_mlr, X) })
# names(lik_ratios) = vars

# DWS protection measures (partial proportional odds model) ---------------
olr_dws_chal1 <- polr(CHAL1 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                       educat + work + politics_s + money_s + clim_s, data= combined_rm, Hess = TRUE)
# summary(olr_dws_chal1)
brant.test(olr_dws_chal1)

# Partial proportional odds in VGAM
# parallel = FALSE ~ al_s + name + age_s + raceOther + educat + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_chal1 <- vglm(CHAL1 ~ val_s + name + region + age_s + latino + 
                            raceOther + educat + work + politics_s + 
                            money_s + gender_collapsed + clim_s,
                          family = cumulative(parallel = FALSE ~ val_s + name +
                                                age_s + raceOther + educat + clim_s, 
                                              reverse = FALSE), data = combined_rm)
# Did not converge
# summary(vglm_partial_chal1)


df = combined_likelihood[, c("lik1", 'gender_collapsed')]
df$lik1 = as.numeric(df$lik1)
aggregate(lik1_str ~ gender_collapsed, data=df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))

table(df)

############################ LIKELIHOOD 1
# DWS likelihood (partial proportion odds model) --------------------------
olr_dws_lik1 <- polr(lik1 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                       educat + work + politics_s + money_s + clim_s,
                     data= combined_likelihood, Hess = TRUE)
brant.test(olr_dws_lik1)


# Partial proportional odds in VGAM
# educat moderately violates the brant test for graduate degree, but passes for the rest
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
# vglm_partial_lik1 <- vglm(lik1_str ~ val_s + name + region + age_s + latino + 
#                             raceOther + educat + work + politics_s + 
#                             money_s + gender_collapsed + clim_s,
#                           family = cumulative(parallel = FALSE ~ name + gender_collapsed + clim_s, reverse = FALSE),
#                           data = combined_likelihood)

vglm_partial_lik1 <- vglm(lik1 ~ val_s + name + region + age_s + latino + 
                            raceOther + educat + work + politics_s + 
                            money_s + gender_collapsed + clim_s,
                          family = cumulative(parallel = FALSE ~ name + gender_collapsed + clim_s, reverse = TRUE),
                          data = combined_likelihood)

summaryvglm(vglm_partial_lik1)

# Check what VGAM thinks the threshold ordering is
vglm_partial_lik1@misc$ynames

# Check the intercepts are ascending
intercepts <- coef(vglm_partial_lik1)[grep("Intercept", names(coef(vglm_partial_lik1)))]
print(round(intercepts, 3))
cat("Ascending:", all(diff(intercepts) > 0), "\n")

# Also check fitted probabilities for a few rows
# Column 1 should be P(lowest category)
head(fitted(vglm_partial_lik1))

# And verify factor level ordering
levels(combined_likelihood$lik1)

# summary(vglm_partial_lik1)
# Check that the log likelihood is a finite value
vglm_partial_lik1@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_lik1))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_lik1)))

# Confirm log-likelihood is stable and the model is identifiable
logLik(vglm_partial_lik1)

# AIC and BIC for model comparison later
AIC(vglm_partial_lik1)
BIC(vglm_partial_lik1)

# vglm_base_lik1 <- vglm(lik1 ~ val_s + name + region + age_s + latino + 
#                          raceOther + educat + work + politics_s + 
#                          money_s + gender_collapsed + clim_s,
#                        family = cumulative(parallel = TRUE, reverse = FALSE),
#                        data = combined_likelihood)
vglm_base_lik1 <- vglm(lik1 ~ val_s + name + region + age_s + latino + 
                         raceOther + educat + work + politics_s + 
                         money_s + gender_collapsed + clim_s,
                       family = cumulative(parallel = TRUE, reverse = TRUE),
                       data = combined_likelihood)

# This LRT should be significant — confirming the partial 
# model improves fit over standard OLR
lrtest(vglm_base_lik1, vglm_partial_lik1)

# If warnings(), then the model does not converge
vglm_uncon_lik1 <- vglm(lik1 ~ val_s + name + region + age_s + latino + 
                          raceOther + educat + work + politics_s + 
                          money_s + gender_collapsed + clim_s,
                        family = cumulative(parallel = FALSE, reverse = TRUE), #reverse = FALSE),
                        data = combined_likelihood)

# # This LRT should NOT be significant — confirming the constrained 
# # predictors don't need relaxing
# lrtest(vglm_partial_con1, vglm_uncon_con1)
# 
# # Also compare AIC/BIC across all three models
# AIC(vglm_base_con1, vglm_partial_con1, vglm_uncon_con1)
# BIC(vglm_base_con1, vglm_partial_con1, vglm_uncon_con1)

# A lower value for partial confirms it is the best fitting parsimonious model
AIC(vglm_base_lik1) - AIC(vglm_partial_lik1) # AIC partial is lower
BIC(vglm_base_lik1) - BIC(vglm_partial_lik1) # BIC base is lower

# ── Goodness of fit: Hosmer-Lemeshow at each cumulative threshold ──────────

# Hosmer-Lemeshow test at each cumulative threshold
# Non-significant p (> 0.05) indicates adequate calibration at that threshold
HL_goodfit(combined_likelihood$lik1, vglm_partial_lik1)

# Save results
summaryvglm(vglm_partial_lik1)
write.csv(summaryvglm(vglm_partial_lik1)@coef3, "paper2/SuppTab_Reverse_DWSLikelihood_partialPPO.csv")

# DWS confidence (partial proportion odds model) --------------------------
olr_dws_con1 <- polr(con1 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                       educat + work + politics_s + money_s + clim_s,
                     data= combined_rm, Hess = TRUE)
brant.test(olr_dws_con1)

# Partial proportional odds in VGAM
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_con1 <- vglm(con1 ~ val_s + name + region + age_s + latino + 
                            raceOther + educat + work + politics_s + 
                            money_s + gender_collapsed + clim_s,
                          family = cumulative(parallel = FALSE ~ val_s, reverse = TRUE),#reverse = FALSE),
                          data = combined_rm)

# summary(vglm_partial_con1)

# Check that the log likelihood is a finite value
vglm_partial_con1@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_con1))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_con1)))

# Confirm log-likelihood is stable and the model is identifiable
logLik(vglm_partial_con1)

# AIC and BIC for model comparison later
AIC(vglm_partial_con1)
BIC(vglm_partial_con1)

vglm_base_con1 <- vglm(con1 ~ val_s + name + region + age_s + latino + 
                         raceOther + educat + work + politics_s + 
                         money_s + gender_collapsed + clim_s,
                       family = cumulative(parallel = TRUE, reverse = TRUE),# reverse = FALSE),
                       data = combined_rm)

# This LRT should be significant — confirming the partial 
# model improves fit over standard OLR
lrtest(vglm_base_con1, vglm_partial_con1)

# If warnings(), then the model does not converge
vglm_uncon_con1 <- vglm(con1 ~ val_s + name + region + age_s + latino + 
                          raceOther + educat + work + politics_s + 
                          money_s + gender_collapsed + clim_s,
                        family = cumulative(parallel = FALSE, reverse = TRUE),# reverse = FALSE),
                        data = combined_rm)

# # This LRT should NOT be significant — confirming the constrained 
# # predictors don't need relaxing
# lrtest(vglm_partial_con1, vglm_uncon_con1)
# 
# # Also compare AIC/BIC across all three models
# AIC(vglm_base_con1, vglm_partial_con1, vglm_uncon_con1)
# BIC(vglm_base_con1, vglm_partial_con1, vglm_uncon_con1)

# A lower value for partial confirms it is the best fitting parsimonious model
AIC(vglm_base_con1) - AIC(vglm_partial_con1) # AIC is lower for partial
BIC(vglm_base_con1) - BIC(vglm_partial_con1) # BIC is lower for partial

# ── Goodness of fit: Hosmer-Lemeshow at each cumulative threshold ──────────

# Hosmer-Lemeshow test at each cumulative threshold
# Non-significant p (> 0.05) indicates adequate calibration at that threshold
HL_goodfit(combined_rm$con1, vglm_partial_con1)

# Save results
summaryvglm(vglm_partial_con1)
write.csv(summaryvglm(vglm_partial_con1)@coef3, "paper2/SuppTab_Reverse_DWSConfidence_partialPPO.csv")

# FIS likelihood (ordinal logistic model) --------------------------
olr_fis_lik2 <- polr(lik2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                       educat + work + politics_s + money_s + clim_s,
                     data= combined_likelihood, Hess = TRUE)
brant.test(olr_fis_lik2)

## store table
lik2_table <- coef(summary(olr_fis_lik2))

## calculate and store p values
lik2_p <- pnorm(abs(lik2_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
lik2_table <- cbind(lik2_table, "p value" = lik2_p)
# write.csv(lik2_table, "paper2/SuppTab_FISLikelihood_OLR.csv")

# exp(coef(olr_fis_lik2))
# 
# coef(olr_fis_lik2)
# exp(-coef(olr_fis_lik2))

# Partial proportional odds in VGAM
# Student in work only marginal violates it and has small numbers across the 
# parallel = FALSE ~ name + genderOther + work means those get 
# separate slopes; everything else is constrained
vglm_partial_lik2 <- vglm(lik2 ~ val_s + name + region + age_s + latino + 
                            raceOther + educat + work + politics_s + 
                            money_s + gender_collapsed + clim_s,
                          family = cumulative(parallel = FALSE ~ val_s + gender_collapsed, reverse = TRUE),#reverse = FALSE),
                          data = combined_likelihood)

# summary(vglm_partial_lik2)
# Check that the log likelihood is a finite value
vglm_partial_lik2@criterion$loglikelihood

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_lik2))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_lik2)))

# Confirm log-likelihood is stable and the model is identifiable
logLik(vglm_partial_lik2)

# AIC and BIC for model comparison later
AIC(vglm_partial_lik2)
BIC(vglm_partial_lik2)

vglm_base_lik2 <- vglm(lik2 ~ val_s + name + region + age_s + latino +
                         raceOther + educat + work + politics_s +
                         money_s + gender_collapsed + clim_s,
                       family = cumulative(parallel = TRUE, reverse = TRUE), #reverse = FALSE),
                       data = combined_likelihood)

# These should be identical (or within rounding error)
logLik(vglm_base_lik2)
logLik(olr_fis_lik2)  # your original polr mode

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_base_lik2))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_base_lik2)))

# This LRT should be significant — confirming the partial
# model improves fit over standard OLR
lrtest(vglm_base_lik2, vglm_partial_lik2)

# If warnings(), then the model does not converge
vglm_uncon_lik2 <- vglm(lik2 ~ val_s + name + region + age_s + latino +
                          raceOther + educat + work + politics_s +
                          money_s + gender_collapsed + clim_s,
                        family = cumulative(parallel = FALSE, reverse = TRUE),#reverse = FALSE),
                        data = combined_likelihood)

# table(combined_likelihood$lik2)

# # This LRT should NOT be significant — confirming the constrained
# # predictors don't need relaxing
# lrtest(vglm_partial_con1, vglm_uncon_con1)
#
# # Also compare AIC/BIC across all three models
# AIC(vglm_base_con1, vglm_partial_con1, vglm_uncon_con1)
# BIC(vglm_base_con1, vglm_partial_con1, vglm_uncon_con1)

# A lower value for partial confirms it is the best fitting parsimonious model
AIC(vglm_base_lik2) - AIC(vglm_partial_lik2) # AIC is lower for partial
BIC(vglm_base_lik2) - BIC(vglm_partial_lik2) # BIC is lower for base

# ── Goodness of fit: Hosmer-Lemeshow at each cumulative threshold ──────────

# Hosmer-Lemeshow test at each cumulative threshold
# Non-significant p (> 0.05) indicates adequate calibration at that threshold
HL_goodfit(combined_likelihood$lik2, vglm_base_lik2)

# Look at calibration at threshold 1
obs_used   <- as.numeric(combined_likelihood$lik2)
pred_probs <- fitted(vglm_base_lik2)
pred_cum1  <- pred_probs[, 1]  # P(Y = lowest category) with reverse=TRUE
binary_obs1 <- as.integer(obs_used <= 1)

# Check calibration by decile
deciles <- cut(pred_cum1,
               breaks = quantile(pred_cum1, probs = seq(0, 1, 0.1)),
               include.lowest = TRUE)

calib_check <- data.frame(
  predicted = tapply(pred_cum1, deciles, mean),
  observed  = tapply(binary_obs1, deciles, mean),
  n         = tapply(binary_obs1, deciles, length)
)
print(round(calib_check, 3))

# Plot
# png(file="paper2/Supp_lik2CalibrationThres.png", family="Helvetica", width=med23_column, 
#     height=column_height*2, pointsize=12, res=300)
# par(mfrow=c(1,1), mar=c(1,1,1,1))
# par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(0.5,1.2,2,0))
plot(calib_check$predicted, calib_check$observed,
     xlim = c(0, 0.3), ylim = c(0, 0.3),
     xlab = "Mean predicted probability",
     ylab = "Observed proportion",
     main = "Calibration at threshold 1 (vs. exceptionally to very unlikely)")
abline(0, 1, col = "red", lty = 2)
# dev.off()

# Refit predictions from polr for comparison
pred_polr <- predict(olr_fis_lik2, type = "probs")
pred_cum1_polr <- pred_polr[, 1]  # lowest category

# Quick correlation check — should be near identical
cor(pred_cum1, pred_cum1_polr)

# Save results
summaryvglm(vglm_base_lik2)
# write.csv(summaryvglm(vglm_partial_lik2)@coef3, "paper2/SuppTab_FISLikelihood_partialPPO.csv")
write.csv(summaryvglm(vglm_base_lik2)@coef3, "paper2/SuppTab_Reverse_FISLikelihood_OLR.csv")


# FIS confidence (partial proportion odds model) --------------------------
olr_dws_con2 <- polr(con2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                       educat + work + politics_s + money_s + clim_s,
                     data= combined_rm, Hess = TRUE)
brant.test(olr_dws_con2)

## store table
con2_table <- coef(summary(olr_dws_con2))

## calculate and store p values
con2_p <- pnorm(abs(con2_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
con2_table <- cbind(con2_table, "p value" = con2_p)
# write.csv(con2_table, "paper2/SuppTab_FISConfidence_OLR.csv")

# clim, pol, latino, gender non-male is significant p-values
# Omnibus passes, but gender marginally violates.

# Partial proportional odds in VGAM
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_con2 <- vglm(con2 ~ val_s + name + region + age_s + latino + 
                            raceOther + educat + work + politics_s + 
                            money_s + gender_collapsed + clim_s,
                          family = cumulative(parallel = FALSE ~ gender_collapsed, reverse = TRUE), #reverse = FALSE),
                          data = combined_rm)

# summary(vglm_partial_con2)

# Check that the log likelihood is a finite value
vglm_partial_con2@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_con2))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_con2)))

# Confirm log-likelihood is stable and the model is identifiable
logLik(vglm_partial_con2)

# AIC and BIC for model comparison later
AIC(vglm_partial_con2)
BIC(vglm_partial_con2)

vglm_base_con2 <- vglm(con2 ~ val_s + name + region + age_s + latino + 
                         raceOther + educat + work + politics_s + 
                         money_s + gender_collapsed + clim_s,
                       family = cumulative(parallel = TRUE, reverse = TRUE), # reverse = FALSE),
                       data = combined_rm)

logLik(vglm_base_con2)
logLik(olr_dws_con2)  # your original polr mode

# This LRT should be significant — confirming the partial 
# model improves fit over standard OLR
lrtest(vglm_base_con2, vglm_partial_con2)

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_base_con2))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_base_con2)))

# If warnings(), then the model does not converge
vglm_uncon_con2 <- vglm(con2 ~ val_s + name + region + age_s + latino + 
                          raceOther + educat + work + politics_s + 
                          money_s + gender_collapsed + clim_s,
                        family = cumulative(parallel = FALSE, reverse = TRUE), #reverse = FALSE),
                        data = combined_rm)

# This LRT should NOT be significant — confirming the constrained
# predictors don't need relaxing
lrtest(vglm_partial_con2, vglm_uncon_con2)
# 
# # Also compare AIC/BIC across all three models
# AIC(vglm_base_con1, vglm_partial_con1, vglm_uncon_con1)
# BIC(vglm_base_con1, vglm_partial_con1, vglm_uncon_con1)

# A lower value for partial confirms it is the best fitting parsimonious model
AIC(vglm_base_con2) - AIC(vglm_partial_con2) # AIC is lower for partial
BIC(vglm_base_con2) - BIC(vglm_partial_con2) # BIC is lower for base
AIC(vglm_uncon_con2) - AIC(vglm_partial_con2) # AIC is lower for partial
BIC(vglm_uncon_con2) - BIC(vglm_base_con2) # BIC is lower for base
AIC(vglm_base_con2); AIC(vglm_partial_con2); AIC(vglm_uncon_con2)
BIC(vglm_base_con2); BIC(vglm_partial_con2); BIC(vglm_uncon_con2)
# ── Goodness of fit: Hosmer-Lemeshow at each cumulative threshold ──────────

# Hosmer-Lemeshow test at each cumulative threshold
# Non-significant p (> 0.05) indicates adequate calibration at that threshold
HL_goodfit(combined_rm$con2, vglm_base_con2)

# Save results
summaryvglm(vglm_base_con2)
# write.csv(summaryvglm(vglm_partial_con2)@coef3, "paper2/SuppTab_FISConfidence_partialPPO.csv")
write.csv(summaryvglm(vglm_base_con2)@coef3, "paper2/SuppTab_Reverse_FISConfidence_OLR.csv")

# REGRESSIONS END #########################################################
# Testing

# There are NAs in money and edu. Remove the NA values from the dataset
rm_mon_edu = which(is.na(combined$educat) | is.na(combined$money))
incGroup_rm = incGroup[-rm_mon_edu]
incCat_rm = inc_3Group[-rm_mon_edu]
accGroup_rm = accGroup[-rm_mon_edu]
concernedGroup_rm = concernedGroup[-rm_mon_edu]
politicalGroup_rm = politicalGroup[-rm_mon_edu]
ageGroup_rm = ageGroup[-rm_mon_edu]

incGroup_rm = factor(incGroup_rm, level=c("Less than 25,000", "25,000-49,999", "50,000-74,999",
                     "75,000-99,999", "100,000-149,999", "150,000 or more"))
accGroup_rm = factor(accGroup_rm, level=c("Below average", "Average", "Above average"))
politicalGroup_rm = factor(politicalGroup_rm, level=c("Liberal", "Neutral", "Conservative"))
ageGroup_rm = factor(ageGroup_rm, level=c("18-34", "35-54", "55+"))
concernedGroup_rm = factor(concernedGroup_rm, level=c("Lower literacy", "Neutral", "Higher literacy"))


combined_rm <- as.data.frame(cbind(combined_rm, incGroup_rm, accGroup_rm, concernedGroup_rm, 
                                   politicalGroup_rm, ageGroup_rm))

combined_rm$inc_cat = incCat_rm

all(which(is.na(combined_rm$lik1)) == which(is.na(combined_rm$lik2))) # confirm we are missing the same responses for both scenarios
combined_likelihood_cat = combined_rm[-which(is.na(combined_rm$lik1)), ]

# Vary the continuous groups as categories -------------------------------------
# Income
olr_con1_inc <- polr(con1 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                       educat + work + politics_s + inc_cat + clim_s,
                     data= combined_rm, Hess = TRUE)
brant.test(olr_con1_inc) # Val and incGroup varies: 75,000-99,999

# Partial proportional odds in VGAM
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_inc <- vglm(con1 ~ val_s + name + region + age_s + latino + 
                            raceOther + educat + work + politics_s + 
                           inc_cat + gender_collapsed + clim_s,
                          family = cumulative(parallel = FALSE ~ val_s, reverse = TRUE), #reverse = FALSE),
                          data = combined_rm)

# Check that the log likelihood is a finite value
vglm_partial_inc@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_inc))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_inc)))

# AIC and BIC for model comparison later
AIC(vglm_partial_con1) - AIC(vglm_partial_inc)# AIC is lower for income
BIC(vglm_partial_con1) - BIC(vglm_partial_inc)# BIC is lower for partial

# Hosmer-Lemeshow test at each cumulative threshold
# Non-significant p (> 0.05) indicates adequate calibration at that threshold
HL_goodfit(combined_rm$con1, vglm_partial_inc) # All above 0.05

summaryvglm(vglm_partial_inc)
# write.csv(summaryvglm(vglm_partial_inc)@coef3, "paper2/SuppTab_FISConfidence1_incomeCat.csv")

# Accuracy
olr_con1_acc <- polr(con1 ~ accGroup_rm + name + region + gender_collapsed + age_s + latino + raceOther + 
                       educat + work + politics_s + money_s + clim_s,
                     data= combined_rm, Hess = TRUE)
brant.test(olr_con1_acc) # Val and incGroup varies: 75,000-99,999

# Partial proportional odds in VGAM
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_acc <- vglm(con1 ~ accGroup_rm + name + region + age_s + latino + 
                           raceOther + educat + work + politics_s + 
                           money_s + gender_collapsed + clim_s,
                         family = cumulative(parallel = FALSE ~ accGroup_rm, reverse = TRUE), #reverse = FALSE),
                         data = combined_rm)

# Check that the log likelihood is a finite value
vglm_partial_acc@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_acc))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_acc)))

# Confirm log-likelihood is stable and the model is identifiable
logLik(vglm_partial_acc)

# AIC and BIC for model comparison later
AIC(vglm_partial_con1) - AIC(vglm_partial_acc)# AIC is lower for partial
BIC(vglm_partial_con1) - BIC(vglm_partial_acc)# BIC is lower for partial

# Hosmer-Lemeshow test at each cumulative threshold
# Non-significant p (> 0.05) indicates adequate calibration at that threshold
HL_goodfit(combined_rm$con1, vglm_partial_acc) # All above 0.05

# Climate
olr_con1_clim <- polr(con1 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                       educat + work + politics_s + money_s + concernedGroup_rm,
                     data= combined_rm, Hess = TRUE)
brant.test(olr_con1_clim) # Val and incGroup varies: 75,000-99,999

# Partial proportional odds in VGAM
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_clim <- vglm(con1 ~ val_s + name + region + age_s + latino + 
                           raceOther + educat + work + politics_s + 
                           money_s + gender_collapsed + concernedGroup_rm,
                         family = cumulative(parallel = FALSE ~ val_s, reverse = TRUE),
                         data = combined_rm)

# Check that the log likelihood is a finite value
vglm_partial_clim@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_clim))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_clim)))

# AIC and BIC for model comparison later
AIC(vglm_partial_con1) - AIC(vglm_partial_clim)# AIC is lower for partial
BIC(vglm_partial_con1) - BIC(vglm_partial_clim)# BIC is lower for partial

# Hosmer-Lemeshow test at each cumulative threshold
# Non-significant p (> 0.05) indicates adequate calibration at that threshold
HL_goodfit(combined_rm$con1, vglm_partial_clim) # All above 0.05

# Politics
olr_con1_pol <- polr(con1 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                        educat + work + politicalGroup_rm + money_s + clim_s,
                      data= combined_rm, Hess = TRUE)
brant.test(olr_con1_pol) # Val and incGroup varies: 75,000-99,999

#combined_rm$politicalGroup_rmMod = factor(combined_rm$politicalGroup_rm, 
#levels=c("Neutral", "Liberal", "Conservative"))

# Partial proportional odds in VGAM
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_pol <- vglm(con1 ~ val_s + name + region + age_s + latino + 
                            raceOther + educat + work + politicalGroup_rm + 
                            money_s + gender_collapsed + clim_s,
                          family = cumulative(parallel = FALSE ~ val_s, reverse = TRUE),
                          data = combined_rm)

# Check that the log likelihood is a finite value
vglm_partial_pol@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_pol))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_pol)))

# AIC and BIC for model comparison later
AIC(vglm_partial_con1) - AIC(vglm_partial_pol)# AIC is lower for politics
BIC(vglm_partial_con1) - BIC(vglm_partial_pol)# BIC is lower for politics

# Hosmer-Lemeshow test at each cumulative threshold
# Non-significant p (> 0.05) indicates adequate calibration at that threshold
HL_goodfit(combined_rm$con1, vglm_partial_pol) # All above 0.05

summaryvglm(vglm_partial_pol)
write.csv(summaryvglm(vglm_partial_pol)@coef3, "paper2/SuppTab_Reverse_DWSConfidence1_pol.csv")

# Age
olr_con1_age <- polr(con1 ~ val_s + name + region + gender_collapsed + ageGroup_rm + latino + raceOther + 
                       educat + work + politics_s + money_s + clim_s,
                     data= combined_rm, Hess = TRUE)
brant.test(olr_con1_age) # Val and incGroup varies: 75,000-99,999

# Partial proportional odds in VGAM
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_age <- vglm(con1 ~ val_s + name + region + ageGroup_rm + latino + 
                           raceOther + educat + work + politics_s + 
                           money_s + gender_collapsed + clim_s,
                         family = cumulative(parallel = FALSE ~ val_s, reverse = TRUE),
                         data = combined_rm)

# Check that the log likelihood is a finite value
vglm_partial_age@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_age))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_age)))

# AIC and BIC for model comparison later
AIC(vglm_partial_con1) - AIC(vglm_partial_age)# AIC is lower for partial
BIC(vglm_partial_con1) - BIC(vglm_partial_age)# BIC is lower for partial

# Hosmer-Lemeshow test at each cumulative threshold
# Non-significant p (> 0.05) indicates adequate calibration at that threshold
HL_goodfit(combined_rm$con1, vglm_partial_age) # All above 0.05

summaryvglm(vglm_partial_age)

# Likelihood #1 -----------------------------------------------------------

# Income
olr_dws_lik1_inc <- polr(lik1 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                       educat + work + politics_s + inc_cat + clim_s,
                     data= combined_likelihood_cat, Hess = TRUE)
brant.test(olr_dws_lik1_inc)

# Partial proportional odds in VGAM
# educat moderately violates the brant test for graduate degree, but passes for the rest
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_lik1_inc <- vglm(lik1 ~ val_s + name + region + age_s + latino + 
                            raceOther + educat + work + politics_s + 
                              inc_cat + gender_collapsed + clim_s,
                          family = cumulative(parallel = FALSE ~ name + gender_collapsed + clim_s, reverse = TRUE),
                          data = combined_likelihood_cat)

# summary(vglm_partial_lik1)
# Check that the log likelihood is a finite value
vglm_partial_lik1_inc@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_lik1_inc))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_lik1_inc)))

# Confirm log-likelihood is stable and the model is identifiable
# logLik(vglm_partial_lik1_inc)

# AIC and BIC for model comparison later
# A lower value for partial confirms it is the best fitting parsimonious model
AIC(vglm_partial_lik1); AIC(vglm_partial_lik1_inc) # AIC partial is lower
BIC(vglm_partial_lik1); BIC(vglm_partial_lik1_inc) # BIC partial is lower

# ── Goodness of fit: Hosmer-Lemeshow at each cumulative threshold ──────────

# Hosmer-Lemeshow test at each cumulative threshold
# Non-significant p (> 0.05) indicates adequate calibration at that threshold
HL_goodfit(combined_likelihood_cat$lik1, vglm_partial_lik1_inc)

# Accuracy
olr_dws_lik1_acc <- polr(lik1 ~ accGroup_rm + name + region + gender_collapsed + age_s + latino + raceOther + 
                           educat + work + politics_s + money_s + clim_s,
                         data= combined_likelihood_cat, Hess = TRUE)
brant.test(olr_dws_lik1_acc)

# Partial proportional odds in VGAM
# educat moderately violates the brant test for graduate degree, but passes for the rest
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_lik1_acc <- vglm(lik1 ~ accGroup_rm + name + region + age_s + latino + 
                                raceOther + educat + work + politics_s + 
                                money_s + gender_collapsed + clim_s,
                              family = cumulative(parallel = FALSE ~ name + gender_collapsed + clim_s, reverse = TRUE),
                              data = combined_likelihood_cat)

# summary(vglm_partial_lik1)
# Check that the log likelihood is a finite value
vglm_partial_lik1_acc@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_lik1_acc))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_lik1_acc)))

# AIC and BIC for model comparison later
# A lower value for partial confirms it is the best fitting parsimonious model
AIC(vglm_partial_lik1); AIC(vglm_partial_lik1_acc) # AIC partial is lower
BIC(vglm_partial_lik1); BIC(vglm_partial_lik1_acc) # BIC base is lower

# ── Goodness of fit: Hosmer-Lemeshow at each cumulative threshold ──────────

# Hosmer-Lemeshow test at each cumulative threshold
# Non-significant p (> 0.05) indicates adequate calibration at that threshold
HL_goodfit(combined_likelihood_cat$lik1, vglm_partial_lik1_acc)

# Climate
olr_dws_lik1_clim <- polr(lik1 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                           educat + work + politics_s + money_s + concernedGroup_rm,
                         data= combined_likelihood_cat, Hess = TRUE)
brant.test(olr_dws_lik1_clim)

# Partial proportional odds in VGAM
# educat moderately violates the brant test for graduate degree, but passes for the rest
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_lik1_clim <- vglm(lik1 ~ accGroup_rm + name + region + age_s + latino + 
                                raceOther + educat + work + politics_s + 
                                money_s + gender_collapsed + concernedGroup_rm,
                              family = cumulative(parallel = FALSE ~ name + gender_collapsed + concernedGroup_rm, reverse = TRUE),
                              data = combined_likelihood_cat)

# summary(vglm_partial_lik1)
# Check that the log likelihood is a finite value
vglm_partial_lik1_clim@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_lik1_clim))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_lik1_clim)))

# AIC and BIC for model comparison later
# A lower value for partial confirms it is the best fitting parsimonious model
AIC(vglm_partial_lik1); AIC(vglm_partial_lik1_clim) # AIC partial is lower
BIC(vglm_partial_lik1); BIC(vglm_partial_lik1_clim) # BIC partial is lower

# Politics
olr_dws_lik1_pol <- polr(lik1 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                            educat + work + politicalGroup_rm + money_s + clim_s,
                          data= combined_likelihood_cat, Hess = TRUE)
brant.test(olr_dws_lik1_pol)

# Partial proportional odds in VGAM
# educat moderately violates the brant test for graduate degree, but passes for the rest
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_lik1_pol <- vglm(lik1 ~ val_s + name + region + age_s + latino + 
                                 raceOther + educat + work + politicalGroup_rm + 
                                 money_s + gender_collapsed + clim_s,
                               family = cumulative(parallel = FALSE ~ name + gender_collapsed + clim_s, reverse = TRUE),
                               data = combined_likelihood_cat)

# summary(vglm_partial_lik1)
# Check that the log likelihood is a finite value
vglm_partial_lik1_pol@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_lik1_pol))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_lik1_pol)))

# AIC and BIC for model comparison later
# A lower value for partial confirms it is the best fitting parsimonious model
AIC(vglm_partial_lik1); AIC(vglm_partial_lik1_pol) # AIC political is lower
BIC(vglm_partial_lik1); BIC(vglm_partial_lik1_pol) # BIC political is lower

summaryvglm(vglm_partial_lik1_pol)
write.csv(summaryvglm(vglm_partial_lik1_pol)@coef3, "paper2/SuppTab_reverse_DWSLikelihood1_pol.csv")

# Age
olr_dws_lik1_age <- polr(lik1 ~ val_s + name + region + gender_collapsed + ageGroup_rm + latino + raceOther + 
                           educat + work + politics_s + money_s + clim_s,
                         data= combined_likelihood_cat, Hess = TRUE)
brant.test(olr_dws_lik1_age)

## store table
lik1_age_table <- coef(summary(olr_dws_lik1_age))

## calculate and store p values
lik1_p_age <- pnorm(abs(lik1_age_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
lik1_age_table <- cbind(lik1_age_table, "p value" = lik1_p_age)
# write.csv(lik1_age_table, "paper2/SuppTab_DWSLikelihood_OLR_age.csv")

# Partial proportional odds in VGAM
# educat moderately violates the brant test for graduate degree, but passes for the rest
# parallel = FALSE ~ name + genderOther + clim_s means those get 
# separate slopes; everything else is constrained
vglm_partial_lik1_age <- vglm(lik1 ~ val_s + name + region + ageGroup_rm + latino + 
                                raceOther + educat + work + politics_s + 
                                money_s + gender_collapsed + clim_s,
                              family = cumulative(parallel = FALSE ~ name + gender_collapsed + clim_s, reverse = TRUE),
                              data = combined_likelihood_cat)

# summary(vglm_partial_lik1)
# Check that the log likelihood is a finite value
vglm_partial_lik1_age@criterion$loglikelihood 

# Check that coefficients are finite and not wildly large
# Very large coefficients (>10) suggest separation issues
max(coef(vglm_partial_lik1_age))

# Check standard errors — Inf or NA values indicate separation
# or estimation failure for specific parameters
sqrt(diag(vcov(vglm_partial_lik1_age)))

# AIC and BIC for model comparison later
# A lower value for partial confirms it is the best fitting parsimonious model
AIC(vglm_partial_lik1); AIC(vglm_partial_lik1_age) # AIC partial is lower
BIC(vglm_partial_lik1); BIC(vglm_partial_lik1_age) # BIC partial is lower

summaryvglm(vglm_partial_lik1_age)
# write.csv(summaryvglm(vglm_partial_lik1_pol)@coef3, "paper2/SuppTab_FISLikelihood1_polCat.csv")

# Likelihood #2 -----------------------------------------------------------

# Income
olr_dws_lik2_inc <- polr(lik2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                           educat + work + politics_s + inc_cat + clim_s,
                         data= combined_likelihood_cat, Hess = TRUE)
brant.test(olr_dws_lik2_inc)
## store table
lik2_inc_table <- coef(summary(olr_dws_lik2_inc))

## calculate and store p values
lik2_inc_p <- pnorm(abs(lik2_inc_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
lik2_inc_table <- cbind(lik2_inc_table, "p value" = lik2_inc_p)
# write.csv(lik2_inc_table, "paper2/SuppTab_FISLikelihood_OLR_inc.csv")

vglm_base_lik2_inc <- vglm(lik2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                             educat + work + politics_s + inc_cat + clim_s,
                       family = cumulative(parallel = TRUE, reverse = TRUE), # reverse = FALSE),
                       data = combined_likelihood_cat)

# Accuracy
olr_dws_lik2_acc <- polr(lik2 ~ accGroup_rm + name + region + gender_collapsed + age_s + latino + raceOther + 
                           educat + work + politics_s + money_s + clim_s,
                         data= combined_likelihood_cat, Hess = TRUE)
brant.test(olr_dws_lik2_acc)
## store table
lik2_acc_table <- coef(summary(olr_dws_lik2_acc))

## calculate and store p values
lik2_acc_p <- pnorm(abs(lik2_acc_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
lik2_acc_table <- cbind(lik2_acc_table, "p value" = lik2_acc_p)
# write.csv(lik2_acc_table, "paper2/SuppTab_FISLikelihood_OLR_acc.csv")

vglm_base_lik2_acc <- vglm(lik2 ~ accGroup_rm + name + region + gender_collapsed + age_s + latino + raceOther + 
                             educat + work + politics_s + money_s + clim_s,
                           family = cumulative(parallel = TRUE, reverse = TRUE), # reverse = FALSE),
                           data = combined_likelihood_cat)

# Climate
olr_dws_lik2_clim <- polr(lik2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                            educat + work + politics_s + money_s + concernedGroup_rm,
                          data= combined_likelihood_cat, Hess = TRUE)
brant.test(olr_dws_lik2_clim)

## store table
lik2_clim_table <- coef(summary(olr_dws_lik2_clim))

## calculate and store p values
lik2_clim_p <- pnorm(abs(lik2_clim_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
lik2_clim_table <- cbind(lik2_clim_table, "p value" = lik2_clim_p)
# write.csv(lik2_clim_table, "paper2/SuppTab_FISLikelihood_OLR_clim.csv")

vglm_base_lik2_clim <- vglm(lik2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                             educat + work + politics_s + money_s + concernedGroup_rm,
                           family = cumulative(parallel = TRUE, reverse = TRUE), # reverse = FALSE),
                           data = combined_likelihood_cat)

# Politics
olr_dws_lik2_pol <- polr(lik2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                           educat + work + politicalGroup_rm + money_s + clim_s,
                         data= combined_likelihood_cat, Hess = TRUE)
brant.test(olr_dws_lik2_pol)

## store table
lik2_pol_table <- coef(summary(olr_dws_lik2_pol))

## calculate and store p values
lik2_pol_p <- pnorm(abs(lik2_pol_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
lik2_pol_table <- cbind(lik2_pol_table, "p value" = lik2_pol_p)
# write.csv(lik2_pol_table, "paper2/SuppTab_FISLikelihood_OLR_pol.csv")

vglm_base_lik2_pol <- vglm(lik2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                              educat + work + politicalGroup_rm + money_s + clim_s,
                            family = cumulative(parallel = TRUE, reverse = TRUE), # reverse = FALSE),
                            data = combined_likelihood_cat)
write.csv(summaryvglm(vglm_base_lik2_pol)@coef3, "paper2/SuppTab_reverse_FISLikelihood_OLR_pol.csv")

# Age
olr_dws_lik2_age <- polr(lik2 ~ val_s + name + region + gender_collapsed + ageGroup_rm + latino + raceOther + 
                           educat + work + politics_s + money_s + clim_s,
                         data= combined_likelihood_cat, Hess = TRUE)
brant.test(olr_dws_lik2_age)

## store table
lik2_age_table <- coef(summary(olr_dws_lik2_age))

## calculate and store p values
lik2_age_p <- pnorm(abs(lik2_age_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
lik2_age_table <- cbind(lik2_age_table, "p value" = lik2_age_p)
# write.csv(lik2_age_table, "paper2/SuppTab_FISLikelihood_OLR_age.csv")

vglm_base_lik2_age <- vglm(lik2 ~ val_s + name + region + gender_collapsed + ageGroup_rm + latino + raceOther + 
                             educat + work + politics_s + money_s + clim_s,
                           family = cumulative(parallel = TRUE, reverse = TRUE), # reverse = FALSE),
                           data = combined_likelihood_cat)

# Confidence #2 -----------------------------------------------------------

# Income
olr_dws_con2_inc <- polr(con2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                           educat + work + politics_s + inc_cat + clim_s,
                         data= combined_rm, Hess = TRUE)
brant.test(olr_dws_con2_inc)
## store table
con2_inc_table <- coef(summary(olr_dws_con2_inc))

## calculate and store p values
con2_inc_p <- pnorm(abs(con2_inc_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
con2_inc_table <- cbind(con2_inc_table, "p value" = con2_inc_p)
# write.csv(con2_inc_table, "paper2/SuppTab_FISconfidence_OLR_inc.csv")

vglm_base_con2_inc <- vglm(con2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                             educat + work + politics_s + inc_cat + clim_s,
                           family = cumulative(parallel = TRUE, reverse = TRUE), # reverse = FALSE),
                           data = combined_rm)

# Accuracy
olr_dws_con2_acc <- polr(con2 ~ accGroup_rm + name + region + gender_collapsed + age_s + latino + raceOther + 
                           educat + work + politics_s + money_s + clim_s,
                         data= combined_rm, Hess = TRUE)
brant.test(olr_dws_con2_acc)
## store table
con2_acc_table <- coef(summary(olr_dws_con2_acc))

## calculate and store p values
con2_acc_p <- pnorm(abs(con2_acc_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
con2_acc_table <- cbind(con2_acc_table, "p value" = con2_acc_p)
# write.csv(con2_acc_table, "paper2/SuppTab_FISconfidence_OLR_acc.csv")

vglm_base_con2_acc <- vglm(con2 ~ accGroup_rm + name + region + gender_collapsed + age_s + latino + raceOther + 
                             educat + work + politics_s + money_s + clim_s,
                           family = cumulative(parallel = TRUE, reverse = TRUE), # reverse = FALSE),
                           data = combined_rm)

# Climate
olr_dws_con2_clim <- polr(con2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                            educat + work + politics_s + money_s + concernedGroup_rm,
                          data= combined_rm, Hess = TRUE)
brant.test(olr_dws_con2_clim)

## store table
con2_clim_table <- coef(summary(olr_dws_con2_clim))

## calculate and store p values
con2_clim_p <- pnorm(abs(con2_clim_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
con2_clim_table <- cbind(con2_clim_table, "p value" = con2_clim_p)
# write.csv(con2_clim_table, "paper2/SuppTab_FISconfidence_OLR_clim.csv")

vglm_base_con2_clim <- vglm(con2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                              educat + work + politics_s + money_s + concernedGroup_rm,
                           family = cumulative(parallel = TRUE, reverse = TRUE), # reverse = FALSE),
                           data = combined_rm)

# Politics
olr_dws_con2_pol <- polr(con2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                           educat + work + politicalGroup_rm + money_s + clim_s,
                         data= combined_rm, Hess = TRUE)
brant.test(olr_dws_con2_pol)

## store table
con2_pol_table <- coef(summary(olr_dws_con2_pol))

## calculate and store p values
con2_pol_p <- pnorm(abs(con2_pol_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
con2_pol_table <- cbind(con2_pol_table, "p value" = con2_pol_p)
# write.csv(con2_pol_table, "paper2/SuppTab_FISconfidence_OLR_pol.csv")

vglm_base_con2_pol <- vglm(con2 ~ val_s + name + region + gender_collapsed + age_s + latino + raceOther + 
                             educat + work + politicalGroup_rm + money_s + clim_s,
                            family = cumulative(parallel = TRUE, reverse = TRUE), # reverse = FALSE),
                            data = combined_rm)
write.csv(summaryvglm(vglm_base_con2_pol)@coef3, "paper2/SuppTab_reverse_FISConfidence_OLR_pol.csv")

# Age
olr_dws_con2_age <- polr(con2 ~ val_s + name + region + gender_collapsed + ageGroup_rm + latino + raceOther + 
                           educat + work + politics_s + money_s + clim_s,
                         data= combined_rm, Hess = TRUE)
brant.test(olr_dws_con2_age)

## store table
con2_age_table <- coef(summary(olr_dws_con2_age))

## calculate and store p values
con2_age_p <- pnorm(abs(con2_age_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
con2_age_table <- cbind(con2_age_table, "p value" = con2_age_p)
# write.csv(con2_age_table, "paper2/SuppTab_FISconfidence_OLR_age.csv")

vglm_base_con2_age <- vglm(con2 ~ val_s + name + region + gender_collapsed + ageGroup_rm + latino + raceOther + 
                             educat + work + politics_s + money_s + clim_s,
                           family = cumulative(parallel = TRUE, reverse = TRUE), # reverse = FALSE),
                           data = combined_rm)

# Comparison plot ---------------------------------------------------------
cnames = c("Variable", "Outcome", "AIC_continuous", "AIC_categorical", 
           "BIC_continuous", "BIC_categorical")

incCon = c("Income", "DWS Confidence", AIC(vglm_partial_con1), AIC(vglm_partial_inc), 
           BIC(vglm_partial_con1), BIC(vglm_partial_inc))
accCon = c("Interpretation", "DWS Confidence", AIC(vglm_partial_con1), AIC(vglm_partial_acc),
           BIC(vglm_partial_con1), BIC(vglm_partial_acc))
climCon = c("Climate literacy", "DWS Confidence", AIC(vglm_partial_con1), AIC(vglm_partial_clim),
            BIC(vglm_partial_con1), BIC(vglm_partial_clim))
polCon = c("Political preference", "DWS Confidence", AIC(vglm_partial_con1), AIC(vglm_partial_pol),
           BIC(vglm_partial_con1), BIC(vglm_partial_pol))
ageCon = c("Age", "DWS Confidence", AIC(vglm_partial_con1), AIC(vglm_partial_age),
           BIC(vglm_partial_con1), BIC(vglm_partial_age))

inclik = c("Income", "DWS Likelihood", AIC(vglm_partial_lik1), AIC(vglm_partial_lik1_inc),
           BIC(vglm_partial_lik1), BIC(vglm_partial_lik1_inc))
acclik = c("Interpretation", "DWS Likelihood", AIC(vglm_partial_lik1), AIC(vglm_partial_lik1_acc),
           BIC(vglm_partial_lik1), BIC(vglm_partial_lik1_acc))
climlik = c("Climate literacy", "DWS Likelihood", AIC(vglm_partial_lik1), AIC(vglm_partial_lik1_clim),
            BIC(vglm_partial_lik1), BIC(vglm_partial_lik1_clim))
pollik = c("Political preference", "DWS Likelihood", AIC(vglm_partial_lik1), AIC(vglm_partial_lik1_pol),
           BIC(vglm_partial_lik1), BIC(vglm_partial_lik1_pol))
agelik = c("Age", "DWS Likelihood", AIC(vglm_partial_lik1), AIC(vglm_partial_lik1_age), 
           BIC(vglm_partial_lik1), BIC(vglm_partial_lik1_age))

inclik2 = c("Income", "FIS Likelihood", AIC(vglm_base_lik2), AIC(vglm_base_lik2_inc),
            BIC(vglm_base_lik2), BIC(vglm_base_lik2_inc))
acclik2 = c("Interpretation", "FIS Likelihood", AIC(vglm_base_lik2), AIC(vglm_base_lik2_acc),
            BIC(vglm_base_lik2), BIC(vglm_base_lik2_acc))
climlik2 = c("Climate literacy", "FIS Likelihood", AIC(vglm_base_lik2), AIC(vglm_base_lik2_clim),
             BIC(vglm_base_lik2), BIC(vglm_base_lik2_clim))
pollik2 = c("Political preference", "FIS Likelihood", AIC(vglm_base_lik2), AIC(vglm_base_lik2_pol),
            BIC(vglm_base_lik2), BIC(vglm_base_lik2_pol))
agelik2 = c("Age", "FIS Likelihood", AIC(vglm_base_lik2), AIC(vglm_base_lik2_age), 
            BIC(vglm_base_lik2), BIC(vglm_base_lik2_age))

incCon2 = c("Income", "FIS Confidence", AIC(vglm_base_con2), AIC(vglm_base_con2_inc), 
            BIC(vglm_base_con2), BIC(vglm_base_con2_inc))
accCon2 = c("Interpretation", "FIS Confidence", AIC(vglm_base_con2), AIC(vglm_base_con2_acc),
            BIC(vglm_base_con2), BIC(vglm_base_con2_acc))
climCon2 = c("Climate literacy", "FIS Confidence", AIC(vglm_base_con2), AIC(vglm_base_con2_clim),
             BIC(vglm_base_con2), BIC(vglm_base_con2_clim))
polCon2 = c("Political preference", "FIS Confidence", AIC(vglm_base_con2), AIC(vglm_base_con2_pol),
            BIC(vglm_base_con2), BIC(vglm_base_con2_pol))
ageCon2 = c("Age", "FIS Confidence", AIC(vglm_base_con2), AIC(vglm_base_con2_age),
            BIC(vglm_base_con2), BIC(vglm_base_con2_age))

AIC_BICmat = matrix(c(incCon, accCon, climCon, polCon, ageCon, inclik, acclik, 
                      climlik, pollik, agelik, inclik2, acclik2, climlik2, pollik2, 
                      agelik2, incCon2, accCon2, climCon2, polCon2, ageCon2), 
                    ncol = 6, byrow=TRUE)
AIC_BIC_DF = as.data.frame(AIC_BICmat)
colnames(AIC_BIC_DF) = cnames

AIC_BIC_DF[,3:6] = lapply(X=3:6, function(X){round(as.numeric(AIC_BIC_DF[,X]), 2)})

library(ggplot2)
library(tidyr)
library(dplyr)

# Step 1 — Calculate deltas directly, no pipes
AIC_BIC_DF$delta_AIC <- AIC_BIC_DF$AIC_categorical - AIC_BIC_DF$AIC_continuous
AIC_BIC_DF$delta_BIC <- AIC_BIC_DF$BIC_categorical - AIC_BIC_DF$BIC_continuous
AIC_BIC_DF$label     <- paste(AIC_BIC_DF$Variable, AIC_BIC_DF$Outcome, sep = "\n")

# Step 2 — Pivot to long format
AIC_BIC_DF_long <- tidyr::pivot_longer(
  AIC_BIC_DF,
  cols      = c("delta_AIC", "delta_BIC"),
  names_to  = "metric",
  values_to = "delta"
)

colRdGy = brewer.pal(3, "RdGy")

# Step 3 — Recode metric labels
AIC_BIC_DF_long$metric <- ifelse(AIC_BIC_DF_long$metric == "delta_AIC", 
                                 "AIC difference", "BIC difference")

# Step 4 — Build factor order manually without arrange
unique_labels <- unique(AIC_BIC_DF$label)
unique_labels <- unique_labels[order(AIC_BIC_DF$Variable[match(unique_labels, AIC_BIC_DF$label)])]
AIC_BIC_DF_long$label <- factor(AIC_BIC_DF_long$label, levels = rev(unique_labels))

# Step 5 — Color by direction: negative = categorical better, positive = continuous better
AIC_BIC_DF_long$direction <- ifelse(AIC_BIC_DF_long$delta < 0, 
                                    "Categorical better", "Continuous better")

# Step 6 — Plot
aicplot = ggplot(AIC_BIC_DF_long, aes(x = delta, y = label, color = direction)) +
  geom_vline(xintercept =  0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_vline(xintercept = -2, linetype = "dotted", color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept =  2, linetype = "dotted", color = "grey60", linewidth = 0.4) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("Categorical better" = colRdGy[1],
               "Continuous better"  = colRdGy[3]),
    name   = NULL
  ) +
  labs(
    x       = "Difference (categorical minus continuous)",
    #caption = "Negative values (green) favour the categorical model; positive values (blue) favour the continuous model.\nDotted lines at +/-2 indicate a meaningful difference threshold.",
    y       = NULL
  ) +
  facet_wrap(~ metric, ncol = 2) +
  theme_bw(base_size = 11) +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 10),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    strip.background   = element_rect(fill = "grey92", color = NA),
    strip.text         = element_text(face = "bold", size = 11),
    axis.text.y        = element_text(size = 9),
    plot.caption       = element_text(size = 8, color = "grey40", hjust = 0),
    plot.margin        = margin(8, 12, 8, 8)
  )

pdf(file="paper2/supp_AIC_BIC_reverse_comparison.pdf", family="Helvetica", 
    width=maximum_width, height=column_height*3)
aicplot
dev.off()

