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
mm_TO_inches = function(mm){
  mm * 0.039370
}

# single_column = mm_TO_inches(84)
# med_column = mm_TO_inches(129)
# double_column = mm_TO_inches(174)
# maximum_width = mm_TO_inches(234)
# column_height = 2.7
# double_height = column_height * 2

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

##########################################################################
# Demographics
##########################################################################
# Is there a correlation between accuracy and:
#   Income, Political preference, Employment, Education, Race, Age, Gender, 
#   Location (multiple linear regression model/ kruskal-Wallis?)
#   linear regression (38 tests = 1+ 1+ 6 + 6 +6 + 1 + 3+ 14)(PA)

# Income ------------------------------------------------------------------
options(max.print=1164)
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

##########################################################################
# Linear Regression
##########################################################################

plotvars = all.size.df

plotvars = as.data.frame(cbind(plotvars, ageGroup))

climcol = brewer.pal(3, "PuOr")
agecol = brewer.pal(3, "Blues")
racecol = brewer.pal(7, "Set3")
workcol = brewer.pal(7, "Dark2")
educol = brewer.pal(7, "GnBu")
divcol = c(brewer.pal(6, "Paired"), brewer.pal(5, "Purples")[3:5])
gencol = brewer.pal(5, "Pastel2")
polcol = brewer.pal(3, "RdYlBu")

# Linear regression -------------------------------------------------------
val.region.modedu <- lm(val ~ name + division + gender + age + latino + racelarge +
                                as.numeric(edunum) + work + politics + clim + money, data = all.size.df)
summary(val.region.modedu)

political.region.modedu <- lm(politics ~ val + name + division + gender + age + latino + racelarge +
                               as.numeric(edunum) + work + clim + money, data = all.size.df)
summary(political.region.modedu)

clim.region.modedu<- lm(clim ~ val + name + division + gender + age + latino + racelarge +
                          as.numeric(edunum) + work + politics + money, data = all.size.df)
summary(clim.region.modedu)

age.region.modedu<- lm(age ~ val + name + division + gender + latino + racelarge +
                         as.numeric(edunum) + work + clim + politics + money, data = all.size.df)
summary(age.region.modedu)

# Refactor variables ------------------------------------------------------
plotvars$division = factor(plotvars$division, levels = c("New England", "Middle Atlantic",
                                                               "East North Central", "West North Central",
                                                               "Mountain", "Pacific",
                                                               "South Atlantic", "East South Central",
                                                               "West South Central"))

plotvars$work = factor(plotvars$work, levels = c("Student", 
                                                 "A homemaker or stay-at-home parent", 
                                                 "Unemployed and looking for work",
                                                 "Working part-time",
                                                 "Working full-time",
                                                 "Retired", "Other"))

plotvars$edu = factor(plotvars$edu, levels = c("Some high school or less",
                                               "High school diploma or GED", 
                                               "Some college, but no degree", 
                                               "Associates or technical degree",
                                               "Bachelor’s degree", 
                                               "Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS etc.)",
                                               "Prefer not to say"))


# Calculate stats ---------------------------------------------------------

climDivStat = aggregate(clim ~ division, data = plotvars,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
climGenStat = aggregate(clim ~ gender, data = plotvars,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
climWorkStat = aggregate(clim ~ work, data = plotvars,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
climPolStat = aggregate(clim ~ ptab$political, data = plotvars,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
climAgeStat = aggregate(clim ~ ageGroup, data = plotvars,
                        function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))

# Divison not a predictor of politics
polDivStat = aggregate(politics ~ division, data = plotvars,
                       function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
polGenStat = aggregate(politics ~ gender, data = plotvars,
                       function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
polWorkStat = aggregate(politics ~ work, data = plotvars,
                        function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
polClimStat = aggregate(politics ~ ptab$clim, data = plotvars,
                        function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))

ageRaceStat = aggregate(age ~ racelarge, data = plotvars,
                        function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
ageEduStat = aggregate(age ~ edu, data = plotvars,
                       function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
ageWorkStat = aggregate(age ~ work, data = plotvars,
                        function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
ageClimStat = aggregate(age ~ ptab$clim, data = plotvars,
                        function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))

# graph type, region, race, education, work
valRaceStat = aggregate(val ~ racelarge, data = plotvars,
                        function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
valEduStat = aggregate(val ~ edu, data = plotvars,
                       function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
valWorkStat = aggregate(val ~ work, data = plotvars,
                        function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
valDivStat = aggregate(val ~ division, data = plotvars,
                       function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
valGraphStat = aggregate(val ~ name, data = plotvars,
                        function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))

##########################################################################
# Generate figures
##########################################################################
# Climate
pdf(file="paper2/climatefactors1.pdf", family="Helvetica", width=double_column, 
    height=column_height*3.75, pointsize=12)

par(mfrow=c(5,1), mgp=c(1.5,0.5,0), mar=c(1,4,1,1))
vioplot(clim ~ gender, data = plotvars, xlab="", cex.axis = 0.8, ylim=c(-7, 8.5),
        yaxt="n", xaxt="n", ylab="Climate science literacy", col=gencol, plotCentre="line")
axis(2, at=c(-5.5,7), labels=c("Lower", "Higher"), tick=FALSE)
put.fig.letter("a.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:5, text = c("Male\n(n=583)", "Female\n(n=571)", "Non-binary\n3rd gender (n=4)",
                               "Self-describe\n(n=4)", "Prefer\nnot to say (n=2)"), 
      line=1.5, cex=0.65)
mtext(side=3, "Gender", adj=0.01, line=-1.2, cex=0.75)
points(1:5, climGenStat$clim[,'mean'], bg="white", pch=21)

legend("topright", legend =c("Median", "Mean"), pch=c(NA, 21), bty='n', 
       pt.bg = c(NA,"white"), lty=c(1, NA), col = 'black', pt.cex=c(NA, 1))

par(mar=c(1,4,2,1))
vioplot(clim ~ ptab$political, data = plotvars, xlab="", ylab="Climate science literacy", 
        yaxt="n", xaxt="n", col=polcol, plotCentre="line", cex.axis = 0.8, ylim=c(-7, 8.5))
axis(2, at=c(-5.5,7), labels=c("Lower", "Higher"), tick=FALSE)
put.fig.letter("b.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:3, text = c("Conservative (n=421)", "Moderate (n=489)", "Liberal (n=254)"), 
      line=0.5, cex=0.65)
mtext(side=3, "Political preference", adj=0.01, line=-1.2, cex=0.75)
points(1:3, climPolStat$clim[,'mean'], bg="white", pch=21)

par(mar=c(2,4,1,1))
vioplot(clim ~ ageGroup, data = plotvars, xlab="", ylab="Climate science literacy", 
        yaxt="n", xaxt="n", col=agecol, plotCentre="line", cex.axis = 0.8, ylim=c(-7, 8.5))
axis(2, at=c(-5.5,7), labels=c("Lower", "Higher"), tick=FALSE)
put.fig.letter("c.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:3, text = c("18-34 (n=347)", "35-54 (n=412)", "55+ (n=405)"), 
      line=0.5, cex=0.65)
mtext(side=3, "Age", adj=0.01, line=-1.2, cex=0.75)
points(1:3, climAgeStat$clim[,'mean'], bg="white", pch=21)

par(mar=c(3.5,4,0,1))
vioplot(clim ~ division, data = plotvars, xlab="", cex.axis = 0.8, ylim=c(-7, 8.5), 
        yaxt="n", xaxt="n", ylab="Climate science literacy", col=divcol, plotCentre="line")
axis(2, at=c(-5.5,7), labels=c("Lower", "Higher"), tick=FALSE)
put.fig.letter("e.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:9, line=1.9, cex=0.65, 
      c("New\nEngland\n(n=38)", "Middle\nAtlantic\n(n=184)",
        "E. N.\nCentral\n(n=177)", "W. N.\nCentral\n(n=71)",
        "Mountain\n(n=80)", "Pacific\n(n=143)",
        "S. Atlantic\n(n=238)", "E. S.\nCentral\n(n=84)",
        "W. S.\nCentral\n(n=149)"))
mtext(side=3, "Division", adj=0.01, line=-1.2, cex=0.75)
points(1:9, climDivStat$clim[,'mean'], bg="white", pch=21)

par(mar=c(2.5,4,0,1))
vioplot(clim ~ work, data = plotvars, xlab="", ylab="Climate science literacy", 
        yaxt="n", xaxt="n", col = workcol, plotCentre="line", cex.axis = 0.8, ylim=c(-7, 8.5))
axis(2, at=c(-5.5,7), labels=c("Lower", "Higher"), tick=FALSE)
put.fig.letter("d.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:7, line=1, cex=0.65, 
      c("Student\n(n=39)", "Home.\n(n=71)", "Unemploy.\n(n=127)", 
        "Part-time\n(n=153)", "Full-time\n(n=463)", "Retired\n(n=258)", 
        "Other\n(n=53)"))
mtext(side=3, "Employment", adj=0.01, line=-1.2, cex=0.75)
points(1:7, climWorkStat$clim[,'mean'], bg="white", pch=21)

dev.off()

# Politics
pdf(file="paper2/politicalfactors.pdf", family="Helvetica", width=double_column, 
    height=column_height*3, pointsize=12)

par(mfrow=c(4,1), mgp=c(1.5,0.5,0), mar=c(2,4,1,1))
vioplot(politics ~ gender, data = plotvars, xlab="", cex.axis = 0.8, ylim=c(0, 11), 
        yaxt="n", xaxt="n", ylab="Political preference", col=gencol, plotCentre="line")
put.fig.letter("a.",font=2, x=0.025, y=0.98)
axis(2, at=c(1,9), labels=c("Liberal", "Conservative"), tick=FALSE)
mtext(side=1, at=1:5, text = c("Male\n(n=583)", "Female\n(n=571)", "Non-binary\n3rd gender (n=4)",
                               "Self-describe\n(n=4)", "Prefer\nnot to say (n=2)"), 
      line=1.5, cex=0.65)
mtext(side=3, "Gender", adj=0.01, line=-1.2, cex=0.75)
points(1:5, polGenStat$politics[,'mean'], bg="white", pch=21)

legend("bottomright", legend =c("Median", "Mean"), pch=c(NA, 21), bty='n', 
       pt.bg = c(NA,"white"), lty=c(1, NA), col = 'black', pt.cex=c(NA, 1))

vioplot(politics ~ ptab$clim, data = plotvars, xlab="", ylab="Political preference", 
        yaxt="n", xaxt="n", col=climcol, plotCentre="line", cex.axis = 0.8, ylim=c(0, 11))
put.fig.letter("b.",font=2, x=0.025, y=0.98)
axis(2, at=c(1,9), labels=c("Liberal", "Conservative"), tick=FALSE)
mtext(side=1, at=1:3, text = c("Lower literacy (n=140)", "Moderate literacy (n=334)", "Higher literacy (n=690)"), 
      line=0.5, cex=0.65)
mtext(side=3, "Climate science literacy", adj=0.01, line=-1.2, cex=0.75)
points(1:3, polClimStat$politics[,'mean'], bg="white", pch=21)

par(mar=c(3,4,0,1))
vioplot(politics ~ work, data = plotvars, xlab="", ylab="Political preference", 
        yaxt="n", xaxt="n", col = workcol, plotCentre="line", cex.axis = 0.8, ylim=c(0, 11))
put.fig.letter("c.",font=2, x=0.025, y=0.98)
axis(2, at=c(1,9), labels=c("Liberal", "Conservative"), tick=FALSE)
mtext(side=1, at=1:7, line=1, cex=0.65, 
      c("Student\n(n=39)", "Home.\n(n=71)", "Unemploy.\n(n=127)", 
        "Part-time\n(n=153)", "Full-time\n(n=463)", "Retired\n(n=258)", 
        "Other\n(n=53)"))
mtext(side=3, "Employment", adj=0.01, line=-1.2, cex=0.75)
points(1:7, polWorkStat$politics[,'mean'], bg="white", pch=21)

par(mar=c(3.5,4,0,1))
vioplot(politics ~ division, data = plotvars, xlab="", cex.axis = 0.8, ylim=c(0, 11), 
        yaxt="n", xaxt="n", ylab="Political preference", col=divcol, plotCentre="line")
put.fig.letter("d.",font=2, x=0.025, y=0.98)
axis(2, at=c(1,9), labels=c("Liberal", "Conservative"), tick=FALSE)
mtext(side=1, at=1:9, line=1.9, cex=0.65, 
      c("New\nEngland\n(n=38)", "Middle\nAtlantic\n(n=184)",
        "E. N.\nCentral\n(n=177)", "W. N.\nCentral\n(n=71)",
        "Mountain\n(n=80)", "Pacific\n(n=143)",
        "S. Atlantic\n(n=238)", "E. S.\nCentral\n(n=84)",
        "W. S.\nCentral\n(n=149)"))
mtext(side=3, "Division", adj=0.01, line=-1.2, cex=0.75)
points(1:9, polDivStat$politics[,'mean'], bg="white", pch=21)

dev.off()

# Age
pdf(file="paper2/Agefactors.pdf", family="Helvetica", width=double_column, 
    height=column_height*3, pointsize=12)

par(mfrow=c(4,1), mgp=c(1.5,0.5,0), mar=c(2,4,1,1))
vioplot(age ~ edu, data = plotvars, xlab="", cex.axis = 0.8, 
        xaxt="n", ylab="Age", col=educol, plotCentre="line", ylim=c(18, 90))
put.fig.letter("a.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:7, text = c("Some HS\nor less (n=40)", "HS or GED\n(n=296)", 
                               "Some\ncollege (n=278)", "Associates\n(n=154)", "Bach.\n(n=259)", 
                               "Postgrad\n(n=133)", "Prefer\nnot to say (n=4)"), line=1.5, cex=0.65)
mtext(side=3, "Education", adj=0.01, line=-1.2, cex=0.75)
points(1:7, ageEduStat$age[,'mean'], bg="white", pch=21)

legend("topright", legend =c("Median", "Mean"), pch=c(NA, 21), bty='n', 
       pt.bg = c(NA,"white"), lty=c(1, NA), col = 'black', pt.cex=c(NA, 1))

vioplot(age ~ ptab$clim, data = plotvars, xlab="", ylab="Age", 
        xaxt="n", col=climcol, plotCentre="line", cex.axis = 0.8, ylim=c(18, 90))
put.fig.letter("b.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:3, text = c("Lower literacy (n=140)", "Moderate literacy (n=334)", "Higher literacy (n=690)"), 
      line=0.5, cex=0.65)
mtext(side=3, "Climate science literacy", adj=0.01, line=-1.15, cex=0.75)
points(1:3, ageClimStat$age[,'mean'], bg="white", pch=21)

par(mar=c(3,4,0,1))
vioplot(age ~ work, data = plotvars, xlab="", ylab="Age", ylim=c(18, 90), 
        xaxt="n", col = workcol, plotCentre="line", cex.axis = 0.8)
put.fig.letter("c.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:7, line=1.2, cex=0.65, 
      c("Student\n(n=39)", "Home.\n(n=71)", "Unemploy.\n(n=127)", 
        "Part-time\n(n=153)", "Full-time\n(n=463)", "Retired\n(n=258)", 
        "Other\n(n=53)"))
mtext(side=3, "Employment", adj=0.01, line=-1.2, cex=0.75)
points(1:7, ageWorkStat$age[,'mean'], bg="white", pch=21)

par(mar=c(3.5,4,0,1))
vioplot(age ~ racelarge, data = plotvars, xlab="", cex.axis = 0.8, 
        xaxt="n", ylab="Age", col=racecol, plotCentre="line", ylim=c(18, 90))
put.fig.letter("d.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:7, line=1.9, cex=0.65, 
      c("White\n(n=855)", "Black\n(n=186)", "Asian\n(n=58)",
        "Native\nAmerican\n(n=18)", "Pacific\nIslander\n(n=3)",
        "Other\n(n=35)", "Prefer\nnot to say (n=9)"))
mtext(side=3, "Race", adj=0.01, line=-1.2, cex=0.75)
points(1:7, ageRaceStat$age[,'mean'], bg="white", pch=21)

dev.off()

# Interpretation
pdf(file="paper2/intfactors.pdf", family="Helvetica", width=double_column, 
    height=column_height*3.75, pointsize=12)

par(mfrow=c(5,1), mgp=c(1.5,0.5,0), mar=c(1.5,4,1,1))
vioplot(val ~ racelarge, data = plotvars, xlab="", cex.axis = 0.8, 
        xaxt="n", ylab="Interpretation", col=racecol, plotCentre="line")
put.fig.letter("a.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:7, line=2, cex=0.65, 
      c("White\n(n=855)", "Black\n(n=186)", "Asian\n(n=58)",
        "Native\nAmerican\n(n=18)", "Pacific\nIslander\n(n=3)",
        "Other\n(n=35)", "Prefer\nnot to say (n=9)"))
mtext(side=3, "Race", adj=0.01, line=-1.2, cex=0.75)
points(1:7, valRaceStat$val[,'mean'], bg="white", pch=21)

legend("topright", legend =c("Median", "Mean"), pch=c(NA, 21), bty='n', 
       pt.bg = c(NA,"white"), lty=c(1, NA), col = 'black', pt.cex=c(NA, 1))

par(mar=c(1,4,2,1))
vioplot(val ~ edu, data = plotvars, xlab="", cex.axis = 0.8, 
        xaxt="n", ylab="Interpretation", col=educol, plotCentre="line")
put.fig.letter("b.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:7, text = c("Some HS\nor less (n=40)", "HS or GED\n(n=296)", 
                               "Some\ncollege (n=278)", "Associates\n(n=154)", "Bach.\n(n=259)", 
                               "Postgrad\n(n=133)", "Prefer\nnot to say (n=4)"), line=1.5, cex=0.65)
mtext(side=3, "Education", adj=0.01, line=-1.2, cex=0.75)
points(1:7, valEduStat$val[,'mean'], bg="white", pch=21)

par(mar=c(1.25,4,1.75,1))
vioplot(val ~ name, data = plotvars, xlab="", ylab="Interpretation", 
        xaxt="n", col=graphcol, plotCentre="line", cex.axis = 0.8)
put.fig.letter("c.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:3, text = c("Bar (n=393)", "Box (n=375)", "Area (n=396)"), 
      line=0.5, cex=0.65)
mtext(side=3, "Graph type", adj=0.01, line=-1.2, cex=0.75)
points(1:3, valGraphStat$val[,'mean'], bg="white", pch=21)

par(mar=c(2.5,4,0.5,1))
vioplot(val ~ work, data = plotvars, xlab="", ylab="Interpretation", 
        xaxt="n", col = workcol, plotCentre="line", cex.axis = 0.8)
put.fig.letter("d.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:7, line=1.2, cex=0.65, 
      c("Student\n(n=39)", "Home.\n(n=71)", "Unemploy.\n(n=127)", 
        "Part-time\n(n=153)", "Full-time\n(n=463)", "Retired\n(n=258)", 
        "Other\n(n=53)"))
mtext(side=3, "Employment", adj=0.01, line=-1.2, cex=0.75)
points(1:7, valWorkStat$val[,'mean'], bg="white", pch=21)

par(mar=c(3.5,4,0,1))
vioplot(val ~ division, data = plotvars, xlab="", cex.axis = 0.8, 
        xaxt="n", ylab="Interpretation", col=divcol, plotCentre="line")
put.fig.letter("e.",font=2, x=0.025, y=0.98)
mtext(side=1, at=1:9, line=1.9, cex=0.65, 
      c("New\nEngland\n(n=38)", "Middle\nAtlantic\n(n=184)",
        "E. N.\nCentral\n(n=177)", "W. N.\nCentral\n(n=71)",
        "Mountain\n(n=80)", "Pacific\n(n=143)",
        "S. Atlantic\n(n=238)", "E. S.\nCentral\n(n=84)",
        "W. S.\nCentral\n(n=149)"))
mtext(side=3, "Division", adj=0.01, line=-1.2, cex=0.75)
points(1:9, valDivStat$val[,'mean'], bg="white", pch=21)

dev.off()


##########################################################################
# End
##########################################################################