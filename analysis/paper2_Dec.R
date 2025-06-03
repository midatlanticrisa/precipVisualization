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

single_column = mm_TO_inches(84)
med_column = mm_TO_inches(129)
double_column = mm_TO_inches(174)
maximum_width = mm_TO_inches(234)
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

scen_conflood = table(ptab$CHAL2_conVal)
scen_conflood = as.data.frame((scen_conflood/sum(scen_conflood))*100)
colnames(scen_conflood) = colnames(scen_con)[1:2]
scen_conflood$name = "Confidence"

# # http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r#:~:text=R%20functions,-Correlation%20coefficient%20can&text=cor()%20computes%20the%20correlation,%2Dvalue)%20of%20the%20correlation%20.
cor.test(ptab$CHAL2_conVal, protab$CHAL2, method = "pearson")
cor.test(ptab$CHAL2_likVal, protab$CHAL2, method = "pearson")
cor.test(ptab$CHAL2_conVal, ptab$CHAL2_likVal, method = "pearson")

cor.test(ptab$CHAL1_conVal, protab$CHAL1, method = "pearson")
cor.test(ptab$CHAL1_likVal, protab$CHAL1, method = "pearson")
cor.test(ptab$CHAL1_conVal, ptab$CHAL1_likVal, method = "pearson")

cor.test(political, concerned, method = "pearson") # politics and climate
cor.test(political, overAcc$val, method = "pearson") # politics and accuracy
cor.test(concerned, overAcc$val, method = "pearson") # climate and accuracy
cor.test(political, age, method = "pearson") # politics and age
cor.test(concerned, age, method = "pearson") # climate and age
cor.test(age, overAcc$val, method = "pearson") # age and accuracy

condf = data.frame(val = protab$CHAL1, con = ptab$CHAL1_con)
dec_con <- condf %>%
  dplyr::group_by(val, con) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(val = factor(val, 
                             levels = 1:4,
                             labels = c("do nothing",
                                        "protect 2 events/yr", 
                                        "protect 4 events/yr",
                                        "pave the driveway\n(120 events)")))%>%
  dplyr::mutate(con = factor(con, levels = confidence$ans))

# # # Grouped plot by graph type split into graphs of accuracy type
# ggplot(dec_con, aes(fill=val,x=con, y=Percent)) + 
#   geom_bar(position="dodge", stat="identity", colour="black", linewidth = 0.1) +
#   labs(x = "Question ID", y= "SUS score per question", fill="Graph type:") +
#   scale_fill_manual(values=driveCols) + #ylim(0, 10) +
#   theme_bw() +
#   theme(legend.title = element_text(size=9), 
#         axis.text.x = element_text(size = 8), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())

drive_sec = topics$AI.Analysis.of.Scenario.Justification..ChatGPT.[match(prochoices$Primary.cycle.code, topics$Code)]
flood_sec = topics$AI.Analysis.of.Scenario.Justification..ChatGPT.[match(prochoices$Primary.cycle.code.1, topics$Code)]

chal2_lm <- lm(protab$CHAL2 ~ ptab$CHAL2_conVal + ptab$CHAL2_likVal + flood_sec)
summary(chal2_lm)
resid.state.var<- 1-summary(chal2_lm)$r.squared; resid.state.var
length(names(chal2_lm$coefficients))

chal1_lm <- lm(protab$CHAL1 ~ ptab$CHAL1_conVal + ptab$CHAL1_likVal + drive_sec)
summary(chal1_lm)
resid.state.var<- 1-summary(chal2_lm)$r.squared; resid.state.var
length(names(chal2_lm$coefficients))

# unqtop = unique(drive_sec)
# plot(density(drive_sec ~ protab$CHAL1), xlab="Reason", ylab="Protective")
# plot(density(protab$CHAL1[which(drive_sec == unqtop[1])]))
# for(i in 2:6){
#   lines(density(protab$CHAL1[which(drive_sec == unqtop[i])]))
# }
# 
# indscoredf = data.frame(val = protab$CHAL1,
#                         ques = drive_sec)
# indscoredf$ques = factor(indscoredf$ques, levels = unqtop)
# 
# # create bar plot data
# dec_pro <- indscoredf %>%
#   dplyr::group_by(ques, val) %>%
#   dplyr::summarise(Frequency = n()) %>%
#   dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
#   dplyr::mutate(val = factor(val, 
#                                levels = 1:4,
#                                labels = c("do nothing",
#                                           "protect 2 events/yr", 
#                                           "protect 4 events/yr",
#                                           "pave the driveway\n(120 events)")))%>%
#   dplyr::mutate(ques = factor(ques, levels = unqtop))
# 
# # # Grouped plot by graph type split into graphs of accuracy type
# ggplot(dec_pro, aes(fill=ques,x=val, y=Percent)) + 
#   geom_bar(position="dodge", stat="identity", colour="black", linewidth = 0.1) +
#   labs(x = "Question ID", y= "SUS score per question", fill="Graph type:") +
#   scale_fill_manual(values=color_codes$colors) + #ylim(0, 10) +
#   theme_bw() +
#   theme(legend.title = element_text(size=9), 
#         axis.text.x = element_text(size = 8), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# dec_op <- indscoredf %>%
#   dplyr::group_by(val, ques) %>%
#   dplyr::summarise(Frequency = n()) %>%
#   dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
#   dplyr::mutate(val = factor(val, 
#                              levels = 1:4,
#                              labels = c("do nothing",
#                                         "protect 2 events/yr", 
#                                         "protect 4 events/yr",
#                                         "pave the driveway\n(120 events)")))%>%
#   dplyr::mutate(ques = factor(ques, levels = unqtop))
# 
# # # Grouped plot by graph type split into graphs of accuracy type
# ggplot(dec_op, aes(fill=val,x=ques, y=Percent)) + 
#   geom_bar(position="dodge", stat="identity", colour="black", linewidth = 0.1) +
#   labs(x = "Question ID", y= "SUS score per question", fill="Graph type:") +
#   scale_fill_manual(values=driveCols) + #ylim(0, 10) +
#   theme_bw() +
#   theme(legend.title = element_text(size=9), 
#         axis.text.x = element_text(size = 8), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())

# Evaluate overall theme
color_codes = data.frame(code = unique(topics$AI.Analysis.of.Scenario.Justification..ChatGPT.), 
                         colors = brewer.pal(length(unique(topics$AI.Analysis.of.Scenario.Justification..ChatGPT.)), 
                                             "Paired"))

par(mar = c(0, 0, 0, 0))
drive_theme = word_freq(prochoices$Primary.cycle.code, color_codes, "paper2/primary_coding_driveway.csv", 
                        "paper2/secondary_coding_driveway.csv")
drive_theme$Perc = (drive_theme$count/sum(drive_theme$count))*100
flood_theme = word_freq(prochoices$Primary.cycle.code.1, color_codes, "paper2/primary_coding_flood.csv", 
                        "paper2/secondary_coding_flood.csv")
f_tab = d_tab = data.frame(topic = unique(topics$AI.Analysis.of.Scenario.Justification..ChatGPT.))
for(i in 1:length(d_tab$topic)){
  d_tab$count[i] = sum(drive_theme$count[which(drive_theme$topic == d_tab$topic[i])])
  f_tab$count[i] = sum(flood_theme$count[which(flood_theme$topic == f_tab$topic[i])])
}
d_tab$Perc = (d_tab$count/sum(d_tab$count))*100
f_tab$Perc = (f_tab$count/sum(f_tab$count))*100

drive_txt_full = d_tab %>%
  ggplot(aes(topic, Perc, fill = topic)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=color_codes$colors, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  geom_text(aes(label = round(Perc), x=topic), vjust=-0.1, size = 3) +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Driveway washout scenario", y= "Percent (%) of participants", 
       fill="Reason")

flood_txt_full = f_tab %>%
  ggplot(aes(topic, Perc, fill = topic)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=color_codes$colors, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  geom_text(aes(label = round(Perc), x=topic), vjust=-0.1, size = 3) +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Flood insurance scenario", y= "Percent (%) of participants", 
       fill="Reason")

# barplot(drive_theme$Perc)

# create grouped bar plot
drive_dec_full = scen_dec %>%
  ggplot(aes(Val, Perc, fill = Val)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=driveCols, labels = str_wrap(driveops, width = 20)) + theme_bw() +
  geom_text(aes(label = round(Perc), x=Val), vjust=-0.1, size = 3) +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Driveway washout scenario", y= "Percent (%) of participants", 
       fill="Protection options")

# Evaluate confidence
drive_con_full = scen_con %>%
  ggplot(aes(Val, Perc, fill = Val)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=conCols, labels = str_wrap(confidence$ans, width = 18)) +
  geom_text(aes(label = round(Perc), x=Val), vjust=-0.1, size = 3) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Driveway washout scenario", y= "Percent (%) of participants", 
       fill=str_wrap(questionslist$Area_Freq$CHAL1.3, width = 20))

# Evaluate risk perception
drive_risk_full = scen_lik %>% 
  ggplot(aes(Val, Perc, fill = Val)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=likCols, labels = str_wrap(likeops$ans, width = 30)) +
  geom_text(aes(label = round(Perc), x=Val), vjust=-0.1, size = 3) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Driveway washout scenario", y= "Percent (%) of participants", 
       fill=str_wrap("How likely do you think you’ll see a year with 4 or more heavy rainfall events in the next 30 years?", 
                     width = 30))

# Flood scenario
flood_dec_full = scen_decflood %>%
  ggplot(aes(Val, Perc, fill = Val)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=driveCols[c(1,4)], labels = str_wrap(driveops, width = 20)) + theme_bw() +
  geom_text(aes(label = round(Perc), x=Val), vjust=-0.1, size = 3) +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Flood insurance scenario", y= "Percent (%) of participants", 
       fill="Buy flood insurance?")

# Evaluate confidence
flood_con_full = scen_conflood %>%
  ggplot(aes(Val, Perc, fill = Val)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=conCols, labels = str_wrap(confidence$ans, width = 18)) +
  geom_text(aes(label = round(Perc), x=Val), vjust=-0.1, size = 3) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Flood insurance scenario", y= "Percent (%) of participants", 
       fill=str_wrap(questionslist$Area_Freq$CHAL2.3, width = 25))

# Evaluate risk perception
flood_risk_full = scen_likflood %>% 
  ggplot(aes(Val, Perc, fill = Val)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=likCols, labels = str_wrap(likeops$ans, width = 30)) +
  geom_text(aes(label = round(Perc), x=Val), vjust=-0.1, size = 3) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Flood insurance scenario", y= "Percent (%) of participants", 
       fill=str_wrap("How likely do you think another flood will occur from a heavy rainfall event in the next 30 years?", 
                     width = 30))

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

# Location ----------------------------------------------------------------
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

# all.size.df<- as.data.frame(cbind(all.size.df, politicalGroup))

# # Climate --------------------------------------------
# # Race assigned to region --------------------------------------------
# total.region.mod<- lm(clim ~ val + name + region + gender + age + latino + racelarge + edu + work + politics + money, 
#                       data= all.size.df)
# summary(total.region.mod)
# resid.region.var<- 1-summary(total.region.mod)$r.squared; resid.region.var
# length(names(total.region.mod$coefficients))
# 
# # EDU as a number
# # Race assigned to region --------------------------------------------
# total.region.modedu<- lm(clim ~ val + name + region+ gender + age + latino + racelarge + 
#                            as.numeric(edunum) + work + politics + money, 
#                          data= all.size.df)
# summary(total.region.modedu)
# resid.region.varedu<- 1-summary(total.region.modedu)$r.squared; resid.region.varedu
# length(names(total.region.modedu$coefficients))
# 
# # Race assigned to division --------------------------------------------
# total.div.modedu<- lm(clim ~ val + name + division + gender + age + latino + racelarge + 
#                         as.numeric(edunum) + work + politics + money, 
#                       data= all.size.df)
# summary(total.div.modedu)
# resid.div.varedu<- 1-summary(total.div.modedu)$r.squared; resid.div.varedu
# length(names(total.div.modedu$coefficients))
aggregate(CHAL1 ~ division, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))

options(scipen=999)
total.region.pro<- lm(CHAL1 ~ val + name + region + gender + age + latino + racelarge + 
                        as.numeric(edunum) + work + politics + money + clim, 
                      data= all.size.df)
summary(total.region.pro)

total.region.con<- lm(ptab$CHAL1_conVal ~ val + name + region + gender + age + latino + racelarge +
                        as.numeric(edunum) + work + politics + money + clim,
                      data= all.size.df)
summary(total.region.con)

total.region.lik<- lm(ptab$CHAL1_likVal ~ val + name + region + gender + age + latino + racelarge +
                        as.numeric(edunum) + work + politics + money + clim,
                      data= all.size.df)
summary(total.region.lik)

aggregate(ptab$CHAL2_conVal ~ politicalGroup, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))


total.region.pro2<- lm(CHAL2 ~ val + name + region + gender + age + latino + racelarge + 
                        as.numeric(edunum) + work + politics + money+ clim, 
                      data= all.size.df)
summary(total.region.pro2)

total.region.con2<- lm(ptab$CHAL2_conVal ~ val + name + region + gender + age + latino + racelarge +
                        as.numeric(edunum) + work + politics + money + clim,
                      data= all.size.df)
summary(total.region.con2)

total.region.lik2<- lm(ptab$CHAL2_likVal ~ val + name + region + gender + age + latino + racelarge +
                        as.numeric(edunum) + work + politics + money + clim,
                      data= all.size.df)
summary(total.region.lik2)

aggregate(CHAL1 ~ region, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))

# Division

total.division.pro<- lm(CHAL1 ~ val + name + division + gender + age + latino + racelarge + 
                        as.numeric(edunum) + work + politics + money + clim, 
                      data= all.size.df)
summary(total.division.pro)

total.division.con<- lm(ptab$CHAL1_conVal ~ val + name + division + gender + age + latino + racelarge +
                        as.numeric(edunum) + work + politics + money + clim,
                      data= all.size.df)
summary(total.division.con)

total.division.lik<- lm(ptab$CHAL1_likVal ~ val + name + division + gender + age + latino + racelarge +
                        as.numeric(edunum) + work + politics + money + clim,
                      data= all.size.df)
summary(total.division.lik)
write.csv(summary(total.division.lik)[["coefficients"]], "lik.csv")


total.division.pro2<- lm(CHAL2 ~ val + name + division + gender + age + latino + racelarge + 
                         as.numeric(edunum) + work + politics + money+ clim, 
                       data= all.size.df)
summary(total.division.pro2)

total.division.con2<- lm(ptab$CHAL2_conVal ~ val + name + division + gender + age + latino + racelarge +
                         as.numeric(edunum) + work + politics + money + clim,
                       data= all.size.df)
summary(total.division.con2)

write.csv(summary(total.division.con2)[["coefficients"]], "con2.csv")

total.division.lik2<- lm(ptab$CHAL2_likVal ~ val + name + division + gender + age + latino + racelarge +
                         as.numeric(edunum) + work + politics + money + clim,
                       data= all.size.df)
summary(total.division.lik2)
write.csv(summary(total.division.lik2)[["coefficients"]], "lik2.csv")

# Female 3.74, male 3.86; 3.73 and 3.81 Male (flood insurance); prefer to self desribe 2.75 (fi). Other groups 4,4,2 people
aggregate(ptab$CHAL1_conVal ~ gender, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
aggregate(ptab$CHAL2_conVal ~ gender, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
# 3.78 (3.74) vs 3.89 (3.95) (flood insurance)
aggregate(ptab$CHAL1_conVal ~ latino, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
aggregate(ptab$CHAL2_conVal ~ latino, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
# indian based on just 18 participants
aggregate(ptab$CHAL1_conVal ~ racelarge, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
aggregate(ptab$CHAL2_conVal ~ racelarge, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
# some high school of less 3.62; grad degree 4.03 (driveway); no pattern in flood insurance
aggregate(ptab$CHAL1_conVal ~ edu, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
aggregate(ptab$CHAL2_conVal ~ edu, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))

# flood insurance: MW-5.18, south-5.26, west-5.59
aggregate(ptab$CHAL1_likVal ~ region, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
aggregate(ptab$CHAL2_likVal ~ region, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
# Female 5.21, male 4.99; 3.73 and 3.81 Male (driveway)
aggregate(ptab$CHAL1_likVal ~ gender, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
aggregate(ptab$CHAL2_likVal ~ gender, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
# 5.65: indian (highest); 3.67: islander (lowest); 5.08 white (driveway). indian based on just 18 participants; islander just 3.
aggregate(ptab$CHAL1_likVal ~ racelarge, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
aggregate(ptab$CHAL2_likVal ~ racelarge, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
# driveway: 5.17 full-time, 4.86 (lowest) - unemployed, flood: 5.41 full-time, 5.13 (lowest) - unemployed
aggregate(ptab$CHAL1_likVal ~ work, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
aggregate(ptab$CHAL2_likVal ~ work, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))

aggregate(val ~ ptab$CHAL1_likVal, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
aggregate(val ~ ptab$CHAL2_likVal, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))


options(scipen=999)
summary(total.region.pro)
total.div.pro<- lm(CHAL2 ~ val + name + division + gender + age + latino + racelarge + 
                     as.numeric(edunum) + work + politics + money+ clim, 
                   data= all.size.df)
summary(total.div.pro)
# cor(all.pro.df$CHAL2, all.pro.df$val)
# aggregate(CHAL1 ~ val, data = all.pro.df,
#           function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x), size = length(x)), 2))
# 
# # http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r#:~:text=R%20functions,-Correlation%20coefficient%20can&text=cor()%20computes%20the%20correlation,%2Dvalue)%20of%20the%20correlation%20.
# res <- cor.test(all.pro.df$CHAL2, all.pro.df$val, method = "pearson")
# tt = cor.test(all.pro.df$CHAL2, all.pro.df$val, method = "pearson")
# round(c(tt$estimate, tt$p.value), 2)
# 
# cor.test(overallUse$val, all.pro.df$val, method = "pearson")

# plot(CHAL1 ~ val, data = all.pro.df)

# age -----------------------------------------------------------------
# create bar plot data
agedat <- protab %>%
  dplyr::group_by(age, CHAL1) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1 = factor(CHAL1, 
                               levels = 1:4,
                               labels = c("do nothing",
                                          "protect 2 events/yr", 
                                          "protect 4 events/yr",
                                          "pave the driveway\n(120 events)")))%>%
  dplyr::mutate(age = factor(age, levels = c("18-34", "35-54", "55+")))

# create grouped bar plot
drive_dec_age = agedat %>%
  ggplot(aes(CHAL1, Percent, fill = CHAL1)) + facet_grid(~age) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=driveCols, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  geom_text(aes(label = round(Percent), x=CHAL1), vjust=-0.1, size = 3) +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y= "Percent (%) of participants", 
       fill="Protection options")

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1 ~ age, data = protab)
dunnTest(CHAL1 ~ age, data = protab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1 ~ age, data = protab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate confidence
gldatcondriveage <- ptab %>%
  dplyr::group_by(age, CHAL1_conVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1_conVal = factor(CHAL1_conVal, levels = confidence$val,
                                      labels = confidence$ans)) %>%
  dplyr::mutate(age = factor(age, levels = c("18-34", "35-54", "55+")))

# create grouped bar plot
drive_conage = ggplot(gldatcondriveage, aes(CHAL1_conVal, Percent, fill = CHAL1_conVal)) +
  facet_grid(~age) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL1_conVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=conCols, labels = function(x) str_wrap(x, width = 18)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y= "Percent (%) of participants", 
       fill=str_wrap(questionslist$Area_Freq$CHAL1.3, width = 20))

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1_conVal ~ age, data = ptab)
dunnTest(CHAL1_conVal ~ age, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1_conVal ~ age, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate risk perception
# create bar plot data
gldatlikage <- ptab %>%
  dplyr::group_by(age, CHAL1_likVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1_likVal = factor(CHAL1_likVal, levels = likeops$val,
                                      labels = likeops$ans)) %>%
  dplyr::mutate(age = factor(age, levels = c("18-34", "35-54", "55+")))

drive_risk_age = ggplot(gldatlikage, aes(CHAL1_likVal, Percent, fill = CHAL1_likVal)) +
  facet_grid(~age) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL1_likVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=likCols, labels = function(x) str_wrap(x, width = 20)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y= "Percent (%) of participants", 
       fill=str_wrap("How likely do you think you’ll see a year with 4 or more heavy rainfall events in the next 30 years?", 
                     width = 20)) #questionslist$Area_Freq$CHAL1.4

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1_likVal ~ age, data = ptab)
dunnTest(CHAL1_likVal ~ age, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1_likVal ~ age, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Flood scenario
# Decision
gldatfloodage <- protab %>%
  dplyr::group_by(age, CHAL2) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2 = factor(CHAL2, 
                               levels = 0:1,
                               labels = c("no", "yes"))) %>%
  dplyr::mutate(age = factor(age, levels = c("18-34", "35-54", "55+")))

# create grouped bar plot
flood_decage = gldatfloodage %>%
  ggplot(aes(CHAL2, Percent, fill = CHAL2)) +
  facet_grid(~age) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL2), vjust=-0.1, size = 3) +
  scale_fill_manual(values=driveCols[c(1,4)], labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y= "Percent (%) of participants", 
       fill="Buy flood insurance?")

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL2 ~ age, data = protab)
dunnTest(CHAL2 ~ age, data = protab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2 ~ age, data = protab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate confidence
ptab$CHAL2_conVal = confidence$val[match(ptab$CHAL2_con, confidence$ans)]

# create bar plot data
gldatconfloage <- ptab %>%
  dplyr::group_by(age, CHAL2_conVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2_conVal = factor(CHAL2_conVal, levels = confidence$val,
                                      labels = confidence$ans)) %>%
  dplyr::mutate(age = factor(age, levels = c("18-34", "35-54", "55+")))

# create grouped bar plot
flood_conage = ggplot(gldatconfloage, aes(CHAL2_conVal, Percent, fill = CHAL2_conVal)) +
  facet_grid(~age) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL2_conVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=conCols, labels = function(x) str_wrap(x, width = 18)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y= "Percent (%) of participants", 
       fill=str_wrap("How confident are you that you will be protected financially from a flood, based on your choice and the information given to you?", 
                     width = 20)) # questionslist$Area_Freq$CHAL2.3

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL2_conVal ~ age, data = ptab)
dunnTest(CHAL2_conVal ~ age, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2_conVal ~ age, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate risk perception
ptab$CHAL2_likVal = likeops$val[match(ptab$CHAL2_lik, likeops$ans)]

gldatlikfloage <- ptab %>%
  dplyr::group_by(age, CHAL2_likVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2_likVal = factor(CHAL2_likVal, levels = likeops$val,
                                      labels = likeops$ans)) %>%
  dplyr::mutate(age = factor(age, levels = c("18-34", "35-54", "55+")))

flood_riskage = ggplot(gldatlikfloage, aes(CHAL2_likVal, Percent, fill = CHAL2_likVal)) +
  facet_grid(~age) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL2_likVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=likCols, labels = function(x) str_wrap(x, width = 20)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y= "Percent (%) of participants", 
       fill=str_wrap("How likely do you think another flood will occur from a heavy rainfall event in the next 30 years?", 
                     width = 20)) # questionslist$Area_Freq$CHAL2.4
# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL2_likVal ~ age, data = ptab)
dunnTest(CHAL2_likVal ~ age, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2_likVal ~ age, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Driveway scenario -------------------------------------------------------
# Climate -----------------------------------------------------------------
# create bar plot data
gldat <- protab %>%
  dplyr::group_by(clim, CHAL1) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1 = factor(CHAL1, 
                               levels = 1:4,
                               labels = c("do nothing",
                                          "protect 2 events/yr", 
                                          "protect 4 events/yr",
                                          "pave the driveway\n(120 events)")))%>%
  dplyr::mutate(clim = factor(clim, levels = c("Lower literacy", "Neutral", 
                                               "Higher literacy")))

# create grouped bar plot
drive_dec = gldat %>%
  ggplot(aes(CHAL1, Percent, fill = CHAL1)) + facet_grid(~clim) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=driveCols, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  geom_text(aes(label = round(Percent), x=CHAL1), vjust=-0.1, size = 3) +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Climate science literacy", y= "Percent (%) of participants", 
       fill="Protection options")

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1 ~ clim, data = protab)
dunnTest(CHAL1 ~ clim, data = protab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1 ~ clim, data = protab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# chisq <- chisq.test(y=protab$CHAL2, x=protab$acc)
# chisq$observed   # observed counts (same as M)
# chisq$expected   # expected counts under the null
# chisq$residuals  # Pearson residuals
# chisq$stdres
# chisq$p.value
# chisq$statistic
# 
# contrib <- 100*chisq$residuals^2/chisq$statistic
# round(contrib, 3)

# Evaluate confidence
gldatcondrive <- ptab %>%
  dplyr::group_by(clim, CHAL1_conVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1_conVal = factor(CHAL1_conVal, levels = confidence$val,
                                      labels = confidence$ans)) %>%
  dplyr::mutate(clim = factor(clim, levels = c("Lower literacy", "Neutral", 
                                               "Higher literacy")))

# create grouped bar plot
drive_con = ggplot(gldatcondrive, aes(CHAL1_conVal, Percent, fill = CHAL1_conVal)) +
  facet_grid(~clim) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL1_conVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=conCols, labels = function(x) str_wrap(x, width = 18)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Climate science literacy", y= "Percent (%) of participants", 
       fill=str_wrap(questionslist$Area_Freq$CHAL1.3, width = 20))

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1_conVal ~ clim, data = ptab)
dunnTest(CHAL1_conVal ~ clim, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1_conVal ~ clim, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate risk perception
# create bar plot data
gldatlik <- ptab %>%
  dplyr::group_by(clim, CHAL1_likVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1_likVal = factor(CHAL1_likVal, levels = likeops$val,
                                      labels = likeops$ans)) %>%
  dplyr::mutate(clim = factor(clim, levels = c("Lower literacy", "Neutral", 
                                               "Higher literacy")))

drive_risk = ggplot(gldatlik, aes(CHAL1_likVal, Percent, fill = CHAL1_likVal)) +
  facet_grid(~clim) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL1_likVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=likCols, labels = function(x) str_wrap(x, width = 20)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Climate science literacy", y= "Percent (%) of participants", 
       fill=str_wrap("How likely do you think you’ll see a year with 4 or more heavy rainfall events in the next 30 years?", 
                     width = 20)) #questionslist$Area_Freq$CHAL1.4

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1_likVal ~ clim, data = ptab)
dunnTest(CHAL1_likVal ~ clim, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1_likVal ~ clim, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Flood scenario
# Decision
gldatflood <- protab %>%
  dplyr::group_by(clim, CHAL2) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2 = factor(CHAL2, 
                               levels = 0:1,
                               labels = c("no", "yes"))) %>%
  dplyr::mutate(clim = factor(clim, levels = c("Lower literacy", "Neutral", 
                                               "Higher literacy")))

# create grouped bar plot
flood_dec = gldatflood %>%
  ggplot(aes(CHAL2, Percent, fill = CHAL2)) +
  facet_grid(~clim) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL2), vjust=-0.1, size = 3) +
  scale_fill_manual(values=driveCols[c(1,4)], labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Climate science literacy", y= "Percent (%) of participants", 
       fill="Buy flood insurance?")

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL2 ~ clim, data = protab)
dunnTest(CHAL2 ~ clim, data = protab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2 ~ clim, data = protab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate confidence
ptab$CHAL2_conVal = confidence$val[match(ptab$CHAL2_con, confidence$ans)]

# create bar plot data
gldatconflo <- ptab %>%
  dplyr::group_by(clim, CHAL2_conVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2_conVal = factor(CHAL2_conVal, levels = confidence$val,
                                      labels = confidence$ans)) %>%
  dplyr::mutate(clim = factor(clim, levels = c("Lower literacy", "Neutral", 
                                               "Higher literacy")))

# create grouped bar plot
flood_con = ggplot(gldatconflo, aes(CHAL2_conVal, Percent, fill = CHAL2_conVal)) +
  facet_grid(~clim) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL2_conVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=conCols, labels = function(x) str_wrap(x, width = 18)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Climate science literacy", y= "Percent (%) of participants", 
       fill=str_wrap("How confident are you that you will be protected financially from a flood, based on your choice and the information given to you?", 
                     width = 20)) # questionslist$Area_Freq$CHAL2.3

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL2_conVal ~ clim, data = ptab)
dunnTest(CHAL2_conVal ~ clim, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2_conVal ~ clim, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate risk perception
ptab$CHAL2_likVal = likeops$val[match(ptab$CHAL2_lik, likeops$ans)]

gldatlikflo <- ptab %>%
  dplyr::group_by(clim, CHAL2_likVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2_likVal = factor(CHAL2_likVal, levels = likeops$val,
                                      labels = likeops$ans)) %>%
  dplyr::mutate(clim = factor(clim, levels = c("Lower literacy", "Neutral", 
                                               "Higher literacy")))

flood_risk = ggplot(gldatlikflo, aes(CHAL2_likVal, Percent, fill = CHAL2_likVal)) +
  facet_grid(~clim) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL2_likVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=likCols, labels = function(x) str_wrap(x, width = 20)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Climate science literacy", y= "Percent (%) of participants", 
       fill=str_wrap("How likely do you think another flood will occur from a heavy rainfall event in the next 30 years?", 
                     width = 20)) # questionslist$Area_Freq$CHAL2.4
# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL2_likVal ~ clim, data = ptab)
dunnTest(CHAL2_likVal ~ clim, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2_likVal ~ clim, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Politics ----------------------------------------------------------------
gldatpol <- protab %>%
  dplyr::group_by(political, CHAL1) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1 = factor(CHAL1, 
                               levels = 1:4,
                               labels = c("do nothing",
                                          "protect 2 events/yr", 
                                          "protect 4 events/yr",
                                          "pave the driveway\n(120 events)")))%>%
  dplyr::mutate(political = factor(political, levels = c("Conservative", "Neutral", 
                                               "Liberal")))

# create grouped bar plot
drive_decpol = gldatpol %>%
  ggplot(aes(CHAL1, Percent, fill = CHAL1)) + facet_grid(~political) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=driveCols, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  geom_text(aes(label = round(Percent), x=CHAL1), vjust=-0.1, size = 3) +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Political preference", y= "Percent (%) of participants", 
       fill="Protection options")

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1 ~ political, data = protab)
dunnTest(CHAL1 ~ political, data = protab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1 ~ political, data = protab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate confidence
gldatcondrivepol <- ptab %>%
  dplyr::group_by(political, CHAL1_conVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1_conVal = factor(CHAL1_conVal, levels = confidence$val,
                                      labels = confidence$ans)) %>%
  dplyr::mutate(political = factor(political, levels = c("Conservative", "Neutral", 
                                                         "Liberal")))

# create grouped bar plot
drive_conpol = ggplot(gldatcondrivepol, aes(CHAL1_conVal, Percent, fill = CHAL1_conVal)) +
  facet_grid(~political) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL1_conVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=conCols, labels = function(x) str_wrap(x, width = 18)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Political preference", y= "Percent (%) of participants", 
       fill=str_wrap(questionslist$Area_Freq$CHAL1.3, width = 20))

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1_conVal ~ political, data = ptab)
dunnTest(CHAL1_conVal ~ political, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1_conVal ~ political, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate risk perception
# create bar plot data
gldatlikpol <- ptab %>%
  dplyr::group_by(political, CHAL1_likVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1_likVal = factor(CHAL1_likVal, levels = likeops$val,
                                      labels = likeops$ans)) %>%
  dplyr::mutate(political = factor(political, levels = c("Conservative", "Neutral", 
                                               "Liberal")))

drive_riskpol = ggplot(gldatlikpol, aes(CHAL1_likVal, Percent, fill = CHAL1_likVal)) +
  facet_grid(~political) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL1_likVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=likCols, labels = function(x) str_wrap(x, width = 20)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Political preference", y= "Percent (%) of participants", 
       fill=str_wrap("How likely do you think you’ll see a year with 4 or more heavy rainfall events in the next 30 years?", 
                     width = 20)) #questionslist$Area_Freq$CHAL1.4

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1_likVal ~ political, data = ptab)
dunnTest(CHAL1_likVal ~ political, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1_likVal ~ political, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Flood scenario
# Decision
gldatfloodpol <- protab %>%
  dplyr::group_by(political, CHAL2) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2 = factor(CHAL2, 
                               levels = 0:1,
                               labels = c("no", "yes"))) %>%
  dplyr::mutate(political = factor(political, levels = c("Conservative", "Neutral", 
                                               "Liberal")))

# create grouped bar plot
flood_decpol = gldatfloodpol %>%
  ggplot(aes(CHAL2, Percent, fill = CHAL2)) +
  facet_grid(~political) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL2), vjust=-0.1, size = 3) +
  scale_fill_manual(values=driveCols[c(1,4)], labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Political preference", y= "Percent (%) of participants", 
       fill="Buy flood insurance?")

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL2 ~ political, data = protab)
dunnTest(CHAL2 ~ political, data = protab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2 ~ political, data = protab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate confidence
# create bar plot data
gldatconflopol <- ptab %>%
  dplyr::group_by(political, CHAL2_conVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2_conVal = factor(CHAL2_conVal, levels = confidence$val,
                                      labels = confidence$ans)) %>%
  dplyr::mutate(political = factor(political, levels = c("Conservative", "Neutral", 
                                               "Liberal")))
# create grouped bar plot
flood_conpol = ggplot(gldatconflopol, aes(CHAL2_conVal, Percent, fill = CHAL2_conVal)) +
  facet_grid(~political) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL2_conVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=conCols, labels = function(x) str_wrap(x, width = 18)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Political preference", y= "Percent (%) of participants", 
       fill=str_wrap("How confident are you that you will be protected financially from a flood, based on your choice and the information given to you?", 
                     width = 20)) # questionslist$Area_Freq$CHAL2.3

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL2_conVal ~ political, data = ptab)
dunnTest(CHAL2_conVal ~ political, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2_conVal ~ political, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate risk perception
gldatlikflopol <- ptab %>%
  dplyr::group_by(political, CHAL2_likVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2_likVal = factor(CHAL2_likVal, levels = likeops$val,
                                      labels = likeops$ans)) %>%
  dplyr::mutate(political = factor(political, levels = c("Conservative", "Neutral", 
                                               "Liberal")))

flood_riskpol = ggplot(gldatlikflopol, aes(CHAL2_likVal, Percent, fill = CHAL2_likVal)) +
  facet_grid(~political) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL2_likVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=likCols, labels = function(x) str_wrap(x, width = 20)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Political preference", y= "Percent (%) of participants", 
       fill=str_wrap("How likely do you think another flood will occur from a heavy rainfall event in the next 30 years?", 
                     width = 20)) # questionslist$Area_Freq$CHAL2.4

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL2_likVal ~ political, data = ptab)
dunnTest(CHAL2_likVal ~ political, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2_likVal ~ political, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Accuracy ----------------------------------------------------------------
gldatacc <- protab %>%
  dplyr::group_by(acc, CHAL1) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1 = factor(CHAL1, 
                               levels = 1:4,
                               labels = c("do nothing",
                                          "protect 2 events/yr", 
                                          "protect 4 events/yr",
                                          "pave the driveway\n(120 events)")))%>%
  dplyr::mutate(acc = factor(acc, levels = c("Below average", "Average", 
                                                         "Above average")))

# create grouped bar plot
drive_decacc = gldatacc %>%
  ggplot(aes(CHAL1, Percent, fill = CHAL1)) + facet_grid(~acc) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=driveCols, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  geom_text(aes(label = round(Percent), x=CHAL1), vjust=-0.1, size = 3) +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Interpretation", y= "Percent (%) of participants", 
       fill="Protection options")

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1 ~ acc, data = protab)
dunnTest(CHAL1 ~ acc, data = protab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1 ~ acc, data = protab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate confidence
gldatcondriveacc <- ptab %>%
  dplyr::group_by(acc, CHAL1_conVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1_conVal = factor(CHAL1_conVal, levels = confidence$val,
                                      labels = confidence$ans)) %>%
  dplyr::mutate(acc = factor(acc, levels = c("Below average", "Average", 
                                                         "Above average")))

# create grouped bar plot
drive_conacc = ggplot(gldatcondriveacc, aes(CHAL1_conVal, Percent, fill = CHAL1_conVal)) +
  facet_grid(~acc) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL1_conVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=conCols, labels = function(x) str_wrap(x, width = 18)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Interpretation", y= "Percent (%) of participants", 
       fill=str_wrap(questionslist$Area_Freq$CHAL1.3, width = 20))

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1_conVal ~ acc, data = ptab)
dunnTest(CHAL1_conVal ~ acc, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1_conVal ~ acc, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate risk perception
# create bar plot data
gldatlikacc <- ptab %>%
  dplyr::group_by(acc, CHAL1_likVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1_likVal = factor(CHAL1_likVal, levels = likeops$val,
                                      labels = likeops$ans)) %>%
  dplyr::mutate(acc = factor(acc, levels = c("Below average", "Average", 
                                                         "Above average")))

drive_riskacc = ggplot(gldatlikacc, aes(CHAL1_likVal, Percent, fill = CHAL1_likVal)) +
  facet_grid(~acc) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL1_likVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=likCols, labels = function(x) str_wrap(x, width = 20)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Interpretation", y= "Percent (%) of participants", 
       fill=str_wrap("How likely do you think you’ll see a year with 4 or more heavy rainfall events in the next 30 years?", 
                     width = 20)) #questionslist$Area_Freq$CHAL1.4

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1_likVal ~ acc, data = ptab)
dunnTest(CHAL1_likVal ~ acc, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1_likVal ~ acc, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Flood scenario
# Decision
gldatfloodacc <- protab %>%
  dplyr::group_by(acc, CHAL2) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2 = factor(CHAL2, 
                               levels = 0:1,
                               labels = c("no", "yes"))) %>%
  dplyr::mutate(acc = factor(acc, levels = c("Below average", "Average", 
                                                         "Above average")))

# create grouped bar plot
flood_decacc = gldatfloodacc %>%
  ggplot(aes(CHAL2, Percent, fill = CHAL2)) +
  facet_grid(~acc) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL2), vjust=-0.1, size = 3) +
  scale_fill_manual(values=driveCols[c(1,4)], labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Interpretation", y= "Percent (%) of participants", 
       fill="Buy flood insurance?")

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL2 ~ acc, data = protab)
dunnTest(CHAL2 ~ acc, data = protab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2 ~ acc, data = protab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate confidence
# create bar plot data
gldatconfloacc <- ptab %>%
  dplyr::group_by(acc, CHAL2_conVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2_conVal = factor(CHAL2_conVal, levels = confidence$val,
                                      labels = confidence$ans)) %>%
  dplyr::mutate(acc = factor(acc, levels = c("Below average", "Average", 
                                                         "Above average")))
# create grouped bar plot
flood_conacc = ggplot(gldatconfloacc, aes(CHAL2_conVal, Percent, fill = CHAL2_conVal)) +
  facet_grid(~acc) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL2_conVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=conCols, labels = function(x) str_wrap(x, width = 18)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Interpretation", y= "Percent (%) of participants", 
       fill=str_wrap("How confident are you that you will be protected financially from a flood, based on your choice and the information given to you?", 
                     width = 20)) # questionslist$Area_Freq$CHAL2.3

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL2_conVal ~ acc, data = ptab)
dunnTest(CHAL2_conVal ~ acc, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2_conVal ~ acc, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

# Evaluate risk perception
gldatlikfloacc <- ptab %>%
  dplyr::group_by(acc, CHAL2_likVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2_likVal = factor(CHAL2_likVal, levels = likeops$val,
                                      labels = likeops$ans)) %>%
  dplyr::mutate(acc = factor(acc, levels = c("Below average", "Average", 
                                                         "Above average")))

flood_riskacc = ggplot(gldatlikfloacc, aes(CHAL2_likVal, Percent, fill = CHAL2_likVal)) +
  facet_grid(~acc) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent), x=CHAL2_likVal), vjust=-0.1, size = 3) +
  scale_fill_manual(values=likCols, labels = function(x) str_wrap(x, width = 20)) +
  theme_bw() +
  theme(legend.title = element_text(size=9), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Interpretation", y= "Percent (%) of participants", 
       fill=str_wrap("How likely do you think another flood will occur from a heavy rainfall event in the next 30 years?", 
                     width = 20)) # questionslist$Area_Freq$CHAL2.4

# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL2_likVal ~ acc, data = ptab)
dunnTest(CHAL2_likVal ~ acc, data = ptab, method = "holm")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2_likVal ~ acc, data = ptab,
          function(x) c(mean = mean(x), med = median(x),  sd = sd(x)))

##########################################################################
# Textual responses
##########################################################################
color_codes = data.frame(code = unique(topics$AI.Analysis.of.Scenario.Justification..ChatGPT.), 
                         colors = brewer.pal(length(unique(topics$AI.Analysis.of.Scenario.Justification..ChatGPT.)), 
                                             "Paired"))
seqreasoncol = brewer.pal(3, "YlGnBu")#"Greys")

# Fix the I don't understand
prochoices$Primary.cycle.code.1[which(prochoices$Primary.cycle.code.1 == "I don\xd5t understand")] = "I don't understand"

# Climate codes -----------------------------------------------------------
sec_drive_clim = group_perc(textchoices=prochoices, codename="Primary.cycle.code", 
                            varname="clim", groupstr=c("Lower literacy", "Neutral", "Higher literacy"), 
                            subgroupstr=c("Low", "Neutral", "High"), color_codes, 
                            scenname="driveway", dirname="paper2/")
sec_flood_clim = group_perc(textchoices=prochoices, codename="Primary.cycle.code.1", 
                            varname="clim", groupstr=c("Lower literacy", "Neutral", "Higher literacy"), 
                            subgroupstr=c("Low", "Neutral", "High"), color_codes, 
                            scenname="flood", dirname="paper2/")

# Grouped secondary aes(reorder_within(topic, percent, clim)
drive_sec_reasonclim = ggplot(sec_drive_clim$sec_text, aes(topic, percent, fill = topic)) +
  geom_bar(stat = 'identity', position=position_dodge(), colour="black", linewidth = 0.1) +
  scale_fill_manual(values=color_codes$colors, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  geom_text(aes(label = round(percent), x=topic), vjust=-0.1, size = 3) +
  facet_wrap(~groups, scales = "free_x") +
  theme(legend.title = element_text(size=9), legend.text = element_text(size=8), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Climate science literacy", y= "Percent (%) of participants", 
       fill="Reason")

# Insurance scenario 
flood_sec_reasonclim = ggplot(sec_flood_clim$sec_text, aes(topic, percent, fill = topic)) +
  geom_bar(stat = 'identity', position=position_dodge(), colour="black", linewidth = 0.1) +
  scale_fill_manual(values=color_codes$colors, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  geom_text(aes(label = round(percent), x=topic), vjust=-0.1, size = 3) +
  facet_wrap(~groups, scales = "free_x") +
  theme(legend.title = element_text(size=9), legend.text = element_text(size=8), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Climate science literacy", y= "Percent (%) of participants", 
       fill="Reason")

sec_drive_clim$sec_text$scenario = "Driveway washout scenario"
sec_flood_clim$sec_text$scenario = "Flood insurance scenario"

boundscenclim = rbind(sec_drive_clim$sec_text, sec_flood_clim$sec_text)

# Part of Fig. 4
reason_clim = ggplot(boundscenclim, aes(fill=groups,x=percent, y=str_wrap(topic, 30))) + 
  geom_bar(position="dodge", stat="identity", colour="black", linewidth = 0.1) +
  labs(y = "", x= "Percent (%) of participants", 
       fill="") + scale_fill_manual(values=seqreasoncol) + theme_bw() +
  geom_text(aes(label = round(percent)), colour = "black", size = 3, hjust = -0.1, position = position_dodge(.9))+
  theme(legend.title = element_text(size=9), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "inside", 
        legend.position.inside = c(0.92, 0.3), 
        legend.background = element_rect(fill = NA, colour = NA)) + 
  facet_wrap(~scenario, ncol=2)

# Create color table S. Tab
clim_tbl <- gt(sec_drive_clim$pertab[,-ncol(sec_drive_clim$pertab)]) |>
  tab_header(title = "Driveway washout scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

clim_fl_tbl <- gt(sec_flood_clim$pertab[,-ncol(sec_flood_clim$pertab)]) |>
  tab_header(title = "Flood insurance scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

clim_tb = gt_group(clim_tbl, clim_fl_tbl)
gtsave(clim_tb, "paper2/clim_reasoning.docx")

# Age codes -----------------------------------------------------------
sec_drive_age = group_perc(textchoices=prochoices, codename="Primary.cycle.code", 
                           varname="age", groupstr=c("18-34", "35-54", "55+"), 
                           subgroupstr=c("18-34", "35-54", "55+"), color_codes, 
                           scenname="driveway", dirname="paper2/")
sec_flood_age = group_perc(textchoices=prochoices, codename="Primary.cycle.code.1", 
                           varname="age", groupstr=c("18-34", "35-54", "55+"), 
                           subgroupstr=c("18-34", "35-54", "55+"), color_codes, 
                           scenname="flood", dirname="paper2/")
# Grouped secondary
drive_sec_reasonage = ggplot(sec_drive_age$sec_text, aes(topic, percent, fill = topic)) +
  geom_bar(stat = 'identity', position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_text(aes(label = round(percent), x=topic), vjust=-0.1, size = 3) +
  scale_fill_manual(values=color_codes$colors, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  facet_wrap(~groups, scales = "free_x") +
  theme(legend.title = element_text(size=9), legend.text = element_text(size=8), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y= "Percent (%) of participants", 
       fill="Reason")

# Insurance scenario 
flood_sec_reasonage = ggplot(sec_flood_age$sec_text, aes(topic, percent, fill = topic)) +
  geom_bar(stat = 'identity', position=position_dodge(), colour="black", linewidth = 0.1) +
  scale_fill_manual(values=color_codes$colors, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  geom_text(aes(label = round(percent), x=topic), vjust=-0.1, size = 3) +
  facet_wrap(~groups, scales = "free_x") +
  theme(legend.title = element_text(size=9), legend.text = element_text(size=8), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y= "Percent (%) of participants", 
       fill="Reason")

sec_drive_age$sec_text$scenario = "Driveway washout scenario"
sec_flood_age$sec_text$scenario = "Flood insurance scenario"

boundscenage = rbind(sec_drive_age$sec_text, sec_flood_age$sec_text)
# geom_text(aes(label = percent, y=topic), 
          # hjust=0, size = 3) +

reason_age = ggplot(boundscenage, aes(fill=groups,x=percent, y=str_wrap(topic, 30))) + 
  geom_bar(position="dodge", stat="identity", colour="black", linewidth = 0.1) +
  labs(y = "", x= "Percent (%) of participants", 
       fill="") + scale_fill_manual(values=seqreasoncol) + theme_bw() +
  geom_text(aes(label = round(percent)), colour = "black", size = 3, hjust = -0.1, position = position_dodge(.9))+
  # geom_text(aes(label = round(percent), x=percent, fill=topic), vjust=-0.1, size = 3) +
  theme(legend.title = element_text(size=9), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "inside", 
        legend.position.inside = c(0.92, 0.3), 
        legend.background = element_rect(fill = NA, colour = NA)) +
  facet_wrap(~scenario, ncol=2)

# Create color table S. Tab
age_tbl <- gt(sec_drive_age$pertab[,-ncol(sec_drive_age$pertab)]) |>
  tab_header(title = "Driveway washout scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

age_fl_tbl <- gt(sec_flood_age$pertab[,-ncol(sec_flood_age$pertab)]) |>
  tab_header(title = "Flood insurance scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

age_tb = gt_group(age_tbl, age_fl_tbl)
gtsave(age_tb, "paper2/age_reasoning.docx")

# Political codes -----------------------------------------------------------
sec_drive_pol = group_perc(textchoices=prochoices, codename="Primary.cycle.code", 
                           varname="political", groupstr=c("Conservative", "Neutral", "Liberal"), 
                           subgroupstr=c("Conservative", "Neutral", "Liberal"), color_codes, 
                           scenname="driveway", dirname="paper2/")
sec_flood_pol = group_perc(textchoices=prochoices, codename="Primary.cycle.code.1", 
                           varname="political", groupstr=c("Conservative", "Neutral", "Liberal"), 
                           subgroupstr=c("Conservative", "Neutral", "Liberal"), color_codes, 
                           scenname="flood", dirname="paper2/")

drive_sec_reasonpol = ggplot(sec_drive_pol$sec_text, aes(topic, percent, fill = topic)) +
  geom_bar(stat = 'identity', position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_text(aes(label = round(percent), x=topic), vjust=-0.1, size = 3) +
  scale_fill_manual(values=color_codes$colors, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  facet_wrap(groups, scales = "free_x") +
  theme(legend.title = element_text(size=9), legend.text = element_text(size=8), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Political preference", y= "Percent (%) of participants", 
       fill="Reason")

# Insurance scenario 
flood_sec_reasonpol = ggplot(sec_flood_pol$sec_text, aes(topic, percent, fill = topic)) +
  geom_bar(stat = 'identity', position=position_dodge(), colour="black", linewidth = 0.1) +
  scale_fill_manual(values=color_codes$colors, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  geom_text(aes(label = round(percent), x=topic), vjust=-0.1, size = 3) +
  facet_wrap(~groups, scales = "free_x") +
  theme(legend.title = element_text(size=9), legend.text = element_text(size=8), axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Political preference", y= "Percent (%) of participants", 
       fill="Reason")

sec_drive_pol$sec_text$scenario = "Driveway washout scenario"
sec_flood_pol$sec_text$scenario = "Flood insurance scenario"

boundscenpol = rbind(sec_drive_pol$sec_text, sec_flood_pol$sec_text)

reason_pol = ggplot(boundscenpol, aes(fill=groups,x=percent, y=str_wrap(topic, 30))) + 
  geom_bar(position="dodge", stat="identity", colour="black", linewidth = 0.1) +
  labs(y = "", x= "Percent (%) of participants", 
       fill="") + scale_fill_manual(values=seqreasoncol) + theme_bw() +
  geom_text(aes(label = round(percent)), colour = "black", size = 3, hjust = -0.1, position = position_dodge(.9))+
  theme(legend.title = element_text(size=9), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "inside", 
        legend.position.inside = c(0.93, 0.3), 
        legend.background = element_rect(fill = NA, colour = NA)) + 
  facet_wrap(~scenario, ncol=2)

# Create color table S. Tab
pol_tbl <- gt(sec_drive_pol$pertab[,-ncol(sec_drive_pol$pertab)]) |>
  tab_header(title = "Driveway washout scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

pol_fl_tbl <- gt(sec_flood_pol$pertab[,-ncol(sec_flood_pol$pertab)]) |>
  tab_header(title = "Flood insurance scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

pol_tb = gt_group(pol_tbl, pol_fl_tbl)
gtsave(pol_tb, "paper2/pol_reasoning.docx")

# Accuracy codes -----------------------------------------------------------
sec_drive_acc = group_perc(textchoices=prochoices, codename="Primary.cycle.code", 
                           varname="acc", groupstr=c("Below average", "Average", "Above average"), 
                           subgroupstr=c("Below", "Average", "Above"), color_codes, 
                           scenname="driveway", dirname="paper2/")

# Insurance scenario 
sec_flood_acc = group_perc(textchoices=prochoices, codename="Primary.cycle.code.1", 
                           varname="acc", groupstr=c("Below average", "Average", "Above average"), 
                           subgroupstr=c("Below", "Average", "Above"), color_codes, 
                           scenname="flood", dirname="paper2/")

sec_drive_acc$sec_text$scenario = "Driveway washout scenario"
sec_flood_acc$sec_text$scenario = "Flood insurance scenario"

boundscen = rbind(sec_drive_acc$sec_text, sec_flood_acc$sec_text)

reason_acc = ggplot(boundscen, aes(fill=groups,x=percent, y=str_wrap(topic, 30))) + 
  geom_bar(position="dodge", stat="identity", colour="black", linewidth = 0.1) +
  labs(y = "", x= "Percent (%) of participants", 
       fill="") + scale_fill_manual(values=seqreasoncol) + theme_bw() +
  geom_text(aes(label = round(percent)), colour = "black", size = 3, hjust = -0.1, position = position_dodge(.9))+
  theme(legend.title = element_text(size=9), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "inside", 
        legend.position.inside = c(0.91, 0.3), 
        legend.background = element_rect(fill = NA, colour = NA)) + 
  facet_wrap(~scenario, ncol=2)

# Create color table S. Tab
acc_tbl <- gt(sec_drive_acc$pertab[,-ncol(sec_drive_acc$pertab)]) |>
  tab_header(title = "Driveway washout scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

acc_fl_tbl <- gt(sec_flood_acc$pertab[,-ncol(sec_flood_acc$pertab)]) |>
  tab_header(title = "Flood insurance scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

acc_tb = gt_group(acc_tbl, acc_fl_tbl)
gtsave(acc_tb, "paper2/acc_reasoning.docx")

# Other characteristics ---------------------------------------------------
all.size.df<- as.data.frame(cbind(all.size.df, ptab$CHAL1_likVal, ptab$CHAL2_likVal, ptab$CHAL1_conVal,
                                  ptab$CHAL2_conVal))

char_prochoices = prochoices
char_prochoices = as.data.frame(cbind(char_prochoices, all.size.df$region, all.size.df$division, all.size.df$gender, 
                                      all.size.df$latino, all.size.df$racelarge, all.size.df$edu, 
                                      all.size.df$work, incGroup))
colnames(char_prochoices)[18:24] = c("region", "division", "gender", "latino", "racelarge", "edu", "work")

# Region
sec_drive_region = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code", 
                              varname="region", groupstr=c("South", "Midwest", "Northeast", "West"), 
                              subgroupstr=c("South", "Midwest", "Northeast", "West"), color_codes, 
                              scenname="driveway", dirname="paper2/")
sec_flood_region = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code.1", 
                              varname="region", groupstr=c("South", "Midwest", "Northeast", "West"), 
                              subgroupstr=c("South", "Midwest", "Northeast", "West"), color_codes, 
                              scenname="flood", dirname="paper2/")

gt_tbl <- gt(sec_drive_region$pertab[,-ncol(sec_drive_region$pertab)]) |>
  tab_header(title = "Driveway washout scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

gt_fl_tbl <- gt(sec_flood_region$pertab[,-ncol(sec_flood_region$pertab)]) |>
  tab_header(title = "Flood insurance scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

region_tb = gt_group(gt_tbl, gt_fl_tbl)
gtsave(region_tb, "paper2/region_reasoning.docx")

# Division
# sec_drive_division = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code", 
#                               varname="division", groupstr=c("South Atlantic", "West North Central", "East North Central", 
#                                                            "Middle Atlantic", "Mountain", "West South Central", "Pacific",
#                                                            "East South Central", "New England"), 
#                               subgroupstr=c("SouthAtlantic", "WestNorthCentral", "EastNorthCentral", 
#                                             "MiddleAtlantic", "Mountain", "WestSouthCentral", "Pacific",
#                                             "EastSouthCentral", "NewEngland"), color_codes, 
#                               scenname="driveway", dirname="paper2/")
# sec_flood_division = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code.1", 
#                               varname="division", groupstr=c("South Atlantic", "West North Central", "East North Central", 
#                                                            "Middle Atlantic", "Mountain", "West South Central", "Pacific",
#                                                            "East South Central", "New England"), 
#                               subgroupstr=c("SouthAtlantic", "WestNorthCentral", "EastNorthCentral", 
#                                             "MiddleAtlantic", "Mountain", "WestSouthCentral", "Pacific",
#                                             "EastSouthCentral", "NewEngland"), color_codes, 
#                               scenname="flood", dirname="paper2/")

sec_drive_division = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code", 
                                varname="division", groupstr=c("New England", "Middle Atlantic", "South Atlantic", "East South Central", 
                                                               "West South Central", "Mountain", "Pacific", "West North Central", 
                                                               "East North Central"), 
                                subgroupstr=c("NewEngland", "MiddleAtlantic", "SouthAtlantic", "EastSouthCentral", 
                                              "WestSouthCentral", "Mountain", "Pacific", "WestNorthCentral", 
                                              "EastNorthCentral"), color_codes, 
                                scenname="driveway", dirname="paper2/")
sec_flood_division = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code.1", 
                                varname="division", groupstr=c("New England", "Middle Atlantic", "South Atlantic", "East South Central", 
                                                               "West South Central", "Mountain", "Pacific", "West North Central", 
                                                               "East North Central"), 
                                subgroupstr=c("NewEngland", "MiddleAtlantic", "SouthAtlantic", "EastSouthCentral", 
                                              "WestSouthCentral", "Mountain", "Pacific", "WestNorthCentral", 
                                              "EastNorthCentral"), color_codes, 
                                scenname="flood", dirname="paper2/")

gt_tbl <- gt(sec_drive_division$pertab[,-ncol(sec_drive_division$pertab)]) |>
  tab_header(title = "Driveway washout scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

gt_fl_tbl <- gt(sec_flood_division$pertab[,-ncol(sec_flood_division$pertab)]) |>
  tab_header(title = "Flood insurance scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

division_tb = gt_group(gt_tbl, gt_fl_tbl)
gtsave(division_tb, "paper2/region_division_grouped.docx")

# Gender
sec_drive_gender = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code", 
                              varname="gender", groupstr=c("Male", "Female", 
                                                           "Non-binary / third gender", 
                                                           "Prefer to self-describe", "Prefer not to say"), 
                              subgroupstr=c("Male", "Female", "Non-binary", "self-describe", "No"), color_codes, 
                              scenname="driveway", dirname="paper2/")
sec_flood_gender = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code.1", 
                              varname="gender", groupstr=c("Male", "Female", 
                                                           "Non-binary / third gender", 
                                                           "Prefer to self-describe", "Prefer not to say"), 
                              subgroupstr=c("Male", "Female", "Non-binary", "self-describe", "No"), color_codes, 
                              scenname="flood", dirname="paper2/")

gend_tbl <- gt(sec_drive_gender$pertab[,-ncol(sec_drive_gender$pertab)]) |>
  tab_header(title = "Driveway washout scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

gend_fl_tbl <- gt(sec_flood_gender$pertab[,-ncol(sec_flood_gender$pertab)]) |>
  tab_header(title = "Flood insurance scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

gender_tb = gt_group(gend_tbl, gend_fl_tbl)
gtsave(gender_tb, "paper2/gender_reasoning.docx")

# Latino
sec_drive_latino = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code", 
                              varname="latino", groupstr=c("No", "Yes"), subgroupstr=c("No", "Yes"), 
                              color_codes, scenname="driveway", dirname="paper2/")
sec_flood_latino = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code.1", 
                              varname="latino", groupstr=c("No", "Yes"), subgroupstr=c("No", "Yes"), 
                              color_codes, scenname="flood", dirname="paper2/")

latino_tbl <- gt(sec_drive_latino$pertab[,-ncol(sec_drive_latino$pertab)]) |>
  tab_header(title = "Driveway washout scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

latino_fl_tbl <- gt(sec_flood_latino$pertab[,-ncol(sec_flood_latino$pertab)]) |>
  tab_header(title = "Flood insurance scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

latino_tb = gt_group(latino_tbl, latino_fl_tbl)
gtsave(latino_tb, "paper2/latino_reasoning.docx")

# Race
sec_drive_race = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code", 
                            varname="racelarge", groupstr=c("white", "black", "asian", "indian", 
                                                            "islander", "other", "prefer"), 
                            subgroupstr=c("White", "Black", "Asian", "Indian", "Islander", "Other", "No"), 
                            color_codes, scenname="driveway", dirname="paper2/")
sec_flood_race = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code.1", 
                            varname="racelarge", groupstr=c("white", "black", "asian", "indian", 
                                                            "islander", "other", "prefer"), 
                            subgroupstr=c("White", "Black", "Asian", "Indian", "Islander", "Other", "No"), 
                            color_codes, scenname="flood", dirname="paper2/")

race_tbl <- gt(sec_drive_race$pertab[,-ncol(sec_drive_race$pertab)]) |>
  tab_header(title = "Driveway washout scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

race_fl_tbl <- gt(sec_flood_race$pertab[,-ncol(sec_flood_race$pertab)]) |>
  tab_header(title = "Flood insurance scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

race_tb = gt_group(race_tbl, race_fl_tbl)
gtsave(race_tb, "paper2/race_reasoning.docx")

# EDU
sec_drive_edu = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code", 
                           varname="edu", groupstr=c("Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS etc.)", 
                                                     "Bachelor’s degree", "Associates or technical degree", "Some college, but no degree", 
                                                     "High school diploma or GED", "Some high school or less", "Prefer not to say"), 
                           subgroupstr=c("Graduate", "Bachelors", "Associates", "Some-college", "High-school", "Some-high-school", "Prefer"), 
                           color_codes, scenname="driveway", dirname="paper2/")
sec_flood_edu = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code.1", 
                           varname="edu", groupstr=c("Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS etc.)", 
                                                     "Bachelor’s degree", "Associates or technical degree", "Some college, but no degree", 
                                                     "High school diploma or GED", "Some high school or less", "Prefer not to say"), 
                           subgroupstr=c("Graduate", "Bachelors", "Associates", "Some-college", "High-school", "Some-high-school", "Prefer"), 
                           color_codes, scenname="flood", dirname="paper2/")

edu_tbl <- gt(sec_drive_edu$pertab[,-ncol(sec_drive_edu$pertab)]) |>
  tab_header(title = "Driveway washout scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

edu_fl_tbl <- gt(sec_flood_edu$pertab[,-ncol(sec_flood_edu$pertab)]) |>
  tab_header(title = "Flood insurance scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

edu_tb = gt_group(edu_tbl, edu_fl_tbl)
gtsave(edu_tb, "paper2/edu_reasoning.docx")

# Work
sec_drive_work = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code", 
                            varname="work", groupstr=c("Other", "Retired", "Working full-time", "Working part-time", 
                                                       "A homemaker or stay-at-home parent", "Student", "Unemployed and looking for work"), 
                            subgroupstr=c("Other", "Retired", "full-time", "part-time", "homemaker", "Student", "Unemployed"), 
                            color_codes, scenname="driveway", dirname="paper2/")
sec_flood_work = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code.1", 
                            varname="work", groupstr=c("Other", "Retired", "Working full-time", "Working part-time", 
                                                       "A homemaker or stay-at-home parent", "Student", "Unemployed and looking for work"), 
                            subgroupstr=c("Other", "Retired", "full-time", "part-time", "homemaker", "Student", "Unemployed"), 
                            color_codes, scenname="flood", dirname="paper2/")

work_tbl <- gt(sec_drive_work$pertab[,-ncol(sec_drive_work$pertab)]) |>
  tab_header(title = "Driveway washout scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

work_fl_tbl <- gt(sec_flood_work$pertab[,-ncol(sec_flood_work$pertab)]) |>
  tab_header(title = "Flood insurance scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

work_tb = gt_group(work_tbl, work_fl_tbl)
gtsave(work_tb, "paper2/work_reasoning.docx")

# Income
sec_drive_income = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code", 
                              varname="incGroup", groupstr=c("Less than 25,000", "25,000-49,999", "50,000-74,999",
                                                             "75,000-99,999", "100,000-149,999", "150,000 or more"), 
                              subgroupstr=c("<25,000", "25,000-49,999", "50,000-74,999",
                                            "75,000-99,999", "100,000-149,999", "150,000+"), 
                              color_codes, scenname="driveway", dirname="paper2/")
sec_flood_income = group_perc(textchoices=char_prochoices, codename="Primary.cycle.code.1", 
                              varname="incGroup", groupstr=c("Less than 25,000", "25,000-49,999", "50,000-74,999",
                                                             "75,000-99,999", "100,000-149,999", "150,000 or more"), 
                              subgroupstr=c("<25,000", "25,000-49,999", "50,000-74,999",
                                            "75,000-99,999", "100,000-149,999", "150,000+"), 
                              color_codes, scenname="flood", dirname="paper2/")

income_tbl <- gt(sec_drive_income$pertab[,-ncol(sec_drive_income$pertab)]) |>
  tab_header(title = "Driveway washout scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

income_fl_tbl <- gt(sec_flood_income$pertab[,-ncol(sec_flood_income$pertab)]) |>
  tab_header(title = "Flood insurance scenario (% of participants)") |>
  data_color(method = "numeric", palette = "inferno", domain = c(0,75))

income_tb = gt_group(income_tbl, income_fl_tbl)
gtsave(income_tb, "paper2/income_reasoning.docx")

# -------------------------------------------------------------------------



# orderedcol_floacc = sec_text_floodacc$color[order(sec_text_floodacc$percent[sec_text_floodacc$acc == "Above average"])]

# # Grouped secondary
# flood_sec_reasonacc = ggplot(sec_text_floodacc$sec, aes(topic, percent, fill = topic)) +
#   geom_bar(stat = 'identity', position=position_dodge(), colour="black", linewidth = 0.1) +
#   geom_text(aes(label = round(percent), x=topic), vjust=-0.1, size = 3) +
#   scale_fill_manual(values=color_codes$colors, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
#   facet_wrap(~acc, scales = "free_x") +
#   theme(legend.title = element_text(size=9), legend.text = element_text(size=8), axis.text.x=element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   labs(x = "Interpretation", y= "Percent (%) of participants", 
#        fill="Reason")

# png(file="paper2/Fig_Decision_Accuracy.png", family="Helvetica", res=300,
#     units="in", width=maximum_width, height=column_height*4, pointsize=10)
# plot_grid(drive_decacc, flood_decacc, drive_conacc, flood_conacc, drive_riskacc, 
#           flood_riskacc, drive_sec_reasonacc, flood_sec_reasonacc,
#           nrow=4, labels = "auto") # , align = "h", axis = "b"
# dev.off()
# 
# drive_dec + geom_text(aes(label = paste0(round(Percent), "%"), y=Percent), position = position_stack(vjust = 0.3), size = 3)
# 
# png(file="paper2/Fig_Decision_protection.png", family="Helvetica", res=300,
#     units="in", width=maximum_width, height=column_height*4, pointsize=10)
# plot_grid(drive_dec_full +ylim(0,55), flood_dec_full+ylim(0,90),
#           drive_dec + theme(legend.position = "none")+ylim(0,55), 
#           flood_dec + theme(legend.position = "none")+ylim(0,90), 
#           drive_decpol + theme(legend.position = "none")+ylim(0,55), 
#           flood_decpol + theme(legend.position = "none")+ylim(0,90), 
#           drive_decacc + theme(legend.position = "none")+ylim(0,55), 
#           flood_decacc + theme(legend.position = "none")+ylim(0,90), 
#           nrow=4, labels = "auto" , align = "h", axis = "b")
# dev.off()

png(file="paper2/Fig_DecisionMaking.png", family="Helvetica", res=300,
    units="in", width=maximum_width, height=column_height*4, pointsize=10)
plot_grid(drive_risk_full, flood_risk_full, 
          drive_dec_full, flood_dec_full,
          drive_con_full, flood_con_full, 
          drive_txt_full, flood_txt_full,
          nrow=4, labels = "auto") # , align = "h", axis = "b"
dev.off()

legend_a <- get_legend(addSmallLegend(drive_dec_age) + theme(legend.position="bottom"))
legend_b <- get_legend(flood_decage + theme(legend.position="bottom"))

png(file="paper2/Fig_Decision_protectionleg.png", family="Helvetica", res=300,
    units="in", width=maximum_width, height=column_height*4, pointsize=10)
plot_grid(legend_a, legend_b,
          drive_dec_age + theme(legend.position = "none")+ylim(0,55), 
          flood_decage+ theme(legend.position = "none")+ylim(0,90),
          drive_dec + theme(legend.position = "none")+ylim(0,55), 
          flood_dec + theme(legend.position = "none")+ylim(0,90), 
          drive_decpol + theme(legend.position = "none")+ylim(0,55), 
          flood_decpol + theme(legend.position = "none")+ylim(0,90), 
          drive_decacc + theme(legend.position = "none")+ylim(0,55), 
          flood_decacc + theme(legend.position = "none")+ylim(0,90), 
          nrow=5, labels = c("", "", letters[1:8]) , align = "h", axis = "b", 
          rel_heights = c(.25, 1, 1, 1, 1))
dev.off()

# png(file="paper2/Fig_Decision_protectionage.png", family="Helvetica", res=300,
#     units="in", width=maximum_width, height=column_height*4, pointsize=10)
# plot_grid(drive_dec_age +theme(legend.position = "top")+ylim(0,55), flood_decage+theme(legend.position = "top")+ylim(0,90),
#           drive_dec + theme(legend.position = "none")+ylim(0,55), 
#           flood_dec + theme(legend.position = "none")+ylim(0,90), 
#           drive_decpol + theme(legend.position = "none")+ylim(0,55), 
#           flood_decpol + theme(legend.position = "none")+ylim(0,90), 
#           drive_decacc + theme(legend.position = "none")+ylim(0,55), 
#           flood_decacc + theme(legend.position = "none")+ylim(0,90), 
#           nrow=4, labels = "auto" , align = "h", axis = "b")
# dev.off()

legend_b <- get_legend(flood_conage + labs(fill="Confidence:") + theme(legend.position="bottom"))

conplot = plot_grid(drive_conage + theme(legend.position = "none", plot.title = element_text(hjust=0.5))+ylim(0,40)+ggtitle("Driveway washout scenario"),
                    flood_conage + theme(legend.position = "none", plot.title = element_text(hjust=0.5))+ylim(0,40)+ggtitle("Flood insurance scenario"),
                    drive_con + theme(legend.position = "none")+ylim(0,40), 
                    flood_con + theme(legend.position = "none")+ylim(0,40), 
                    drive_conpol + theme(legend.position = "none")+ylim(0,40), 
                    flood_conpol + theme(legend.position = "none")+ylim(0,40), 
                    drive_conacc + theme(legend.position = "none")+ylim(0,40), 
                    flood_conacc + theme(legend.position = "none")+ylim(0,40), 
                    nrow=4, labels = "auto", rel_heights = c(1.1, 1, 1, 1))

png(file="paper2/Fig_Decision_confidenceleg.png", family="Helvetica", res=300,
    units="in", width=maximum_width, height=column_height*4, pointsize=10)
plot_grid(legend_b, conplot, 
          nrow=2, labels = c("", ""), 
          rel_heights = c(.05, 1)) # , align = "h", axis = "b"
dev.off()

legend_b <- get_legend(flood_riskage + labs(fill="Likelihood:") + theme(legend.position="bottom"))

likplot = plot_grid(drive_risk_age+ theme(legend.position = "none", plot.title = element_text(hjust=0.5))+ylim(0,40)+ggtitle("Driveway washout scenario"), 
                    flood_riskage+theme(legend.position = "none", plot.title = element_text(hjust=0.5))+ylim(0,40)+ggtitle("Flood insurance scenario"),
          drive_risk + theme(legend.position = "none")+ylim(0,40), 
          flood_risk + theme(legend.position = "none")+ylim(0,40), 
          drive_riskpol + theme(legend.position = "none")+ylim(0,40), 
          flood_riskpol + theme(legend.position = "none")+ylim(0,40), 
          drive_riskacc + theme(legend.position = "none")+ylim(0,40), 
          flood_riskacc + theme(legend.position = "none")+ylim(0,40),
          nrow=4, labels = "auto", rel_heights = c(1.1, 1, 1, 1))


png(file="paper2/Fig_Decision_riskleg.png", family="Helvetica", res=300,
    units="in", width=maximum_width, height=column_height*4, pointsize=10)
plot_grid(legend_b, likplot,
          nrow=2, labels = c("", ""), 
          rel_heights = c(.06, 1)) # , align = "h", axis = "b"
dev.off()

# png(file="paper2/Fig_Decision_reason.png", family="Helvetica", res=300,
#     units="in", width=maximum_width, height=column_height*4, pointsize=10)
# plot_grid(drive_txt_full+ylim(0,50), flood_txt_full+ylim(0,50),
#           drive_sec_reasonage + theme(legend.position = "none")+ylim(0,50), 
#           flood_sec_reasonage + theme(legend.position = "none")+ylim(0,50),
#           drive_sec_reasonclim + theme(legend.position = "none")+ylim(0,50), 
#           flood_sec_reasonclim + theme(legend.position = "none")+ylim(0,50),
#           drive_sec_reasonpol + theme(legend.position = "none")+ylim(0,50), 
#           flood_sec_reasonpol + theme(legend.position = "none")+ylim(0,50),
#           drive_sec_reasonacc + theme(legend.position = "none")+ylim(0,50), 
#           flood_sec_reasonacc + theme(legend.position = "none")+ylim(0,50),
#           nrow=4, labels = "auto") # , align = "h", axis = "b"
# dev.off()

png(file="paper2/Fig_Decision_reasonleg.png", family="Helvetica", res=300,
    units="in", width=maximum_width, height=column_height*4, pointsize=10)
plot_grid(reason_age+labs(y="Age"), reason_clim+labs(y="Climate science literacy"), 
          reason_pol+labs(y="Political affiliation"), 
          reason_acc+labs(y="Interpretation"),
          nrow=4, labels = "auto") # , align = "h", axis = "b"
dev.off()

# Primary graph -----------------------------------------------------------
# drive_age_prim = grouped_primary(topics, sec_text_age, subgroupstr=c("18-34", "35-54", "55+"), color_codes, 
#                                  groupstr=c("18-34", "35-54", "55+"))
# flood_age_prim = grouped_primary(topics, sec_text_floodage, subgroupstr=c("18-34", "35-54", "55+"), color_codes, 
#                                  groupstr=c("18-34", "35-54", "55+"))
# 
# drive_clim_prim = grouped_primary(topics, sec_text_clim, subgroupstr=c("Low", "Neutral", "High"), color_codes, 
#                                  groupstr=c("Lower literacy", "Neutral", "Higher literacy"))
# flood_clim_prim = grouped_primary(topics, sec_text_floodclim, subgroupstr=c("Low", "Neutral", "High"), color_codes, 
#                                  groupstr=c("Lower literacy", "Neutral", "Higher literacy"))
# 
# drive_pol_prim = grouped_primary(topics, sec_text_pol, subgroupstr=c("Conservative", "Neutral", "Liberal"), color_codes, 
#                                  groupstr=c("Conservative", "Neutral", "Liberal"))
# flood_pol_prim = grouped_primary(topics, sec_text_floodpol, subgroupstr=c("Conservative", "Neutral", "Liberal"), color_codes, 
#                                  groupstr=c("Conservative", "Neutral", "Liberal"))
# 
# drive_acc_prim = grouped_primary(topics, sec_text_acc, subgroupstr=c("Below", "Average", "Above"), color_codes, 
#                                  groupstr=c("Below average", "Average", "Above average"))
# flood_acc_prim = grouped_primary(topics, sec_text_floodacc, subgroupstr=c("Below", "Average", "Above"), color_codes, 
#                                  groupstr=c("Below average", "Average", "Above average"))

png(file="paper2/Fig_primarydriveage.png", 
    family="Helvetica", res=300, 
    units="in", width=double_column, height=column_height*3, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

sec_drive_age$prim.df = arrange(sec_drive_age$prim.df, sec_drive_age$prim.df$topic)
sec_drive_age$prim.df$word = factor(sec_drive_age$prim.df$word, levels = unique(sec_drive_age$prim.df$word))
sec_drive_age$prim.df$level = factor(sec_drive_age$prim.df$level, levels = c("18-34", "35-54", "55+"))

ggplot(sec_drive_age$prim.df, aes(x = percent, y = word, fill=topic)) +
  scale_fill_manual(values=color_codes$color, labels = function(x) str_wrap(x, width = 22)) + 
  xlim(0, 16) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(legend.position="top", legend.text=element_text(size=8), legend.box.spacing = unit(0, "pt")) +
  facet_wrap(~level) + geom_text(aes(label = round(percent,1), y=word), hjust=0, size = 3) +
  labs(y = "", x= "Percent (%) of participants", fill="Reason")
dev.off()

png(file="paper2/Fig_primaryfloodage.png", 
    family="Helvetica", res=300, 
    units="in", width=double_column, height=column_height*3, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

sec_flood_age$prim.df = arrange(sec_flood_age$prim.df, sec_flood_age$prim.df$topic)
sec_flood_age$prim.df$word = factor(sec_flood_age$prim.df$word, levels = unique(sec_flood_age$prim.df$word))
sec_flood_age$prim.df$level = factor(sec_flood_age$prim.df$level, levels = c("18-34", "35-54", "55+"))

ggplot(sec_flood_age$prim.df, aes(x = percent, y = word, fill=topic)) +
  scale_fill_manual(values=color_codes$color, labels = function(x) str_wrap(x, width = 22)) + 
  xlim(0, 16) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(legend.position="top", legend.text=element_text(size=8), legend.box.spacing = unit(0, "pt")) +
  facet_wrap(~level) + geom_text(aes(label = percent, y=word), hjust=0, size = 3) +
  labs(y = "", x= "Percent (%) of participants", fill="Reason")
dev.off()

png(file="paper2/Fig_primarydriveclim.png", 
    family="Helvetica", res=300, 
    units="in", width=double_column, height=column_height*3, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

sec_drive_clim$prim.df = arrange(sec_drive_clim$prim.df, sec_drive_clim$prim.df$topic)
sec_drive_clim$prim.df$word = factor(sec_drive_clim$prim.df$word, levels = unique(sec_drive_clim$prim.df$word))
sec_drive_clim$prim.df$level = factor(sec_drive_clim$prim.df$level, levels = c("Low", "Neutral", "High"))
levels(sec_drive_clim$prim.df$level) <- c("Lower literacy", "Neutral", "Higher literacy")

ggplot(sec_drive_clim$prim.df, aes(x = percent, y = word, fill=topic)) +
  scale_fill_manual(values=color_codes$color, labels = function(x) str_wrap(x, width = 22)) + 
  xlim(0, 16) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(legend.position="top", legend.text=element_text(size=8), legend.box.spacing = unit(0, "pt")) +
  facet_wrap(~level) + geom_text(aes(label = round(percent,1), y=word), hjust=0, size = 3) +
  labs(y = "", x= "Percent (%) of participants", fill="Reason")
dev.off()

png(file="paper2/Fig_primaryfloodclim.png", 
    family="Helvetica", res=300, 
    units="in", width=double_column, height=column_height*3, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

sec_flood_clim$prim.df = arrange(sec_flood_clim$prim.df, sec_flood_clim$prim.df$topic)
sec_flood_clim$prim.df$word = factor(sec_flood_clim$prim.df$word, levels = unique(sec_flood_clim$prim.df$word))
sec_flood_clim$prim.df$level = factor(sec_flood_clim$prim.df$level, levels = c("Low", "Neutral", "High"))
levels(sec_flood_clim$prim.df$level) <- c("Lower literacy", "Neutral", "Higher literacy")

ggplot(sec_flood_clim$prim.df, aes(x = percent, y = word, fill=topic)) +
  scale_fill_manual(values=color_codes$color, labels = function(x) str_wrap(x, width = 22)) + 
  xlim(0, 16) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(legend.position="top", legend.text=element_text(size=8), legend.box.spacing = unit(0, "pt")) +
  facet_wrap(~level) + geom_text(aes(label = percent, y=word), hjust=0, size = 3) +
  labs(y = "", x= "Percent (%) of participants", fill="Reason")
dev.off()

png(file="paper2/Fig_primarydrivePol.png", 
    family="Helvetica", res=300, 
    units="in", width=double_column, height=column_height*3, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

sec_drive_pol$prim.df = arrange(sec_drive_pol$prim.df, sec_drive_pol$prim.df$topic)
sec_drive_pol$prim.df$word = factor(sec_drive_pol$prim.df$word, levels = unique(sec_drive_pol$prim.df$word))
sec_drive_pol$prim.df$level = factor(sec_drive_pol$prim.df$level, levels = c("Conservative", "Neutral", "Liberal"))

ggplot(sec_drive_pol$prim.df, aes(x = percent, y = word, fill=topic)) +
  scale_fill_manual(values=color_codes$color, labels = function(x) str_wrap(x, width = 22)) + 
  xlim(0, 14) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(legend.position="top", legend.text=element_text(size=8), legend.box.spacing = unit(0, "pt")) +
  facet_wrap(~level) + geom_text(aes(label = percent, y=word), hjust=0, size = 3) +
  labs(y = "", x= "Percent (%) of participants", fill="Reason")
dev.off()

png(file="paper2/Fig_primaryfloodPol.png", 
    family="Helvetica", res=300, 
    units="in", width=double_column, height=column_height*3, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

sec_flood_pol$prim.df = arrange(sec_flood_pol$prim.df, sec_flood_pol$prim.df$topic)
sec_flood_pol$prim.df$word = factor(sec_flood_pol$prim.df$word, levels = unique(sec_flood_pol$prim.df$word))
sec_flood_pol$prim.df$level = factor(sec_flood_pol$prim.df$level, levels = c("Conservative", "Neutral", "Liberal"))

ggplot(sec_flood_pol$prim.df, aes(x = percent, y = word, fill=topic)) +
  scale_fill_manual(values=color_codes$color, labels = function(x) str_wrap(x, width = 22)) + 
  xlim(0, 14) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(legend.position="top", legend.text=element_text(size=8), legend.box.spacing = unit(0, "pt")) +
  facet_wrap(~level) + geom_text(aes(label = percent, y=word), hjust=0, size = 3) +
  labs(y = "", x= "Percent (%) of participants", fill="Reason")
dev.off()

png(file="paper2/Fig_primarydriveAcc.png", 
    family="Helvetica", res=300, 
    units="in", width=double_column, height=column_height*3, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

sec_drive_acc$prim.df = arrange(sec_drive_acc$prim.df, sec_drive_acc$prim.df$topic)
sec_drive_acc$prim.df$word = factor(sec_drive_acc$prim.df$word, levels = unique(sec_drive_acc$prim.df$word))
sec_drive_acc$prim.df$level = factor(sec_drive_acc$prim.df$level, levels = c("Below", "Average", "Above"))
levels(sec_drive_acc$prim.df$level) <- c("Below average", "Average", "Above average")

ggplot(sec_drive_acc$prim.df, aes(x = percent, y = word, fill=topic)) +
  scale_fill_manual(values=color_codes$color, labels = function(x) str_wrap(x, width = 22)) + 
  xlim(0, 14) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(legend.position="top", legend.text=element_text(size=8), legend.box.spacing = unit(0, "pt")) +
  facet_wrap(~level) + geom_text(aes(label = percent, y=word), 
                                 hjust=0, size = 3) +
  labs(y = "", x= "Percent (%) of participants", 
       fill="Reason")
dev.off()

png(file="paper2/Fig_primaryfloodAcc.png", 
    family="Helvetica", res=300, 
    units="in", width=double_column, height=column_height*3, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

sec_flood_acc$prim.df = arrange(sec_flood_acc$prim.df, sec_flood_acc$prim.df$topic)
sec_flood_acc$prim.df$word = factor(sec_flood_acc$prim.df$word, levels = unique(sec_flood_acc$prim.df$word))
sec_flood_acc$prim.df$level = factor(sec_flood_acc$prim.df$level, levels = c("Below", "Average", "Above"))
levels(sec_flood_acc$prim.df$level) <- c("Below average", "Average", "Above average")

ggplot(sec_flood_acc$prim.df, aes(x = percent, y = word, fill=topic)) +
  scale_fill_manual(values=color_codes$color, labels = function(x) str_wrap(x, width = 22)) + 
  xlim(0, 26) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(legend.position="top", legend.text=element_text(size=8), legend.box.spacing = unit(0, "pt")) +
  facet_wrap(~level) + geom_text(aes(label = percent, y=word), 
                                 hjust=0, size = 3) +
  labs(y = "", x= "Percent (%) of participants", 
       fill="Reason")
dev.off()
