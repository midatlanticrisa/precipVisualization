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

# Read in the survey data
source("scripts/readSurvey.R")

# Source the functions
source("scripts/surveyFunctions.R")

# Read in the answers/ our interpretations to the questions
ans = read.csv("data/answer_sheet.csv")

##########################################################################
# Plot the responses to each accuracy question by graph type
##########################################################################
# How did participants respond to each question and how does it differ 
# across the graph types?

question_id = toupper(c("exp1", "exp2", "exp3", "exp4", "emp1", "emp2", "emp3", 
                        "emp4", "emp5", "emp6", "acc1", "acc2", "acc3", 
                        "acc4", "acc5", "acc6", "acc7"))

plot_ans = function(ques_ID="EMP5", split.by.block, questionslist, ans,
                    color_class = "Set3"){
  overtabAF = split.by.block[["Area_Freq"]][paste0(ques_ID,"")]
  overtabAI = split.by.block[["Area_Int"]][paste0(ques_ID,".1")]
  overtabBF = split.by.block[["Bar_freq"]][paste0(ques_ID,".2")]
  overtabBI = split.by.block[["Bar_int"]][paste0(ques_ID,".3")]
  overtabXF = split.by.block[["Box_freq"]][paste0(ques_ID,".4")]
  overtabXI = split.by.block[["Box_int"]][paste0(ques_ID,".5")]
  
  overtabtab = data.frame(answers = unlist(c(overtabAF, overtabAI, overtabBF, overtabBI, overtabXF, overtabXI)),
                          graph = c(rep("Area", nrow(overtabAF) + nrow(overtabAI)), 
                                    rep("Bar", nrow(overtabBF) + nrow(overtabBI)),
                                    rep("Box", nrow(overtabXF) + nrow(overtabXI))),
                          correct = c(rep(ans$Answer[which(ans$ID == ques_ID & ans$VAR == "AF")], 
                                          nrow(overtabAF) + nrow(overtabAI)),
                                      rep(ans$Answer[which(ans$ID == ques_ID & ans$VAR == "BF")], 
                                          nrow(overtabBF) + nrow(overtabBI)),
                                      rep(ans$Answer[which(ans$ID == ques_ID & ans$VAR == "XF")], 
                                          nrow(overtabXF) + nrow(overtabXI))))
  
  # create bar plot data
  gldat <- overtabtab %>%
    dplyr::group_by(graph, correct, answers) %>%
    dplyr::summarise(Frequency = n()) %>%
    dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1))
  
  unq_options = unique(overtabtab$answers)
  seqCols = brewer.pal(length(unq_options), color_class)
  
  # create grouped bar plot
  datPlot = ggplot(gldat, aes(answers, Percent, 
                              fill = answers)) +
    facet_grid(~graph) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_line() +
    # define colors
    scale_fill_manual(values=seqCols, 
                      labels = function(x) str_wrap(x, width = 100)) +
    # add text and define color
    geom_text(aes(label=round(Percent,0)), vjust=1.6, color="black", 
              # define text position and size
              position = position_dodge(0.9),  size=3.5) +  
    geom_point(aes(x=correct, y=0), pch = 8, color="black", size=3.5, show.legend=FALSE) +
    theme_bw() +
    theme(axis.text.x=element_blank(), legend.position="top", 
          legend.direction = "vertical", axis.title = element_text(size = 9),
          legend.title = element_text(size=9), legend.text = element_text(size=8),
          legend.box.spacing = unit(0, "pt")) +
    labs(x = "", y= "Percent (%) of correct responses", 
         fill=str_wrap(questionslist$Area_Freq[ques_ID], width = 100))
  
  png(file=paste0("Figures/Fig_", ques_ID, ".png"), 
      family="Helvetica", res=300, 
      units="in", width=6, height=4, pointsize=10)
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))
  
  print(datPlot)
  
  dev.off()
}

# Modeled historical instead of hindcast
for(i in question_id){
  plot_ans(ques_ID=i, split.by.block, questionslist, ans)
}

##########################################################################
# Plot all responses, total responses for each question
##########################################################################

plot_ans_total = function(ques_ID="ACC4", split.by.block, questionslist, ans,
                    color_class = "Set3"){
  overtabAF = split.by.block[["Area_Freq"]][paste0(ques_ID,"")]
  overtabAI = split.by.block[["Area_Int"]][paste0(ques_ID,".1")]
  overtabBF = split.by.block[["Bar_freq"]][paste0(ques_ID,".2")]
  overtabBI = split.by.block[["Bar_int"]][paste0(ques_ID,".3")]
  overtabXF = split.by.block[["Box_freq"]][paste0(ques_ID,".4")]
  overtabXI = split.by.block[["Box_int"]][paste0(ques_ID,".5")]
  
  overtabtab = data.frame(answers = unlist(c(overtabAF, overtabAI, overtabBF, overtabBI, overtabXF, overtabXI)),
                          graph = c(rep("Area", nrow(overtabAF) + nrow(overtabAI)), 
                                    rep("Bar", nrow(overtabBF) + nrow(overtabBI)),
                                    rep("Box", nrow(overtabXF) + nrow(overtabXI))),
                          correct = c(rep(ans$Answer[which(ans$ID == ques_ID & ans$VAR == "AF")], 
                                          nrow(overtabAF) + nrow(overtabAI)),
                                      rep(ans$Answer[which(ans$ID == ques_ID & ans$VAR == "BF")], 
                                          nrow(overtabBF) + nrow(overtabBI)),
                                      rep(ans$Answer[which(ans$ID == ques_ID & ans$VAR == "XF")], 
                                          nrow(overtabXF) + nrow(overtabXI))))
  
  # create bar plot data
  gldat <- overtabtab %>%
    dplyr::group_by(correct, answers) %>%
    dplyr::summarise(Frequency = n()) %>%
    dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1))
  
  unq_options = unique(overtabtab$answers)
  seqCols = brewer.pal(length(unq_options), color_class)
  
  # create grouped bar plot
  datPlot = ggplot(gldat, aes(answers, Percent, 
                              fill = answers)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_line() +
    # define colors
    scale_fill_manual(values=seqCols, labels = function(x) str_wrap(x, width = 80)) +
    # add text and define color
    geom_text(aes(label=round(Percent,0)), vjust=1.6, color="black", 
              position = position_dodge(0.9),  size=3.5) +  
    geom_point(aes(x=correct, y=0), pch = 8, color="black", size=3.5, show.legend=FALSE) +
    theme_bw() +
    theme(axis.text.x=element_blank(), legend.position="top", legend.direction = "vertical", 
          axis.title = element_text(size = 9),
          legend.title = element_text(size=9), legend.text = element_text(size=8),
          legend.box.spacing = unit(0, "pt")) +
    labs(x = "", y= "Percent (%) of correct responses", 
         fill=str_wrap(questionslist$Area_Freq[ques_ID], width = 80))
  
  png(file=paste0("Figures/Fig_TOTAL_", ques_ID, ".png"), 
      family="Helvetica", res=300, 
      units="in", width=5, height=4, pointsize=10)
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))
  
  print(datPlot)
  
  dev.off()
}

# Modeled historical instead of hindcast
plot_ans_total(ques_ID="ACC4", split.by.block, questionslist, ans)
plot_ans_total(ques_ID="EMP3", split.by.block, questionslist, ans)
plot_ans_total(ques_ID="EMP6", split.by.block, questionslist, ans)

print("List color mixing responses by percentage: ")
colormixing = table(unlist(c(split.by.block[["Area_Freq"]]["EMP5"], split.by.block[["Area_Int"]]["EMP5.1"])))
colormixing/sum(colormixing)*100
