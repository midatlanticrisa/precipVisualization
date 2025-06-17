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
library(tidyverse)
library(wordcloud)
library(ggpubr)
library(magick)

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
accuracyTab = rbind(oaAF, oaAI, oaBF, oaBI, oaXF, oaXI)

# Create table of percent of correct responses for each accuracy question by graph type
overaccTab = data.frame(percent = c(round((colSums(rbind(oaAF, oaAI))[-18]/nrow(rbind(oaAF, oaAI)))*100), 
                                    round((colSums(rbind(oaBF, oaBI))[-18]/nrow(rbind(oaBF, oaBI)))*100), 
                                    round((colSums(rbind(oaXF, oaXI))[-18]/nrow(rbind(oaXF, oaXI)))*100)))
overaccTab$ques = rep(names(oaAF)[-18], 3)
overaccTab$graph = c(rep("Area", length(names(oaAF)[-18])), 
                     rep("Bar", length(names(oaAF)[-18])),
                     rep("Box", length(names(oaAF)[-18])))
overaccTab$graph = factor(overaccTab$graph, levels = c("Bar", "Box", "Area"))

freqQues = c("exp1", "exp2", "emp1", "emp2", "emp3", "acc1", "acc2", "acc3")
intQues = c("exp3", "exp4", "emp4", "emp5", "emp6", "acc4", "acc5", "acc6", "acc7")
freq_int = unlist(lapply(X = freqQues, function(X){which(overaccTab$ques == X)}))
int_int = unlist(lapply(X = intQues, function(X){which(overaccTab$ques == X)}))
overaccTab$var = NA
overaccTab$var[freq_int] = "Frequency"
overaccTab$var[int_int] = "Intensity"

overaccTab$acctype = NA
overaccTab$acctype[grep("acc", overaccTab$ques)] = "Accuracy"
overaccTab$acctype[grep("emp", overaccTab$ques)] = "Emphasis"
overaccTab$acctype[grep("exp", overaccTab$ques)] = "Expressiveness"
overaccTab$acctype = factor(overaccTab$acctype, levels = c("Expressiveness", "Emphasis", "Accuracy"))

# https://forum.posit.co/t/how-to-automatically-add-text-annotations-or-tags-outside-of-faceted-plots/13700/10
tag_facet_flex <- function(p, position = 'both', 
                           open = c("(", ""), close = c(")", "."),
                           tag_fun_top = function(i) letters[i],
                           tag_fun_right = function(i) letters[i],
                           x = c(0, 0), y = c(0.5, 1),
                           hjust = c(0, 0), vjust = c(0.5, 1),
                           fontface = c(2, 2), ...) {
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout  
  
  if (grepl(position, 'top')) {
    
    lay <- gb$layout$layout
    
    tags_top <- paste0(open[1], tag_fun_top(unique(lay$COL)), close[1])
    
    tl <- lapply(tags_top, grid::textGrob,
                 x = x[1], y = y[1],
                 hjust = hjust[1], vjust = vjust[1], 
                 gp = grid::gpar(fontface = fontface[1], ...)
    )
    
    g <- ggplot_gtable(gb)
    g <- gtable::gtable_add_rows(g, grid::unit(1, "line"), pos = 0)
    lm <- unique(g$layout[grepl("panel", g$layout$name), "l"])
    g <- gtable::gtable_add_grob(g, grobs = tl, t = 1, l = lm)
    
  } else if (grepl(position, 'right')) {
    
    tags_right <- paste0(open[2], tag_fun_right(unique(lay$ROW)), close[2])
    
    rl <- lapply(tags_right, grid::textGrob,
                 x = x[1], y = y[2],
                 hjust = hjust[1], vjust = vjust[2], 
                 gp = grid::gpar(fontface = fontface[2], ...)
    )
    
    g <- ggplot_gtable(gb)
    l <- unique(g$layout[grepl("panel", g$layout$name), "l"])
    
    wm <- do.call(grid::unit.pmax, lapply(rl, grid::grobWidth))
    g <- gtable::gtable_add_cols(g, wm, pos = 0)
    t <- unique(g$layout[grepl("panel", g$layout$name), "t"])
    g <- gtable::gtable_add_grob(g, grobs = rl, t = t, l = min(l))
    g <- gtable::gtable_add_cols(g, unit(2, "mm"), pos = 0)
    
  } else {
    
    print('Use default tagging option: both top and right sides')
    
    tags_top <- paste0(open[1], tag_fun_top(unique(lay$COL)), close[1])
    tags_right <- paste0(open[2], tag_fun_right(unique(lay$ROW)), close[2])
    
    tl <- lapply(tags_top, grid::textGrob,
                 x = x[1], y = y[1],
                 hjust = hjust[1], vjust = vjust[1], 
                 gp = grid::gpar(fontface = fontface[1], ...)
    )
    
    rl <- lapply(tags_right, grid::textGrob,
                 x = x[2], y = y[2],
                 hjust = hjust[2], vjust = vjust[2], 
                 gp = grid::gpar(fontface = fontface[2], ...)
    )
    
    g <- ggplot_gtable(gb)
    g <- gtable::gtable_add_rows(g, grid::unit(1, "line"), pos = 0)
    l <- unique(g$layout[grepl("panel", g$layout$name), "l"])
    g <- gtable::gtable_add_grob(g, grobs = tl, t = 1, l = l)
    
    wm <- do.call(grid::unit.pmax, lapply(rl, grid::grobWidth))
    g <- gtable::gtable_add_cols(g, wm, pos = max(l))
    t <- unique(g$layout[grepl("panel", g$layout$name), "t"])
    g <- gtable::gtable_add_grob(g, grobs = rl, t = t, l = max(l) + 1)
    g <- gtable::gtable_add_cols(g, unit(2, "mm"), pos = max(l))
  }
  
  if (!is.null(g)) {
    grid::grid.newpage()
    grid::grid.draw(g)    
  }
  
  return(g)
  
}

### Figure #3
exp_questions = data.frame(ID = c(paste0("EXP", 1:4), paste0("EMP", 1:6), paste0("ACC", 1:7)),
                           Question = c("What information does the graph show? (frequency graph)", 
                                        "The primary purpose of this graph is to __________ . (frequency graph)", 
                                        "What information does the graph show? (intensity graph)",
                                        "The primary purpose of this graph is to __________ . (intensity graph)",
                                        questions$EMP1, questions$EMP2, 
                                        questions$EMP3, questions$EMP4,
                                        questions$EMP5, questions$EMP6,
                                        questions$ACC1, questions$ACC2,
                                        questions$ACC3, questions$ACC4,
                                        questions$ACC5, questions$ACC6,
                                        questions$ACC7))

exp_gtable = gt(exp_questions, rowname_col = "ID")
exp_gtable %>% cols_width("Question" ~ px(500)) %>%
  gtsave("exp_questions.png", path = "paper1", expand = 10)
exp_drawtable <- ggdraw() + draw_image("paper1/exp_questions.png")#, scale = 0.95)

facetover = ggplot(overaccTab, aes(fill=graph,y=percent, x=ques)) + 
  geom_bar(position="dodge", stat="identity", colour="black", linewidth = 0.1) +
  labs(x = "Question ID", y= "Percent (%) of participants with correct response", 
       fill="") + 
  # define colors
  scale_fill_manual(values=graphcol) +
  theme_bw() + geom_text(aes(label = round(percent)), vjust=-0.1, position = position_dodge(0.9), size = 2) +
  theme(legend.title = element_text(size=8), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "none", #"inside"
        legend.position.inside = c(0.16, 0.96), 
        legend.background = element_rect(fill = NA, colour = NA)) +
  facet_wrap(~acctype, ncol=1, scales = "free_x")

outside_legend <- function(){
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(0,0,0,0))
  plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("top", legend =c("Bar", "Box", "Area"), pch=c(15, 15, 15), bty='n',
         col = graphcol, pt.cex=2)}

leg_tab = plot_grid(outside_legend, exp_drawtable, nrow=2, rel_heights = c(0.25, 2))

png(file="paper1/Fig3_DiagwithIDswidth.png", family="Helvetica", res=300,
    units="in", width=double_column, height=column_height*2.35, pointsize=9)
plot_grid(tag_facet_flex(facetover, position = 'right'), exp_drawtable, nrow=1)
dev.off()

# cairo_ps(filename = "paper1/figure03.eps", family="Helvetica", fallback_resolution = 300,
#          width=double_column, height=column_height*2.35, pointsize=9)
# postscript(file="paper1/figure03.eps", horizontal = FALSE, onefile = FALSE, paper = "special", family="Helvetica",
#            width=double_column, height=column_height*2.35, pointsize=9)
pdf(file="paper1/figure03.pdf", family="Helvetica", width=double_column, height=column_height*2.35, pointsize=9)
plot_grid(tag_facet_flex(facetover, position = 'right'), leg_tab, nrow=1)
dev.off()
### Figure #3 stop

# -------------------------------------------------------------------------
# Interpretation
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

# Check for normality using Shapiro-Wilk normality test
# This is not necessary given the sample sizes are large (usually n ≥30).
res_aov <- aov(val ~ name, data = overAcc)

# Check for normality and outliers, visually
qqPlot(res_aov$residuals, id = FALSE)
boxplot(val ~ name, data = overAcc)
hist(res_aov$residuals)

# Normality is proven when alpha is greater than 5%
shapiro.test(res_aov$residuals)
leveneTest(val ~ name, data = overAcc)

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(val ~ name, data = overAcc,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x),
                              quantile(x, 0.95), quantile(x, 0.05)), 1))

# 1st method: Assuming equal variances
oneway.test(val ~ name, data = overAcc, var.equal = TRUE)

# 2nd method. The p-value in both should be the same meaning the results and
# conclusions will be unchanged.
summary(res_aov)

# If the p-value is smaller than 0.05 than at least one of the groups is statistically
# different
report(res_aov)

# Preform Post-hoc test (Tukey HSD test) is ANOVA shows differences
# If any groups are smaller than 0.05, then they are statistically different. If 
# larger then the two groups are considered equal.
post_test <- glht(res_aov, linfct = mcp(name = "Tukey"))
summary(post_test)

# - -----------------------------------------------------------------------
# Expressiveness
# A lack of expressiveness occurs when the design does not clearly indicate the 
# intent of the visualization. For example, if identifying a specific pattern or 
# trend is the intent of the image, then the most effective chart types and visual 
# variables used should direct attention to that specific pattern or trend and not 
# another less important one (Gerst et al., 2020).
# Chart appropriateness for the task
# - -----------------------------------------------------------------------
# Is there a difference in expressiveness across 3 graph types?
#   Which group is different?
#   Kruskal-Wallis test with a post-hoc test: Dunn test, Dwass-Steel-Crichtlow-Fligner 
#   test, Conover-Iman test (2 tests - PA)
areaExpress = c(rowSums(oaAF[ ,c("exp1", "exp3", "exp2", "exp4")]), rowSums(oaAI[ ,c("exp1", "exp3", "exp2", "exp4")]))
barExpress = c(rowSums(oaBF[ ,c("exp1", "exp3", "exp2", "exp4")]), rowSums(oaBI[ ,c("exp1", "exp3", "exp2", "exp4")]))
boxExpress = c(rowSums(oaXF[ ,c("exp1", "exp3", "exp2", "exp4")]), rowSums(oaXI[ ,c("exp1", "exp3", "exp2", "exp4")]))

# Create a dataframe for One-way ANOVA
datExpress = data.frame(val = c(areaExpress,barExpress,boxExpress), 
                        name = c(rep("Area", length(area)), rep("Bar", length(bar)), 
                                        rep("Box", length(box))))
datExpress$name = factor(datExpress$name, levels = c("Bar", "Box", "Area"))

# Check for normality using Shapiro-Wilk normality test
res_aovExp <- aov(val ~ name, data = datExpress)
qqPlot(res_aovExp$residuals, id = FALSE)
boxplot(val ~ name, data = datExpress)
hist(res_aovExp$residuals)

# Normality is proven when alpha is greater than 5%
shapiro.test(res_aovExp$residuals)
leveneTest(val ~ name, data = datExpress)

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(val ~ name, data = datExpress,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x)), 1))

# One-way ANOVA
# 1st method:
oneway.test(val ~ name, data = datExpress, var.equal = TRUE)

# 2nd method:
summary(res_aovExp)
report(res_aovExp)

# Kruskal-Wallis test and post-hoc test
# kruskal.test(val ~ name, data = datExpress)
# dunnTest(val ~ name, data = datExpress, method = "holm")

# - -----------------------------------------------------------------------
# Emphasis
# Communication gap
# Lack of emphasis occurs when auxiliary elements, such as legends, grids, or 
# annotations, do not highlight essential areas of the image (Gerst et al., 2020).
# - -----------------------------------------------------------------------
# Is there a difference in emphasis across 3 graph types?
#   Which group is different?
#   Kruskal-Wallis test with a post-hoc test: Dunn test, Dwass-Steel-Crichtlow-Fligner 
#   test, Conover-Iman test (2 tests - PA)

areaEmphasis = c(rowSums(oaAF[ ,c("emp1", "emp2", "emp3", "emp4", "emp5", "emp6")]), 
                 rowSums(oaAI[ ,c("emp1", "emp2", "emp3", "emp4", "emp5", "emp6")]))
barEmphasis = c(rowSums(oaBF[ ,c("emp1", "emp2", "emp3", "emp4", "emp5", "emp6")]), 
                rowSums(oaBI[ ,c("emp1", "emp2", "emp3", "emp4", "emp5", "emp6")]))
boxEmphasis = c(rowSums(oaXF[ ,c("emp1", "emp2", "emp3", "emp4", "emp5", "emp6")]), 
                rowSums(oaXI[ ,c("emp1", "emp2", "emp3", "emp4", "emp5", "emp6")]))

datEmphasis = data.frame(val = c(areaEmphasis,barEmphasis,boxEmphasis), 
                         name = c(rep("Area", length(area)), rep("Bar", length(bar)), 
                                         rep("Box", length(box))))
datEmphasis$name = factor(datEmphasis$name, levels = c("Bar", "Box", "Area"))

# Check for normality using Shapiro-Wilk normality test
res_aovEmp <- aov(val ~ name, data = datEmphasis)
qqPlot(res_aovEmp$residuals, id = FALSE)
boxplot(val ~ name, data = datEmphasis)
hist(res_aovEmp$residuals)

# Normality is proven when alpha is greater than 5%
shapiro.test(res_aovEmp$residuals)
leveneTest(val ~ name, data = datEmphasis)

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(val ~ name, data = datEmphasis,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x)), 1))

# One-way ANOVA
# 1st method:
oneway.test(val ~ name, data = datEmphasis, var.equal = TRUE)

# 2nd method:
summary(res_aovEmp)
report(res_aovEmp)

# Preform Post-hoc test (Tukey HSD test) is ANOVA shows differences
post_testEmp <- glht(res_aovEmp, linfct = mcp(name = "Tukey"))
summary(post_testEmp)

# - -----------------------------------------------------------------------
# Accuracy
# Inaccurate inference:
# While a user might correctly interpret one quantity as greater than another, 
# design problems, such as using the wrong chart type, can estimate that 
# difference as too large or too small.
# - -----------------------------------------------------------------------

# Precent of people who confused ann. precip with precip intensity
annprecip = c(oaAF[, "acc4"], oaAI[, "acc4"], oaBF[, "acc4"], oaBI[, "acc4"], oaXF[, "acc4"], oaXI[, "acc4"])
round(length(which(annprecip!=1))/length(annprecip)*100)

# Is there a difference in accuracy across 3 graph types?
#   Which group is different?
#   Kruskal-Wallis test with a post-hoc test: Dunn test, Dwass-Steel-Crichtlow-Fligner 
#   test, Conover-Iman test (2 tests - PA)
areaAcc = c(rowSums(oaAF[ ,c("acc1", "acc2", "acc3", "acc4", "acc5", "acc6", "acc7")]), 
            rowSums(oaAI[ ,c("acc1", "acc2", "acc3", "acc4", "acc5", "acc6", "acc7")]))
barAcc = c(rowSums(oaBF[ ,c("acc1", "acc2", "acc3", "acc4", "acc5", "acc6", "acc7")]), 
           rowSums(oaBI[ ,c("acc1", "acc2", "acc3", "acc4", "acc5", "acc6", "acc7")]))
boxAcc = c(rowSums(oaXF[ ,c("acc1", "acc2", "acc3", "acc4", "acc5", "acc6", "acc7")]), 
           rowSums(oaXI[ ,c("acc1", "acc2", "acc3", "acc4", "acc5", "acc6", "acc7")]))
# boxAcc = boxAcc[-which(boxAcc == 7)]

datAcc = data.frame(val = c(areaAcc,barAcc,boxAcc), 
                    name = c(rep("Area", length(area)), rep("Bar", length(bar)), 
                                    rep("Box", length(box))))
datAcc$name = factor(datAcc$name, levels = c("Bar", "Box", "Area"))

res_aovAcc <- aov(val ~ name, data = datAcc)
qqPlot(res_aovAcc$residuals, id = FALSE)
boxplot(val ~ name, data = datAcc)
vioplot(val ~ name, data = datAcc)
hist(res_aovAcc$residuals)

# Normality is proven when alpha is greater than 5%
shapiro.test(res_aovAcc$residuals)
leveneTest(val ~ name, data = datAcc)

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(val ~ name, data = datAcc,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x)), 1))

# One-way ANOVA
# # 1st method:
# oneway.test(val ~ name, data = datAcc, var.equal = TRUE)
# 
# # 2nd method:
# summary(res_aovAcc)
# report(res_aovAcc)
# 
# # Preform Post-hoc test (Tukey HSD test) is ANOVA shows differences
# post_testAcc <- glht(res_aovAcc, linfct = mcp(name = "Tukey"))
# summary(post_testAcc)

# Kruskal-Wallis test and post-hoc test
kruskal.test(val ~ name, data = datAcc)
dunnTest(val ~ name, data = datAcc, method = "holm")

# Mood's Median test and post-hoc test (Pairwise Median Test)
# https://rcompanion.org/handbook/F_09.html
median_test(val ~ name, data = datAcc)
pairwiseMedianTest(val ~ name, data = datAcc, exact  = NULL, method = "fdr")

# - -----------------------------------------------------------------------
# Efficiency
# - -----------------------------------------------------------------------
oatimeAF = calcAccuracyTime(name="Area_Freq", id="")
oatimeAI = calcAccuracyTime(name="Area_Int", id=".1")
oatimeBF = calcAccuracyTime(name="Bar_freq", id=".2")
oatimeBI = calcAccuracyTime(name="Bar_int", id=".3")
oatimeXF = calcAccuracyTime(name="Box_freq", id=".4")
oatimeXI = calcAccuracyTime(name="Box_int", id=".5")

# Seconds to minutes
areatime = c(oatimeAF$tot, oatimeAI$tot)/60
bartime = c(oatimeBF$tot, oatimeBI$tot)/60
boxtime = c(oatimeXF$tot, oatimeXI$tot)/60

datTime = data.frame(val = c(areatime,bartime,boxtime), 
                     name = c(rep("Area", length(area)), rep("Bar", length(bar)), 
                                     rep("Box", length(box))))
datTime$name = factor(datTime$name, levels = c("Bar", "Box", "Area"))

# Check for normality using Shapiro-Wilk normality test
res_aovTime <- aov(val ~ name, data = datTime)
qqPlot(res_aovTime$residuals, id = FALSE)
boxplot(val ~ name, data = datTime)
hist(res_aovTime$residuals)

# Normality is proven when alpha is greater than 5%
shapiro.test(res_aovTime$residuals)
leveneTest(val ~ name, data = datTime)

# # 2nd method:
# summary(res_aovTime)
# report(res_aovTime)

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(val ~ name, data = datTime,
          function(x) round(c(mean = mean(x), med = median(x),  sd = sd(x)), 1))

# Kruskal-Wallis test and post-hoc test
kruskal.test(val ~ name, data = datTime)
dunnTest(val ~ name, data = datTime, method = "holm")

# Mood's Median test and post-hoc test (Pairwise Median Test)
median_test(val ~ name, data = datTime)
pairwiseMedianTest(val ~ name, data = datTime, exact = NULL, method = "fdr")

##########################################################################
## Figure 2
# png(file="paper1/Fig2_PerfDiagVio.png", family="Helvetica", res=300, 
#     units="in", width=double_column, height=column_height*3, pointsize=14)
pdf(file="paper1/figure02.pdf", family="Helvetica", width=double_column, height=column_height*2.5, pointsize=14)

par(mfrow=c(3,2), mgp=c(1.5,.5,0), mar=c(2,4,1,1))
vioplot(val ~ name, data = overAcc, xlab="", ylab="Number of correct responses", 
        col=graphcol, plotCentre="line")
points(1, mean(overAcc$val[overAcc$name == "Bar"]), bg="white", pch=21)
points(2, mean(overAcc$val[overAcc$name == "Box"]), bg="white", pch=21)
points(3, mean(overAcc$val[overAcc$name == "Area"]), bg="white", pch=21)

put.fig.letter("a.",font=2, x=0.025, y=0.98)
put.fig.letter("Overall interpretation",font=2, x=0.5525, y=0.96)

# Legend
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c("Median", "Mean", "Bar", "Box", "Area"), pch=c(NA, 21, 15, 15, 15), 
       pt.bg = c(NA,"white", NA, NA, NA), lty=c(1, NA, NA, NA, NA), bty='n',
       col = c('black', 'black', graphcol), pt.cex=c(NA, 1, 2,2,2))

vioplot(val ~ name, data = datExpress, xlab="", ylab="Number of correct responses", 
        col=graphcol, plotCentre="line")
points(1, mean(datExpress$val[datExpress$name == "Bar"]), bg="white", pch=21)
points(2, mean(datExpress$val[datExpress$name == "Box"]), bg="white", pch=21)
points(3, mean(datExpress$val[datExpress$name == "Area"]), bg="white", pch=21)

put.fig.letter("b.",font=2, x=0.025, y=0.98)
put.fig.letter("Expressiveness",font=2, x=0.5525, y=0.96)

vioplot(val ~ name, data = datEmphasis, xlab="", ylab="Number of correct responses", 
        col=graphcol, plotCentre="line")
points(1, mean(datEmphasis$val[datEmphasis$name == "Bar"]), bg="white", pch=21)
points(2, mean(datEmphasis$val[datEmphasis$name == "Box"]), bg="white", pch=21)
points(3, mean(datEmphasis$val[datEmphasis$name == "Area"]), bg="white", pch=21)

put.fig.letter("c.",font=2, x=0.025, y=0.98)
put.fig.letter("Emphasis",font=2, x=0.5525, y=0.96)

vioplot(val ~ name, data = datAcc, xlab="", ylab="Number of correct responses", 
        col=graphcol, plotCentre="line")
points(1, mean(datAcc$val[datAcc$name == "Bar"]), bg="white", pch=21)
points(2, mean(datAcc$val[datAcc$name == "Box"]), bg="white", pch=21)
points(3, mean(datAcc$val[datAcc$name == "Area"]), bg="white", pch=21)

put.fig.letter("d.",font=2, x=0.025, y=0.98)
put.fig.letter("Accuracy",font=2, x=0.5525, y=0.96)

vioplot(val ~ name, data = datTime, xlab="", ylab="Time (minutes)", 
        col=graphcol, plotCentre="line")
points(1, mean(datTime$val[datTime$name == "Bar"]), bg="white", pch=21)
points(2, mean(datTime$val[datTime$name == "Box"]), bg="white", pch=21)
points(3, mean(datTime$val[datTime$name == "Area"]), bg="white", pch=21)

put.fig.letter("e.",font=2, x=0.025, y=0.98)
put.fig.letter("Efficiency",font=2, x=0.5525, y=0.96)

dev.off()

##########################################################################
# User Rating
##########################################################################
overallUse = data.frame(val = c(split.by.block$Area_Freq$USE12, 
                                split.by.block$Area_Int$USE12.1,
                                split.by.block$Bar_freq$USE12.2,
                                split.by.block$Bar_int$USE12.3,
                                split.by.block$Box_freq$USE12.4,
                                split.by.block$Box_int$USE12.5), 
                        name = c(rep("Area", length(split.by.block$Area_Freq$USE12) + 
                                              length(split.by.block$Area_Int$USE12.1)), 
                                        rep("Bar", length(split.by.block$Bar_freq$USE12.2) + 
                                              length(split.by.block$Bar_int$USE12.3)), 
                                        rep("Box", length(split.by.block$Box_freq$USE12.4) + 
                                              length(split.by.block$Box_int$USE12.5))))
overallUse$name = factor(overallUse$name, levels = c("Bar", "Box", "Area"))
overallUse$val = as.numeric(overallUse$val)

## supplementary figure
png(file="paper1/SFig2_usability.png", family="Helvetica", res=300, 
    units="in", width=single_column, height=column_height, pointsize=8)
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(1.5,2.5,1.5,0.75))
  vioplot(val ~ name, data = overallUse, xlab="", ylab=questions$USE12, 
          col = graphcol, pch=20, lwd=1.5, plotCentre="line", las=1)
  points(1, mean(overallUse$val[overallUse$name == "Bar"]), bg="white", pch=21)
  points(2, mean(overallUse$val[overallUse$name == "Box"]), bg="white", pch=21)
  points(3, mean(overallUse$val[overallUse$name == "Area"]), bg="white", pch=21)
  add_legend("topright", legend =c("Median", "Mean"), pch=c(NA, 21), 
             pt.bg = c(NA,"white"), lty=c(1, NA), bty='n',
             col = c('black', 'black'), pt.cex=c(NA, 1), horiz=TRUE)
dev.off()
## supplementary figure

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(val ~ name, data = overallUse,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 1))

# Check for normality using Shapiro-Wilk normality test
res_aovuse <- aov(val ~ name, data = overallUse)
qqPlot(res_aovuse$residuals, id = FALSE)
boxplot(val ~ name, data = overallUse)
hist(res_aovuse$residuals)

# Normality is proven when alpha is greater than 5%
shapiro.test(res_aovuse$residuals)
leveneTest(val ~ name, data = overallUse)

# # One-way ANOVA
# # 1st method:
# oneway.test(val ~ name, data = overallUse, var.equal = TRUE)
# 
# # 2nd method:
# summary(res_aovuse)
# report(res_aovuse)
# 
# # Post-hoc test (Tukey HSD test)
# post_test <- glht(res_aovuse, linfct = mcp(name = "Tukey"))
# summary(post_test)
# 
# cor(ovaccuracy, overallUse$val)

# Kruskal-Wallis test and post-hoc test
kruskal.test(val ~ name, data = overallUse)
dunnTest(val ~ name, data = overallUse, method = "holm")

# Mood's Median test and post-hoc test (Pairwise Median Test)
median_test(val ~ name, data = overallUse)
pairwiseMedianTest(val ~ name, data = overallUse, exact = NULL, method = "fdr")

##########################################################################
# User Rating
##########################################################################

susAF = calcSusScores(name="Area_Freq", id="")
susAI = calcSusScores(name="Area_Int", id=".1")
susBF = calcSusScores(name="Bar_freq", id=".2")
susBI = calcSusScores(name="Bar_int", id=".3")
susXF = calcSusScores(name="Box_freq", id=".4")
susXI = calcSusScores(name="Box_int", id=".5")

sus_score = rbind(susAF, susAI, susBF, susBI, susXF, susXI)
fullsus = c(susAF$TOT, susAI$TOT, susBF$TOT, susBI$TOT, susXF$TOT, susXI$TOT)
# mean(fullsus)
# summary(fullsus)

# - -----------------------------------------------------------------------
# Is there a graph thats more useful?
areasus = c(susAF$TOT, susAI$TOT)
barsus = c(susBF$TOT, susBI$TOT)
boxsus = c(susXF$TOT, susXI$TOT)

susdat = data.frame(sus = c(areasus,barsus,boxsus),
                    name = c(rep("Area", length(areasus)), rep("Bar", length(barsus)), 
                                    rep("Box", length(boxsus))))
susdat$name = factor(susdat$name, levels = c("Bar", "Box", "Area"))

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(sus ~ name, data = susdat,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 1))

# Check for normality using Shapiro-Wilk normality test
res_aovsus <- aov(sus ~ name, data = susdat)
qqPlot(res_aovsus$residuals, id = FALSE)
boxplot(sus ~ name, data = susdat)
hist(res_aovsus$residuals)

# Normality is proven when alpha is greater than 5%
shapiro.test(res_aovsus$residuals)
leveneTest(sus ~ name, data = susdat)

# One-way ANOVA
# 1st method:
oneway.test(sus ~ name, data = susdat, var.equal = TRUE)

# 2nd method:
summary(res_aovsus)
report(res_aovsus)

# Preform Post-hoc test (Tukey HSD test) is ANOVA shows differences
post_testsus <- glht(res_aovsus, linfct = mcp(name = "Tukey"))
summary(post_testsus)

# -------------------------------------------------------------------------
# define a function that emits the desired plot
## Figure 4
susDen <- function() {
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,1,0,0))

  # SUS scores by graph type
  boxplot(sus ~ name, data = susdat, xlab="SUS score", ylab="", horizontal=TRUE, 
          axes=FALSE, col = graphcol, pch=20, lwd=1.5, plotCentre="line")
  points(mean(susdat$sus[susdat$name == "Bar"]), 1, bg="white", pch=21)
  points(mean(susdat$sus[susdat$name == "Box"]), 2,  bg="white", pch=21)
  points(mean(susdat$sus[susdat$name == "Area"]), 3, bg="white", pch=21)
  
  axis(1, at = seq(from = 0, to = 100, by = 10), cex.axis = 1, lwd.tick = 2,
       col = "gray44")
}
# - -----------------------------------------------------------------------
# https://handbook.gitlab.com/handbook/product/ux/performance-indicators/system-usability-scale/
sus_score$graph = c(rep("Area", length(areasus)), 
                                  rep("Bar", length(barsus)), 
                                  rep("Box", length(boxsus)))

indScorearea = colMeans(sus_score[which(sus_score$graph == "Area"),1:10], na.rm = TRUE)*25
indScorebar = colMeans(sus_score[which(sus_score$graph == "Bar"),1:10], na.rm = TRUE)*25
indScorebox = colMeans(sus_score[which(sus_score$graph == "Box"),1:10], na.rm = TRUE)*25

indscoredf = data.frame(val = c(indScorearea, indScorebar, indScorebox), 
                        ques = rep(names(indScorearea), 3), 
                        graph = c(rep("Area", 10), rep("Bar", 10), 
                                  rep("Box", 10)))
indscoredf$graph = factor(indscoredf$graph, levels = c("Bar", "Box", "Area"))
indscoredf$ques = factor(indscoredf$ques, levels = c("USE2", "USE3", "USE4", "USE5",
                                                     "USE6", "USE7", "USE8", "USE9",
                                                     "USE10", "USE11"))

# # Grouped plot by graph type split into graphs of accuracy type
susQues = ggplot(indscoredf, aes(fill=graph,y=val, x=ques)) + 
  geom_bar(position="dodge", stat="identity", colour="black", linewidth = 0.1, show.legend = FALSE) +
  labs(x = "Question ID", y= "SUS score per question", fill="Graph type:") +
  scale_fill_manual(values=graphcol) + #ylim(0, 10) +
  theme_bw() + geom_text(aes(label = round(val)), vjust=-0.1, position = position_dodge(0.9), size = 2.2) +
  theme(legend.title = element_text(size=9), 
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

# png(file="paper1/Fig4_SUS.png", family="Helvetica", res=300,
#     units="in", width=6, height=8, pointsize=10)
# plot_grid(susDen, susQues, nrow=2, labels = "auto")
# dev.off()

# Table
sus_questions = data.frame(ID = paste0("USE", 2:11),
                           Question = c(questions$USE2, questions$USE3, questions$USE4,
                                        questions$USE5, questions$USE6, questions$USE7,
                                        questions$USE8, questions$USE9, questions$USE10,
                                        questions$USE11))

sus_gt = gt(sus_questions, rowname_col = "ID")

sus_gt %>%
  gtsave("sus_gt_table.png", path = "paper1")
sus_gt_table <- ggdraw() + draw_image("paper1/sus_gt_table.png")#, scale = 0.95)

a_comolegend <- function(){
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(0,0,0,0))
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("top", legend =c("Median", "Mean", "Bar", "Box", "Area"), pch=c(NA, 21, 15, 15, 15), 
         pt.bg = c(NA,"white", NA, NA, NA), lty=c(1, NA, NA, NA, NA), bty='n',
         col = c('black', 'black', graphcol), pt.cex=c(NA, 1,2,2,2))}

sus_plot = plot_grid(susDen, a_comolegend, susQues, nrow=2, 
                   labels = c("a.", "", "b."), rel_widths=c(1,0.25))

png(file="paper1/Fig4_SUS.png", family="Helvetica", res=300,
    units="in", width=med_column, height=column_height*3, pointsize=10)
plot_grid(sus_plot, sus_gt_table, ncol=1, rel_heights = c(2.1,0.9))
dev.off()

pdf(file="paper1/figure04.pdf", family="Helvetica", width=med23_column, height=column_height*3, pointsize=10)
# png(file="paper1/Fig4_SUS.png", family="Helvetica", res=300,
#     units="in", width=med23_column, height=column_height*3, pointsize=10)
plot_grid(sus_plot, sus_gt_table, ncol=1, rel_heights = c(2.1,0.9))
dev.off()
## Figure 4 end

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

# Is there a correlation between interpretation and protection value
cor.test(overAcc$val, protab$CHAL1)
cor.test(overAcc$val, protab$CHAL2)

# Risk Perception 
likeops = data.frame(ans = c("exceptionally unlikely", 
                             "very unlikely",
                             "unlikely",
                             "about as likely as not",
                             "likely",
                             "very likely",
                             "virtually certain"), val = 1:7)
likCols = brewer.pal(nrow(likeops), "PuBu")

# Subjective confidence
confidence = data.frame(ans = c("not at all confident", "not very confident", 
                                "moderately confident",
                                "very confident", "extremely confident"), val = 1:5)
conCols = brewer.pal(nrow(confidence), "RdPu")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1 ~ graph, data = protab,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 2))

# Check for normality using Shapiro-Wilk normality test
res_aovchal1 <- aov(CHAL1 ~ graph, data = protab)
qqPlot(res_aovchal1$residuals, id = FALSE)
boxplot(CHAL1 ~ graph, data = protab)
hist(res_aovchal1$residuals)

# # One-way ANOVA
# # 1st method:
# oneway.test(CHAL1 ~ graph, data = protab, var.equal = TRUE)
# 
# # 2nd method:
# summary(res_aovchal1)
# report(res_aovchal1)

### Protection
# Kruskal-Wallis test and post-hoc test
kruskal.test(CHAL1 ~ graph, data = protab)
dunnTest(CHAL1 ~ graph, data = protab, method = "holm")

# Mood's Median test and post-hoc test (Pairwise Median Test)
# https://rcompanion.org/handbook/F_09.html
median_test(CHAL1 ~ graph, data = protab)
pairwiseMedianTest(CHAL1 ~ graph, data = protab, exact  = NULL, method = "fdr")

# Driveway scenario -------------------------------------------------------
# create bar plot data
gldat <- protab %>%
  dplyr::group_by(graph, CHAL1) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1 = factor(CHAL1, 
                               levels = 1:4,
                               labels = c("do nothing",
                                          "protect 2 events/yr", 
                                          "protect 4 events/yr",
                                          "pave the driveway\n(120 events)")))%>%
  dplyr::mutate(graph = factor(graph, levels = c("Bar", "Box", "Area")))

driveCols = brewer.pal(length(levels(gldat$CHAL1)), "OrRd")

# create grouped bar plot
drive_dec = gldat %>%
  ggplot(aes(CHAL1, Percent, fill = CHAL1)) + facet_grid(~graph) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + scale_fill_manual(values=driveCols, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
  theme(legend.title = element_text(size=6), legend.text = element_text(size=6), 
        axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.title = element_text(size = 7),
        strip.text = element_text(size = 6), axis.text.y = element_text(size=6),
        legend.key.size = unit(0.4, 'cm'), legend.box.spacing = unit(0, "pt")) + 
  geom_text(aes(label = round(Percent)), vjust=-0.1, position = position_dodge(0.9), size = 1.9) +
  labs(x = "Driveway washout scenario", y= "Percent (%) of participants", 
       fill="Protection options")

# Evaluate confidence
ptab$CHAL1_conVal = confidence$val[match(ptab$CHAL1_con, confidence$ans)]
ptab$graph = factor(ptab$graph, levels = c("Bar", "Box", "Area"))

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1_conVal ~ graph, data = ptab,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 2))

# Check for normality using Shapiro-Wilk normality test
res_aovcon1 <- aov(CHAL1_conVal ~ graph, data = ptab)
qqPlot(res_aovcon1$residuals, id = FALSE)
boxplot(CHAL1_conVal ~ graph, data = ptab)

kruskal.test(CHAL1_conVal ~ graph, data = ptab)
dunnTest(CHAL1_conVal ~ graph, data = ptab, method = "holm")

median_test(CHAL1_conVal ~ graph, data = ptab)
pairwiseMedianTest(CHAL1_conVal ~ graph, data = ptab, exact  = NULL, method = "fdr")

# # One-way ANOVA
# # 1st method:
# oneway.test(CHAL1_conVal ~ graph, data = ptab, var.equal = TRUE)
# 
# # 2nd method:
# summary(res_aovchal1)
# report(res_aovchal1)

# Percent by total data
ptab %>%
  dplyr::group_by(CHAL1_conVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 2)) %>%
  dplyr::mutate(CHAL1_conVal = factor(CHAL1_conVal, levels = confidence$val,
                                      labels = confidence$ans))

# create bar plot data by graph type
gldatcondrive <- ptab %>%
  dplyr::group_by(graph, CHAL1_conVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1_conVal = factor(CHAL1_conVal, levels = confidence$val,
                                      labels = confidence$ans)) %>%
  dplyr::mutate(graph = factor(graph, levels = c("Bar", "Box", "Area")))

# create grouped bar plot
drive_con = ggplot(gldatcondrive, aes(CHAL1_conVal, Percent, fill = CHAL1_conVal)) +
  facet_grid(~graph) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() +
  scale_fill_manual(values=conCols, labels = function(x) str_wrap(x, width = 18)) +
  theme_bw() + geom_text(aes(label = round(Percent)), vjust=-0.1, position = position_dodge(0.9), size = 1.9) +
  theme(legend.title = element_text(size=6), legend.text = element_text(size=6), 
        axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.title = element_text(size = 7),
        strip.text = element_text(size = 6), axis.text.y = element_text(size=6),
        legend.key.size = unit(0.4, 'cm'), legend.box.spacing = unit(0, "pt")) +
  labs(x = "Driveway washout scenario", y= "Percent (%) of participants", 
       fill=str_wrap(questionslist$Area_Freq$CHAL1.3, width = 20))

# Evaluate risk perception
ptab$CHAL1_likVal = likeops$val[match(ptab$CHAL1_lik, likeops$ans)]
# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL1_likVal ~ graph, data = ptab,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 2))

# Check for normality using Shapiro-Wilk normality test
res_aovlik1 <- aov(CHAL1_likVal ~ graph, data = ptab)
qqPlot(res_aovlik1$residuals, id = FALSE)
boxplot(CHAL1_likVal ~ graph, data = ptab)

kruskal.test(CHAL1_likVal ~ graph, data = ptab)
dunnTest(CHAL1_likVal ~ graph, data = ptab, method = "holm")

median_test(CHAL1_likVal ~ graph, data = ptab)
pairwiseMedianTest(CHAL1_likVal ~ graph, data = ptab, exact  = NULL, method = "fdr")

# # One-way ANOVA
# # 1st method:
# oneway.test(CHAL1_likVal ~ graph, data = ptab, var.equal = TRUE)
# 
# # 2nd method:
# summary(res_aovlik1)
# report(res_aovlik1)

ptab %>%
  dplyr::group_by(CHAL1_likVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 2)) %>%
  dplyr::mutate(CHAL1_likVal = factor(CHAL1_likVal, levels = likeops$val,
                                      labels = likeops$ans))

# create bar plot data
gldatlik <- ptab %>%
  dplyr::group_by(graph, CHAL1_likVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL1_likVal = factor(CHAL1_likVal, levels = likeops$val,
                                      labels = likeops$ans)) %>%
  dplyr::mutate(graph = factor(graph, levels = c("Bar", "Box", "Area")))

# create grouped bar plot
drive_risk = ggplot(gldatlik, aes(CHAL1_likVal, Percent, fill = CHAL1_likVal)) +
  facet_grid(~graph) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent)), vjust=-0.1, position = position_dodge(0.9), size = 1.9) +
  scale_fill_manual(values=likCols, labels = function(x) str_wrap(x, width = 21)) +
  theme_bw() +
  theme(legend.title = element_text(size=6), legend.text = element_text(size=6), 
        axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.title = element_text(size = 7),
        strip.text = element_text(size = 6), axis.text.y = element_text(size=6),
        legend.key.size = unit(0.4, 'cm'), legend.box.spacing = unit(0, "pt")) +
  labs(x = "Driveway washout scenario", y= "Percent (%) of participants", 
       fill=str_wrap("How likely do you think you’ll see a year with 4 or more heavy rainfall events in the next 30 years?", 
                     width = 20)) #questionslist$Area_Freq$CHAL1.4

# Flood scenario ----------------------------------------------------------

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2 ~ graph, data = protab,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 2))

kruskal.test(CHAL2 ~ graph, data = protab)
dunnTest(CHAL2 ~ graph, data = protab, method = "holm")

# median_test(CHAL2 ~ graph, data = protab)
# pairwiseMedianTest(CHAL2 ~ graph, data = protab, exact  = NULL, method = "fdr")

protab %>%
  dplyr::group_by(CHAL2) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2 = factor(CHAL2, 
                               levels = 0:1,
                               labels = c("no", "yes")))

# Decision
gldatflood <- protab %>%
  dplyr::group_by(graph, CHAL2) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2 = factor(CHAL2, 
                               levels = 0:1,
                               labels = c("no", "yes"))) %>%
  dplyr::mutate(graph = factor(graph, levels = c("Bar", "Box", "Area")))

# create grouped bar plot
flood_dec = gldatflood %>%
  ggplot(aes(CHAL2, Percent, fill = CHAL2)) +
  facet_grid(~graph) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + geom_text(aes(label = round(Percent)), vjust=-0.1, position = position_dodge(0.9), size = 1.9) +
  scale_fill_manual(values=driveCols[c(1,4)], labels = function(x) str_wrap(x, width = 21)) + theme_bw() +
  theme(legend.title = element_text(size=6), legend.text = element_text(size=6), 
        axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.title = element_text(size = 7),
        strip.text = element_text(size = 6), axis.text.y = element_text(size=6),
        legend.key.size = unit(0.4, 'cm'), legend.box.spacing = unit(0, "pt")) +
  labs(x = "Flood insurance scenario", y= "Percent (%) of participants", 
       fill="Buy flood insurance?     ") # Spaces are added to maintain plot alignment

# Evaluate confidence
ptab$CHAL2_conVal = confidence$val[match(ptab$CHAL2_con, confidence$ans)]

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2_conVal ~ graph, data = ptab,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 2))

kruskal.test(CHAL2_conVal ~ graph, data = ptab)
dunnTest(CHAL2_conVal ~ graph, data = ptab, method = "holm")

median_test(CHAL2_conVal ~ graph, data = ptab)
pairwiseMedianTest(CHAL2_conVal ~ graph, data = ptab, exact  = NULL, method = "fdr")

# Check for normality using Shapiro-Wilk normality test
res_aovcon2 <- aov(CHAL2_conVal ~ graph, data = ptab)
qqPlot(res_aovcon2$residuals, id = FALSE)
boxplot(CHAL2_conVal ~ graph, data = ptab)

# # One-way ANOVA
# # 1st method:
# oneway.test(CHAL2_conVal ~ graph, data = ptab, var.equal = TRUE)
# 
# # 2nd method:
# summary(res_aovcon2)
# report(res_aovcon2)

ptab %>%
  dplyr::group_by(CHAL2_conVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 2)) %>%
  dplyr::mutate(CHAL2_conVal = factor(CHAL2_conVal, levels = confidence$val,
                                      labels = confidence$ans))

# create bar plot data
gldatconflo <- ptab %>%
  dplyr::group_by(graph, CHAL2_conVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2_conVal = factor(CHAL2_conVal, levels = confidence$val,
                                      labels = confidence$ans)) %>%
  dplyr::mutate(graph = factor(graph, levels = c("Bar", "Box", "Area")))

# create grouped bar plot
flood_con = ggplot(gldatconflo, aes(CHAL2_conVal, Percent, fill = CHAL2_conVal)) +
  facet_grid(~graph) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() +
  scale_fill_manual(values=conCols, labels = function(x) str_wrap(x, width = 18)) +
  theme_bw() + geom_text(aes(label = round(Percent)), vjust=-0.1, position = position_dodge(0.9), size = 1.9) +
  theme(legend.title = element_text(size=6), legend.text = element_text(size=6), 
        axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.title = element_text(size = 7),
        strip.text = element_text(size = 6), axis.text.y = element_text(size=6),
        legend.key.size = unit(0.4, 'cm'), legend.box.spacing = unit(0, "pt")) +
  labs(x = "Flood insurance scenario", y= "Percent (%) of participants", 
       fill=str_wrap("How confident are you that you will be protected financially from a flood, based on your choice and the information given to you?", 
                     width = 20)) # questionslist$Area_Freq$CHAL2.3

# Evaluate risk perception
ptab$CHAL2_likVal = likeops$val[match(ptab$CHAL2_lik, likeops$ans)]

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2_likVal ~ graph, data = ptab,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 2))

# Check for normality using Shapiro-Wilk normality test
res_aovlik2 <- aov(CHAL2_likVal ~ graph, data = ptab)
qqPlot(res_aovlik2$residuals, id = FALSE)
boxplot(CHAL2_likVal ~ graph, data = ptab)

kruskal.test(CHAL2_likVal ~ graph, data = ptab)
dunnTest(CHAL2_likVal ~ graph, data = ptab, method = "holm")

median_test(CHAL2_likVal ~ graph, data = ptab)
pairwiseMedianTest(CHAL2_likVal ~ graph, data = ptab, exact  = NULL, method = "fdr")

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(CHAL2_likVal ~ graph, data = ptab,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 2))

aggregate(CHAL1_likVal ~ graph, data = ptab,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 2))

aggregate(CHAL2_conVal ~ graph, data = ptab,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 2))

aggregate(CHAL1_conVal ~ graph, data = ptab,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 2))

ptab %>%
  dplyr::group_by(CHAL2_likVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 2)) %>%
  dplyr::mutate(CHAL2_likVal = factor(CHAL2_likVal, levels = likeops$val,
                                      labels = likeops$ans))

# create bar plot data
gldatlikflo <- ptab %>%
  dplyr::group_by(graph, CHAL2_likVal) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) %>%
  dplyr::mutate(CHAL2_likVal = factor(CHAL2_likVal, levels = likeops$val,
                                      labels = likeops$ans)) %>%
  dplyr::mutate(graph = factor(graph, levels = c("Bar", "Box", "Area")))

# create grouped bar plot
flood_risk = ggplot(gldatlikflo, aes(CHAL2_likVal, Percent, fill = CHAL2_likVal)) +
  facet_grid(~graph) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  geom_line() + 
  scale_fill_manual(values=likCols, labels = function(x) str_wrap(x, width = 21)) +
  theme_bw() + geom_text(aes(label = round(Percent)), vjust=-0.1, position = position_dodge(0.9), size = 1.9) +
  theme(legend.title = element_text(size=6), legend.text = element_text(size=6), 
        axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.title = element_text(size = 7),
        strip.text = element_text(size = 6), axis.text.y = element_text(size=6),
        legend.key.size = unit(0.4, 'cm'), legend.box.spacing = unit(0, "pt")) +
  labs(x = "Flood insurance scenario", y= "Percent (%) of participants", 
       fill=str_wrap("How likely do you think another flood will occur from a heavy rainfall event in the next 30 years?", 
                     width = 20)) # questionslist$Area_Freq$CHAL2.4

# Read in notes with primary codes ----------------------------------------
prochoices = read.csv("data/protectiveTable_5june2024.csv")

prochoices$graph = prochoices$name
prochoices$graph = ifelse(prochoices$graph == "Area_Freq" | prochoices$graph == "Area_Int", 
                          "Area", prochoices$graph)
prochoices$graph = ifelse(prochoices$graph == "Bar_freq" | prochoices$graph == "Bar_int", 
                          "Bar", prochoices$graph)
prochoices$graph = ifelse(prochoices$graph == "Box_freq" | prochoices$graph == "Box_int", 
                          "Box", prochoices$graph)


# Read in code topics (secondary coding) ----------------------------------
topics = read.csv("data/codebook.csv")

# Create a vector of colors based on secondary codes
# color_codes = data.frame(code = unique(topics$What.influences.participant.decisions.), 
#                          colors = brewer.pal(length(unique(topics$What.influences.participant.decisions.)), 
#                                              "Paired"))
color_codes = data.frame(code = unique(topics$AI.Analysis.of.Scenario.Justification..ChatGPT.), 
                         colors = brewer.pal(length(unique(topics$AI.Analysis.of.Scenario.Justification..ChatGPT.)), 
                                             "Paired"))

# Create tables of code frequency and wordclouds --------------------------
# Create tables -----------------------------------------------------------
drive_tab = list()

par(mar = c(0, 0, 0, 0))
drive_tab$Area = word_freq(prochoices$Primary.cycle.code[prochoices$graph == "Area"], color_codes, "paper1/primary_coding_drivewayArea.csv", 
                      "paper1/secondary_coding_drivewayArea.csv")
drive_tab$Bar = word_freq(prochoices$Primary.cycle.code[prochoices$graph == "Bar"], color_codes, "paper1/primary_coding_drivewayBar.csv", 
                           "paper1/secondary_coding_drivewayBar.csv")
drive_tab$Box = word_freq(prochoices$Primary.cycle.code[prochoices$graph == "Box"], color_codes, "paper1/primary_coding_drivewayBox.csv", 
                           "paper1/secondary_coding_drivewayBox.csv")
# secondary
sec_tab = data.frame(topic = unique(topics$AI.Analysis.of.Scenario.Justification..ChatGPT.))
# sec_tab = data.frame(topic = unique(topics$What.influences.participant.decisions.))
for(i in 1:length(sec_tab$topic)){
  sec_tab$Areacount[i] = sum(drive_tab$Area$count[which(drive_tab$Area$topic == sec_tab$topic[i])])
  sec_tab$Barcount[i] = sum(drive_tab$Bar$count[which(drive_tab$Bar$topic == sec_tab$topic[i])])
  sec_tab$Boxcount[i] = sum(drive_tab$Box$count[which(drive_tab$Box$topic == sec_tab$topic[i])])
}
sec_tab$color = color_codes$colors[match(sec_tab$topic, color_codes$code)]
sec_tab$topic = gsub("\\sand", " &", sec_tab$topic)

secdriveway = data.frame(topic = rep(sec_tab$topic, 3))
secdriveway$count = c(sec_tab$Areacount, sec_tab$Barcount, sec_tab$Boxcount)
secdriveway$percent = c(round(sec_tab$Areacount/sum(sec_tab$Areacount)*100, 1), 
                        round(sec_tab$Barcount/sum(sec_tab$Barcount)*100, 1), 
                        round(sec_tab$Boxcount/sum(sec_tab$Boxcount)*100, 1))
secdriveway$graph = c(rep("Area", nrow(sec_tab)), rep("Bar", nrow(sec_tab)),
                      rep("Box", nrow(sec_tab)))
secdriveway$graph = factor(secdriveway$graph, levels = c("Bar", "Box", "Area"))

s_codeind = order(rowSums(sec_tab[ ,2:4]))
secdriveway$topic = factor(secdriveway$topic, levels=sec_tab$topic[s_codeind])

# Grouped secondary
drive_sec_reason = ggplot(secdriveway, aes(reorder_within(topic, percent, graph), percent, fill = topic)) +
  geom_bar(stat = 'identity', position=position_dodge(), colour="black", linewidth = 0.1) +
  scale_fill_manual(values=sec_tab$color, labels = function(x) str_wrap(x, width = 16)) + theme_bw() +
  facet_wrap(~graph, scales = "free_x") +
  theme(legend.title = element_text(size=6), legend.text = element_text(size=6), 
        axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.title = element_text(size = 7),
        strip.text = element_text(size = 6), axis.text.y = element_text(size=6),
        legend.key.size = unit(0.4, 'cm'), legend.box.spacing = unit(0, "pt"),
        legend.key.spacing.y = unit(2, "pt")) +
  geom_text(aes(label = round(percent)), vjust=-0.1, position = position_dodge(0.9), size = 1.9) +
  labs(x = "Driveway washout scenario", y= "Percent (%) of participants", 
       fill="Reason")

# Insurance scenario -------------------------------------------------------
flood_tab = list()

# Fix the I don't understand
prochoices$Primary.cycle.code.1[which(prochoices$Primary.cycle.code.1 == "I don\xd5t understand")] = "I don't understand"

par(mar = c(0, 0, 0, 0))
flood_tab$Area = word_freq(prochoices$Primary.cycle.code.1[prochoices$graph == "Area"], color_codes, "paper1/primary_coding_floodArea.csv", 
                           "paper1/secondary_coding_floodArea.csv")
flood_tab$Bar = word_freq(prochoices$Primary.cycle.code.1[prochoices$graph == "Bar"], color_codes, "paper1/primary_coding_floodBar.csv", 
                          "paper1/secondary_coding_floodBar.csv")
flood_tab$Box = word_freq(prochoices$Primary.cycle.code.1[prochoices$graph == "Box"], color_codes, "paper1/primary_coding_floodBox.csv", 
                          "paper1/secondary_coding_floodBox.csv")

# secondary
# sec_tab_flood = data.frame(topic = unique(topics$What.influences.participant.decisions.))
sec_tab_flood = data.frame(topic = unique(topics$AI.Analysis.of.Scenario.Justification..ChatGPT.))
for(i in 1:length(sec_tab_flood$topic)){
  sec_tab_flood$Areacount[i] = sum(flood_tab$Area$count[which(flood_tab$Area$topic == sec_tab_flood$topic[i])])
  sec_tab_flood$Barcount[i] = sum(flood_tab$Bar$count[which(flood_tab$Bar$topic == sec_tab_flood$topic[i])])
  sec_tab_flood$Boxcount[i] = sum(flood_tab$Box$count[which(flood_tab$Box$topic == sec_tab_flood$topic[i])])
}
sec_tab_flood$color = color_codes$colors[match(sec_tab_flood$topic, color_codes$code)]
sec_tab_flood$topic = gsub("\\sand", " &", sec_tab_flood$topic)

secfloodway = data.frame(topic = rep(sec_tab_flood$topic, 3))
secfloodway$count = c(sec_tab_flood$Areacount, sec_tab_flood$Barcount, sec_tab_flood$Boxcount)
secfloodway$percent = c(round(sec_tab_flood$Areacount/sum(sec_tab_flood$Areacount)*100, 1), 
                        round(sec_tab_flood$Barcount/sum(sec_tab_flood$Barcount)*100, 1), 
                        round(sec_tab_flood$Boxcount/sum(sec_tab_flood$Boxcount)*100, 1))
secfloodway$graph = c(rep("Area", nrow(sec_tab_flood)), rep("Bar", nrow(sec_tab_flood)),
                      rep("Box", nrow(sec_tab_flood)))
secfloodway$graph = factor(secfloodway$graph, levels = c("Bar", "Box", "Area"))

s_codeind = order(rowSums(sec_tab[ ,2:4]))

secfloodway$topic = factor(secfloodway$topic, levels=sec_tab_flood$topic[s_codeind])

# grouped secondary
flood_sec_reason = ggplot(secfloodway, aes(reorder_within(topic, percent, graph), percent, fill = topic)) +
  geom_bar(stat = 'identity', position=position_dodge(), colour="black", linewidth = 0.1) +
  scale_fill_manual(values=sec_tab_flood$color, labels = function(x) str_wrap(x, width = 16)) + theme_bw() +
  facet_wrap(~graph, scales = "free_x") +
  theme(legend.title = element_text(size=6), legend.text = element_text(size=6), 
        axis.text.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.title = element_text(size = 7),
        strip.text = element_text(size = 6), axis.text.y = element_text(size=6),
        legend.key.size = unit(0.4, 'cm'), legend.box.spacing = unit(0, "pt"), 
        legend.key.spacing.y = unit(2, "pt")) +
  geom_text(aes(label = round(percent)), vjust=-0.1, position = position_dodge(0.9), size = 1.9) +
  labs(x = "Flood insurance scenario", y= "Percent (%) of participants", 
       fill="Reason")

## Fig. 5
png(file="paper1/Fig5_DecisionAll_percenttest.png", family="Helvetica", res=300,
    units="in", width=maximum_width, height=column_height*3, pointsize=10)
plot_grid(drive_dec, flood_dec, drive_con, flood_con, drive_risk, flood_risk,
          drive_sec_reason, flood_sec_reason,
          nrow=4, labels = "auto") # , align = "h", axis = "b"
dev.off()

pdf(file="paper1/figure05.pdf", family="Helvetica", width=maximum_width, height=column_height*2.75)
plot_grid(drive_dec, flood_dec, drive_con, flood_con, drive_risk, flood_risk,
          drive_sec_reason, flood_sec_reason,
          nrow=4, labels = "auto", label_size=10) # , align = "h", axis = "b"
dev.off()
## Fig. 5

# png(file="Pub1/Fig4_Decision_poster.png", family="Helvetica", res=300,
#     units="in", width=column_height*8, height=7, pointsize=10)
# plot_grid(drive_dec, drive_con, drive_risk, drive_sec_reason, flood_dec, 
#           flood_con, flood_risk, flood_sec_reason,
#           nrow=2, labels = "auto") # , align = "h", axis = "b"
# dev.off()

# ##########################################################################
# # General
# ##########################################################################
# # Read in general notes ---------------------------------------------------
# genFeed = read.csv("data/generalFeedback_June5.csv")
# topics = read.csv("data/generalcodebook.csv")
# 
# genFeed$graph = genFeed$block
# genFeed$graph = ifelse(genFeed$graph == "Area_Freq" | genFeed$graph == "Area_Int", 
#                        "Area", genFeed$graph)
# genFeed$graph = ifelse(genFeed$graph == "Bar_freq" | genFeed$graph == "Bar_int", 
#                        "Bar", genFeed$graph)
# genFeed$graph = ifelse(genFeed$graph == "Box_freq" | genFeed$graph == "Box_int", 
#                        "Box", genFeed$graph)
# 
# gen_tabtotal = word_freq(genFeed$primary.cycle.codes, color_codes, "primary_coding_generalTot.csv", 
#                          "secondary_coding_generalTot.csv")
# 
# gen_tab = list()
# 
# par(mar = c(0, 0, 0, 0))
# gen_tab$Area = word_freq(genFeed$primary.cycle.codes[genFeed$graph == "Area"], color_codes, "primary_coding_generalArea.csv", 
#                            "secondary_coding_generalArea.csv")
# gen_tab$Bar = word_freq(genFeed$primary.cycle.codes[genFeed$graph == "Bar"], color_codes, "primary_coding_generalBar.csv", 
#                           "secondary_coding_generalBar.csv")
# gen_tab$Box = word_freq(genFeed$primary.cycle.codes[genFeed$graph == "Box"], color_codes, "primary_coding_generalBox.csv", 
#                           "secondary_coding_generalBox.csv")
# # secondary
# sec_gen = data.frame(topic = unique(topics$What.influences.participant.decisions.))
# for(i in 1:length(sec_gen$topic)){
#   sec_gen$Areacount[i] = sum(gen_tab$Area$count[which(gen_tab$Area$topic == sec_gen$topic[i])])
#   sec_gen$Barcount[i] = sum(gen_tab$Bar$count[which(gen_tab$Bar$topic == sec_gen$topic[i])])
#   sec_gen$Boxcount[i] = sum(gen_tab$Box$count[which(gen_tab$Box$topic == sec_gen$topic[i])])
# }
# sec_gen$color = color_codes$colors[match(sec_gen$topic, color_codes$code)]
# 
# secgenfeed = data.frame(topic = rep(sec_gen$topic, 3))
# secgenfeed$count = c(sec_gen$Areacount, sec_gen$Barcount, sec_gen$Boxcount)
# secgenfeed$percent = c(round(sec_gen$Areacount/sum(sec_gen$Areacount)*100, 1), 
#                         round(sec_gen$Barcount/sum(sec_gen$Barcount)*100, 1), 
#                         round(sec_gen$Boxcount/sum(sec_gen$Boxcount)*100, 1))
# secgenfeed$graph = c(rep("Area", nrow(sec_gen)), rep("Bar", nrow(sec_gen)),
#                       rep("Box", nrow(sec_gen)))
# secgenfeed$graph = factor(secgenfeed$graph, levels = c("Bar", "Box", "Area"))
# 
# s_gencodeind = order(rowSums(sec_gen[ ,2:4]))
# secgenfeed$topic = factor(secgenfeed$topic, levels=sec_gen$topic[s_gencodeind])
# 
# # Grouped secondary
# # drive_sec_reason = 
# ggplot(secgenfeed, aes(reorder_within(topic, percent, graph), percent, fill = topic)) +
#   geom_bar(stat = 'identity', position=position_dodge(), colour="black", linewidth = 0.1) +
#   scale_fill_manual(values=sec_gen$color, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
#   facet_wrap(~graph, scales = "free_x") + 
#   theme(legend.title = element_text(size=9), legend.text = element_text(size=8), 
#         axis.text.x=element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   labs(x = "General feedback", y= "Percent (%) of participants", 
#        fill="Issues")
# 
# # Primary graph -----------------------------------------------------------
# primarygengraph = data.frame(reason = topics$What.influences.participant.decisions., word = topics$Code)
# 
# genprim_flood = merge(primarygengraph, gen_tab$Bar[, c("word", "count")], by="word", all = T)
# genprim_flood = merge(genprim_flood, gen_tab$Box[, c("word", "count")], by="word", all = T)
# genprim_flood = merge(genprim_flood, gen_tab$Area[, c("word", "count")], by="word", all = T)
# gencom_flood = arrange(genprim_flood, genprim_flood$reason)
# gencom_flood$color = color_codes$colors[match(gencom_flood$reason, color_codes$code)]
# gencom_flood$count.x = ifelse(is.na(gencom_flood$count.x), 0, gencom_flood$count.x)
# gencom_flood$count.y = ifelse(is.na(gencom_flood$count.y), 0, gencom_flood$count.y)
# gencom_flood$count = ifelse(is.na(gencom_flood$count), 0, gencom_flood$count)
# 
# gencom_flood$pbar = round(gencom_flood$count.x/sum(gencom_flood$count.x) * 100, 1)
# gencom_flood$pbox = round(gencom_flood$count.y/sum(gencom_flood$count.y) * 100, 1)
# gencom_flood$parea = round(gencom_flood$count/sum(gencom_flood$count) * 100, 1)
# 
# genprimary.df_flood = data.frame(word = rep(gencom_flood$word,3), reason = rep(gencom_flood$reason,3), 
#                               percent = c(gencom_flood$pbar, gencom_flood$pbox, gencom_flood$parea),
#                               graph = c(rep("Bar", nrow(gencom_flood)), rep("Box", nrow(gencom_flood)),
#                                         rep("Area", nrow(gencom_flood))))
# genprimary.df_flood$word = factor(genprimary.df_flood$word, levels = gencom_flood$word)
# genprimary.df_flood$graph = factor(genprimary.df_flood$graph, levels = c("Bar", "Box", "Area"))
# 
# insightful_general = genprimary.df_flood[!genprimary.df_flood$reason == "not insightful", ]
# 
# 
# 
# 
# gen_tabtotal$percent = round(gen_tabtotal$count/sum(gen_tabtotal$count) * 100, 1)
# insightful_gen_tabtotal = gen_tabtotal[!gen_tabtotal$topic == "not insightful", ]
# insightful_gen_tabtotal = arrange(insightful_gen_tabtotal, insightful_gen_tabtotal$topic)
# 
# insightful_gen_tabtotal$word = factor(insightful_gen_tabtotal$word, levels = insightful_gen_tabtotal$word)
# 
# # y = "General feedback", 
# png(file=paste0("paper1/general_feedback.png"), 
#     family="Helvetica", res=300, 
#     units="in", width=8, height=8, pointsize=10)
# par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))
# 
# ggplot(insightful_gen_tabtotal, aes(count, word, fill = topic)) +
#   geom_bar(stat = 'identity', position=position_dodge(), colour="black", linewidth = 0.1) +
#   scale_fill_manual(values=sec_gen$color, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
#   # add text and define color
#   geom_text(aes(label=count), hjust=-0.2, color="black", 
#             # define text position and size
#             position = position_dodge(0.9),  size=3.5) + 
#   theme(legend.title = element_text(size=9), legend.text = element_text(size=8), 
#         # axis.text.y=element_blank(), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   labs(y="", x= "Number of participants", 
#        fill="Primary issues")
# dev.off()
# 
# ggplot(insightful_general, aes(percent, word, fill = reason)) +
#   geom_bar(stat = 'identity', position=position_dodge(), colour="black", linewidth = 0.1) +
#   scale_fill_manual(values=sec_gen$color, labels = function(x) str_wrap(x, width = 20)) + theme_bw() +
#   facet_wrap(~graph, scales = "free_x") +
#   theme(legend.title = element_text(size=9), legend.text = element_text(size=8), 
#         axis.text.x=element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   labs(x = "General feedback", y= "Percent (%) of participants", 
#        fill="Primary issues")

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

# Political ---------------------------------------------------------------
political = as.numeric(c(split.by.block$Area_Freq$DEM7_AF, split.by.block$Area_Int$DEM7_AI,
                         split.by.block$Bar_freq$DEM7_BF, split.by.block$Bar_int$DEM7_BI,
                         split.by.block$Box_freq$DEM7_XF, split.by.block$Box_int$DEM7_XI))

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

# Age ---------------------------------------------------------------------
age = 2024 - as.numeric(c(split.by.block$Area_Freq$DEM3_AF, split.by.block$Area_Int$DEM3_AI, 
                          split.by.block$Bar_freq$DEM3_BF, split.by.block$Bar_int$DEM3_BI, 
                          split.by.block$Box_freq$DEM3_XF, split.by.block$Box_int$DEM3_XI))

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
# DEMrace[multirace, ]

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

ageedu = data.frame(age=age, edu=edu)
ageedu25 = ageedu[ageedu$age >= 25, ]
table(ageedu25$edu)/length(ageedu25$edu)*100
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

all.size.df<- as.data.frame(cbind(overAcc, DEMrespcensus))
all.pro.df<- as.data.frame(cbind(protab[,1:2], all.size.df))

# Calculate descriptive stats, i.e., mean and standard deviation
aggregate(val ~ gender, data = all.pro.df,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 1))
aggregate(val ~ work, data = all.pro.df,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 1))
aggregate(val ~ edu, data = all.pro.df,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 1))
aggregate(val ~ race_largest, data = all.pro.df,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 1))
aggregate(val ~ region, data = all.pro.df,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 1))

# Table S2 ----------------------------------------------------------------
# Gender
round((table(all.size.df$gender[all.size.df$name == "Area"])/length(all.size.df$gender[all.size.df$name == "Area"]))*100, 1)
round((table(all.size.df$gender[all.size.df$name == "Box"])/length(all.size.df$gender[all.size.df$name == "Box"]))*100, 1)
round((table(all.size.df$gender[all.size.df$name == "Bar"])/length(all.size.df$gender[all.size.df$name == "Bar"]))*100, 1)
round((table(all.size.df$gender)/length(all.size.df$gender))*100, 1)

# Ethnicity
round((table(all.size.df$latino[all.size.df$name == "Area"])/length(all.size.df$latino[all.size.df$name == "Area"]))*100, 1)
round((table(all.size.df$latino[all.size.df$name == "Box"])/length(all.size.df$latino[all.size.df$name == "Box"]))*100, 1)
round((table(all.size.df$latino[all.size.df$name == "Bar"])/length(all.size.df$latino[all.size.df$name == "Bar"]))*100, 1)
round((table(all.size.df$latino)/length(all.size.df$latino))*100, 1)

# Race
round((table(all.size.df$racelarge[all.size.df$name == "Area"])/length(all.size.df$racelarge[all.size.df$name == "Area"]))*100, 1)
round((table(all.size.df$racelarge[all.size.df$name == "Box"])/length(all.size.df$racelarge[all.size.df$name == "Box"]))*100, 1)
round((table(all.size.df$racelarge[all.size.df$name == "Bar"])/length(all.size.df$racelarge[all.size.df$name == "Bar"]))*100, 1)
round((table(all.size.df$racelarge)/length(all.size.df$racelarge))*100, 1)

# Education
round((table(all.size.df$edu[all.size.df$name == "Area"])/length(all.size.df$edu[all.size.df$name == "Area"]))*100, 1)
round((table(all.size.df$edu[all.size.df$name == "Box"])/length(all.size.df$edu[all.size.df$name == "Box"]))*100, 1)
round((table(all.size.df$edu[all.size.df$name == "Bar"])/length(all.size.df$edu[all.size.df$name == "Bar"]))*100, 1)
round((table(all.size.df$edu)/length(all.size.df$edu))*100, 1)

aggregate(val ~ edu, data = all.size.df,
          function(x) round(c(mean = mean(x), med = median(x), sd = sd(x)), 2))

# Employment
round((table(all.size.df$work[all.size.df$name == "Area"])/length(all.size.df$work[all.size.df$name == "Area"]))*100, 1)
round((table(all.size.df$work[all.size.df$name == "Box"])/length(all.size.df$work[all.size.df$name == "Box"]))*100, 1)
round((table(all.size.df$work[all.size.df$name == "Bar"])/length(all.size.df$work[all.size.df$name == "Bar"]))*100, 1)
round((table(all.size.df$work)/length(all.size.df$work))*100, 1)

# Region
round((table(all.size.df$region[all.size.df$name == "Area"])/length(all.size.df$region[all.size.df$name == "Area"]))*100, 1)
round((table(all.size.df$region[all.size.df$name == "Box"])/length(all.size.df$region[all.size.df$name == "Box"]))*100, 1)
round((table(all.size.df$region[all.size.df$name == "Bar"])/length(all.size.df$region[all.size.df$name == "Bar"]))*100, 1)
round((table(all.size.df$region)/length(all.size.df$region))*100, 1)

# Division
round((table(all.size.df$division[all.size.df$name == "Area"])/length(all.size.df$division[all.size.df$name == "Area"]))*100, 1)
round((table(all.size.df$division[all.size.df$name == "Box"])/length(all.size.df$division[all.size.df$name == "Box"]))*100, 1)
round((table(all.size.df$division[all.size.df$name == "Bar"])/length(all.size.df$division[all.size.df$name == "Bar"]))*100, 1)
round((table(all.size.df$division)/length(all.size.df$division))*100, 1)

# Climate group
round((table(all.size.df$climGroup[all.size.df$name == "Area"])/length(all.size.df$climGroup[all.size.df$name == "Area"]))*100, 1)
round((table(all.size.df$climGroup[all.size.df$name == "Box"])/length(all.size.df$climGroup[all.size.df$name == "Box"]))*100, 1)
round((table(all.size.df$climGroup[all.size.df$name == "Bar"])/length(all.size.df$climGroup[all.size.df$name == "Bar"]))*100, 1)
round((table(all.size.df$climGroup)/length(all.size.df$climGroup))*100, 1)

# Political group
politicalgroups = all.size.df$politics
polGroup = ifelse(politicalgroups <= 3, "Liberal", politicalgroups)
polGroup = ifelse(politicalgroups >= 7, "Conservative", polGroup)
polGroup = ifelse(politicalgroups >= 4 & politicalgroups <= 6, "Neutral", polGroup)
round((table(polGroup[all.size.df$name == "Area"])/length(polGroup[all.size.df$name == "Area"]))*100, 1)
round((table(polGroup[all.size.df$name == "Box"])/length(polGroup[all.size.df$name == "Box"]))*100, 1)
round((table(polGroup[all.size.df$name == "Bar"])/length(polGroup[all.size.df$name == "Bar"]))*100, 1)
round((table(polGroup)/length(polGroup))*100, 1)

# Age group
agegroups = all.size.df$age
ageGroup = ifelse(agegroups <= 34, "18-34",agegroups)
ageGroup = ifelse(agegroups >= 55, "55+", ageGroup)
ageGroup = ifelse(agegroups >= 35 & agegroups <= 54, "35-54", ageGroup)
round((table(ageGroup[all.size.df$name == "Area"])/length(ageGroup[all.size.df$name == "Area"]))*100, 1)
round((table(ageGroup[all.size.df$name == "Box"])/length(ageGroup[all.size.df$name == "Box"]))*100, 1)
round((table(ageGroup[all.size.df$name == "Bar"])/length(ageGroup[all.size.df$name == "Bar"]))*100, 1)
round((table(ageGroup)/length(ageGroup))*100, 1)

# Income group
incomegp = all.size.df$money
incomeGroup = ifelse(incomegp < 25000, ">25",incomegp)
incomeGroup = ifelse(incomegp >= 25000 & agegroups < 50000, "25-49", incomeGroup)
incomeGroup = ifelse(incomegp >= 50000 & agegroups < 75000, "50-74", incomeGroup)
incomeGroup = ifelse(incomegp >= 75000 & agegroups < 100000, "75-99", incomeGroup)
incomeGroup = ifelse(incomegp >= 100000 & agegroups < 150000, "100-149", incomeGroup)
incomeGroup = ifelse(incomegp >= 150000, "150+", incomeGroup)
incomeGroup = ifelse(is.na(incomegp), "Prefer not to say", incomeGroup)

round((table(incomeGroup[all.size.df$name == "Area"])/length(incomeGroup[all.size.df$name == "Area"]))*100, 1)
round((table(incomeGroup[all.size.df$name == "Box"])/length(incomeGroup[all.size.df$name == "Box"]))*100, 1)
round((table(incomeGroup[all.size.df$name == "Bar"])/length(incomeGroup[all.size.df$name == "Bar"]))*100, 1)
round((table(incomeGroup)/length(incomeGroup))*100, 1)

# Education with people 25+
age25p = which(all.size.df$age >= 25)
round((table(all.size.df$edu[age25p])/length(all.size.df$edu[age25p]))*100, 1)


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

# Base cases:
# Working full-time
# White
# High school diploma or GED
# Not latino
# Male
# Texas / South / South Atlantic

# EDU as a number
# Race assigned to region --------------------------------------------
total.region.modedu<- lm(val~ name + region+ gender + age + latino + racelarge + 
                        as.numeric(edunum) + work + politics + money + clim, 
                      data= all.size.df)
summary(total.region.modedu)
resid.region.varedu<- 1-summary(total.region.modedu)$r.squared; resid.region.varedu
length(names(total.region.modedu$coefficients))

round(summary(total.region.modedu)[["coefficients"]],3)

write.csv(summary(total.region.modedu)[["coefficients"]], "paper1/SuppTab1_DemRegion.csv")


# Demographic comparison --------------------------------------------------
dec = read.csv("data/DecennialCensusComparison.csv")

census = cbind(dec[,1:3], rep("Census", nrow(dec)))
study = cbind(dec[,c(1,2,4)], rep("This study", nrow(dec)))
colnames(census) = c("Demographic", "Variable", "Percent", "Source")
colnames(study) = c("Demographic", "Variable", "Percent", "Source")

formatted_dec = rbind(census, study)
formatted_dec$Variable = factor(formatted_dec$Variable, levels = formatted_dec$Variable[1:nrow(dec)])

po_col = brewer.pal(3, "PuOr")

# png(file="paper1/Demographics.png", family="Helvetica", res=300,
#     units="in", width=maximum_width, height=column_height*3, pointsize=10)
# 
# ggplot(formatted_dec, aes(fill=Source,x=Percent, y=Variable)) + 
#   geom_bar(position="dodge", stat="identity", colour="black", linewidth = 0.1) +
#   labs(y = "Demographic variable", x= "Percent (%) of population", fill="") + 
#   # define colors
#   scale_fill_manual(values=po_col[c(1,3)]) +
#   theme_bw() +
#   geom_text(aes(label = round(Percent)), hjust=-0.1, size = 3, position = position_dodge(0.9)) +
#   theme(legend.title = element_text(size=9), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), legend.position = c(0.55,0.2),  
#         legend.background = element_rect(fill = NA, colour = NA)) +
#   facet_wrap(~Demographic, ncol=2, scales = "free_y")
# 
# dev.off()

RdYlBu_col = brewer.pal(7, "RdYlBu")
png(file="paper1/Sfigure01.png", family="Helvetica", res=300,
    units="in", width=maximum_width, height=8)

# pdf(file="paper1/Sfigure01.pdf", family="Helvetica", width=maximum_width, 
#     height=8)

ggplot(formatted_dec, aes(x = Percent, y = Variable, fill=Demographic)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", linewidth = 0.1) +
  theme_bw() +
  scale_fill_manual(values=RdYlBu_col) +
  theme(legend.position="top", legend.text=element_text(size=9), 
        legend.box.spacing = unit(0, "pt"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.text.y = element_text(size=7)) +
  facet_grid(Demographic ~ Source, scales = "free_y", 
             labeller = labeller(Demographic = label_wrap_gen(15))) + 
  geom_text(aes(label = round(Percent), y=Variable), hjust=-0.1, size = 2.3) +
  labs(y = "", x= "Percent (%) of population", fill="")

dev.off()


