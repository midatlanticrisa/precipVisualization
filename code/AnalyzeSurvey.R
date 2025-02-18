library(ggplot2)
library(RColorBrewer)
library(scales)
library(stringr)

# responses = read.csv("/Users/klr324/Downloads/Heavy Precipitation_September 14, 2023_10.18.csv")
responses = read.csv("/Users/klr324/Downloads/Heavy Precipitation_September 18, 2023_07.40.csv")

user_resp = responses[3:nrow(responses), ]
questions = responses[1, ]

# theme_gray()

for(i in c(24:82, 101:111)){
  
  datTable = as.data.frame(table(user_resp[ ,i]))
  
  datPlot =   ggplot(datTable, aes(y = reorder(Var1, -Freq), x = Freq)) +
    geom_col() +
    scale_y_discrete(labels = label_wrap(30)) +
    scale_x_continuous(labels = comma) +
    theme_set(theme_bw()) +
    theme_update(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank()) +
    theme(axis.text=element_text(size=8),
          plot.title=element_text(size=14)) +
    labs(y = NULL, x = "Number of responses",
         title = str_wrap(questions[,i], width = 45))
  
  png(file=paste0("eval/Fig_", colnames(questions)[i], ".png"), family="Helvetica", res=300, 
      units="in", width=6, height=4, pointsize=10)
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(1.75,11,5,0.75))

  print(datPlot)
  
  dev.off()
}

for(i in 83:ncol(responses)){
  
  datTable = as.data.frame(table(user_resp[ ,i]))
  
  datPlot =   ggplot(datTable, aes(y = Var1, x = Freq)) +
    geom_col() +
    scale_y_discrete(labels = label_wrap(30)) +
    scale_x_continuous(labels = comma) +
    theme_set(theme_bw()) +
    theme_update(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank()) +
    theme(axis.text=element_text(size=8),
          plot.title=element_text(size=14)) +
    labs(y = NULL, x = "Number of responses",
         title = str_wrap(questions[,i], width = 50))
  
  png(file=paste0("eval/Fig_", colnames(questions)[i], ".png"), family="Helvetica", res=300, 
      units="in", width=6, height=4, pointsize=10)
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(1.75,11,5,0.75))
  
  print(datPlot)
  
  dev.off()
}

level_ord <- c('Extremely unlikely', 'Somewhat unlikely', 'Neither likely nor unlikely',
               'Somewhat likely', 'Extremely likely', '') 

level_ord <- c('Strongly disagree', 'Somewhat disagree', 'Neither agree nor disagree',
               'Somewhat agree', 'Strongly agree', '') 
for(i in 83:100){
  
  datTable = as.data.frame(table(user_resp[ ,i]))
  
  datPlot =   ggplot(datTable, aes(y = factor(Var1, level=level_ord), x = Freq)) +
    geom_col() +
    scale_y_discrete(labels = label_wrap(30)) +
    scale_x_continuous(labels = comma) +
    theme_set(theme_bw()) +
    theme_update(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank()) +
    theme(axis.text=element_text(size=8),
          plot.title=element_text(size=14)) +
    labs(y = NULL, x = "Number of responses",
         title = str_wrap(questions[,i], width = 50))
  
  png(file=paste0("../eval/Fig_", colnames(questions)[i], ".png"), family="Helvetica", res=300, 
      units="in", width=6, height=4, pointsize=10)
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(1.75,11,5,0.75))
  
  print(datPlot)
  
  dev.off()
}

# Climate literacy --------------------------------------------------------
redBlue = brewer.pal(4, "RdBu")

climTable = read.csv("climateTable.csv", header=TRUE)

climDat = responses[3:nrow(responses), grep("CLIM", colnames(responses))]

concerned = mat.or.vec(nrow(climDat), 2)
for(i in 1:nrow(climDat)){
  concerned[i, 1] = sum(climTable$CLIM1_CAT[match(climDat$CLIM1[i], climTable$CLIM1)],
                  climTable$CLIM2_CAT[match(climDat$CLIM2[i], climTable$CLIM2)],
                  climTable$CLIM3_CAT[match(climDat$CLIM3[i], climTable$CLIM3)],
                  climTable$CLIM4_CAT[match(climDat$CLIM4[i], climTable$CLIM4)],
                  climTable$CLIM5_CAT[match(climDat$CLIM5[i], climTable$CLIM5)],
                  climTable$CLIM6_CAT[match(climDat$CLIM6[i], climTable$CLIM6)],
                  climTable$CLIM7_CAT[match(climDat$CLIM7[i], climTable$CLIM7)],
                  climTable$CLIM8_CAT[match(climDat$CLIM8[i], climTable$CLIM8)],
                  climTable$CLIM9_CAT[match(climDat$CLIM9[i], climTable$CLIM9)],
                  climTable$CLIM10_CAT[match(climDat$CLIM10[i], climTable$CLIM10)], na.rm = TRUE)
  
  if(concerned[i, 1] > 0){ 
    concerned[i, 2] = redBlue[1]
  } else if(concerned[i, 1] < 0) {
    concerned[i, 2] = redBlue[length(redBlue)]
  } else {
    concerned[i, 2] = "black"
  }
}

png(file="eval/Fig_ClimateLiteracy.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,0.75,0.75))

barplot(as.numeric(concerned[,1]), ylab="Responder", horiz=TRUE, 
        xlab="Climate Literacy", col=concerned[,2], xlim=c(-10,10))
box()

dev.off()

# Testing correct reponses -----------------------------------------------------
intTable = read.csv("/Users/klr324/Documents/Github/precipVisualization/interpretTable.csv", header=TRUE)

empDat = responses[3:nrow(responses), grep("EMP", colnames(responses))]
expDat = responses[3:nrow(responses), grep("EXP", colnames(responses))]
accDat = responses[3:nrow(responses), grep("ACC", colnames(responses))]
kmDat = responses[3:nrow(responses), grep("KM", colnames(responses))]

totalemphasis = length(intTable$Answer[grep("EMP", intTable$ID)])
totalexpress = length(intTable$Answer[grep("EXP", intTable$ID)])
totalaccuracy = length(intTable$Answer[grep("ACC", intTable$ID)])

empaccexp = mat.or.vec(nrow(empDat), 3)
for(i in 1:nrow(empDat)){
  
  # Emphasis ----------------------------------------------------------------
  empaccexp[i, 1] = sum(as.integer(empDat[i,] == intTable$Answer[grep("EMP", intTable$ID)]))
  
  # Accuracy ----------------------------------------------------------------
  acc_ans2 = sum(as.integer(accDat[i,c(3,7)] == intTable$Answer[grep("ACC", intTable$ID)][c(3,7)] | 
                              accDat[i,c(3,7)] == intTable$Answer2[grep("ACC", intTable$ID)][c(3,7)]))
  acc_ans1 = sum(as.integer(accDat[i,c(1:2,4:6)] == intTable$Answer[grep("ACC", intTable$ID)][c(1:2,4:6)]))
  
  empaccexp[i, 2] = acc_ans1 + acc_ans2
  
  # Expression --------------------------------------------------------------
  exp_1 = sum(as.integer(expDat[i,c(1,3)] == intTable$Answer[grep("EXP", intTable$ID)][c(1,3)]))
  print(exp_1)
  
  if(expDat[i,2] == intTable$Answer[grep("EXP", intTable$ID)][2] & expDat[i,2] == intTable$Answer2[grep("EXP", intTable$ID)][2]){
    exp_2 = 1
  } else if(expDat[i,2] == intTable$Answer[grep("EXP", intTable$ID)][2] & expDat[i,2] != intTable$Answer2[grep("EXP", intTable$ID)][2]){
    exp_2 = 0.5
  } else if(expDat[i,2] != intTable$Answer[grep("EXP", intTable$ID)][2] & expDat[i,2] == intTable$Answer2[grep("EXP", intTable$ID)][2]){
    exp_2 = 0.5
  } else{
    exp_2 = 0
  }
  
  if(expDat[i,4] == intTable$Answer[grep("EXP", intTable$ID)][4] & expDat[i,4] == intTable$Answer2[grep("EXP", intTable$ID)][4]){
    exp_4 = 1
  } else if(expDat[i,4] == intTable$Answer[grep("EXP", intTable$ID)][4] & expDat[i,4] != intTable$Answer2[grep("EXP", intTable$ID)][4]){
    exp_4 = 0.5
  } else if(expDat[i,4] != intTable$Answer[grep("EXP", intTable$ID)][4] & expDat[i,4] == intTable$Answer2[grep("EXP", intTable$ID)][4]){
    exp_4 = 0.5
  } else{
    exp_4 = 0
  }
  empaccexp[i, 3] = exp_1 + exp_2 + exp_4
}
empaccexp = as.data.frame(empaccexp)
colnames(empaccexp) = c("Emphasis", "Accuracy", "Expression")
empaccexp$Total = rowSums(empaccexp)

stats = data.frame(mean = colMeans(empaccexp), 
                   max = apply(empaccexp, MARGIN=c(2), max),
                   min = apply(empaccexp, MARGIN=c(2), min),
                   num = c(totalemphasis, totalaccuracy, totalexpress, 
                           (totalexpress+totalaccuracy+totalemphasis)))
  
png(file="eval/Fig_Emphasis.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

barplot(empaccexp$Emphasis, horiz=TRUE, xlim=c(0,totalemphasis), 
        main="Emphasis", xlab="Correct responses", ylab="Responders",
        font.main = 1, yaxs="i")
box()

legend("bottomright",legend=c(paste("Mean:", round(stats["Emphasis", ]$mean,1)),
                           paste("Min:", stats["Emphasis", ]$min),
                           paste("Max:", stats["Emphasis", ]$max),
                           paste("No. of Ques.:", stats["Emphasis", ]$num)), 
       bty="n", cex=0.8)

dev.off()

png(file="eval/Fig_Accuracy.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

barplot(empaccexp$Accuracy, horiz=TRUE, xlim=c(0,totalaccuracy), 
        font.main = 1,main="Accuracy", xlab="Correct responses", ylab="Responders",
        yaxs="i")
box()

legend("bottomright",legend=c(paste("Mean:", round(stats["Accuracy", ]$mean,1)),
                              paste("Min:", stats["Accuracy", ]$min),
                              paste("Max:", stats["Accuracy", ]$max),
                              paste("No. of Ques.:", stats["Accuracy", ]$num)), 
       bty="n", cex=0.8)

dev.off()

png(file="eval/Fig_Expression.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

barplot(empaccexp$Expression, horiz=TRUE, xlim=c(0,totalexpress), 
        font.main = 1,main="Expression", xlab="Correct responses", ylab="Responders",
        yaxs="i")
box()

legend("bottomright",legend=c(paste("Mean:", round(stats["Expression", ]$mean,1)),
                              paste("Min:", stats["Expression", ]$min),
                              paste("Max:", stats["Expression", ]$max),
                              paste("No. of Ques.:", stats["Expression", ]$num)), 
       bty="n", cex=0.8)
dev.off()


png(file="eval/Fig_Total.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

barplot(empaccexp$Total, horiz=TRUE, xlim=c(0,(totalexpress+totalaccuracy+totalemphasis)), 
        main="Total", xlab="Correct responses", ylab="Responders",
        font.main = 1,yaxs="i")
box()

legend("bottomright",legend=c(paste("Mean:", round(stats["Total", ]$mean,1)),
                              paste("Min:", stats["Total", ]$min),
                              paste("Max:", stats["Total", ]$max),
                              paste("No. of Ques.:", stats["Total", ]$num)), 
       bty="n", cex=0.8)
dev.off()


# The System Usability Scale (SUS)
# The SUS is a 10 item questionnaire with 5 response options.
# Strongly Disagree 1 to Strongly Agree 5
# https://measuringu.com/sus/
#
# Scoring SUS
# For odd items (Positive statements): subtract one from the user response.
# For even-numbered items (Negative statements): subtract the user responses from 5
# This scales all values from 0 to 4 (with four being the most positive response).
# Add up the converted responses for each user and multiply that total by 2.5. 
# This converts the range of possible values from 0 to 100 instead of from 0 to 40.
# 
# What is a Good SUS Score?
# The average SUS score from all 500 studies is a 68. A SUS score above a 68 
# would be considered above average and anything below 68 is below average.

sus_scale = c("Strongly disagree", "Somewhat disagree", 
                        "Neither agree nor disagree", "Somewhat agree", "Strongly agree")

positive_sus = function(var, sus_scale){
  # Returns vector position 1:5 (Strongly disagree to strong agree)
  resp = match(var, sus_scale)
  
  # Subtract one from the user response. This scales all values from 0 to 4 (with 
  # four being the most positive response).
  scale_resp = resp - 1
  
  return(scale_resp)
}

negative_sus = function(var, sus_scale){
  # Returns vector position 1:5 (Strongly disagree to strong agree)
  resp = match(var, sus_scale)
  
  # Ssubtract the user responses from 5. This scales all values from 0 to 4 (with 
  # four being the most positive response).
  scale_resp = 5 - resp
  
  return(scale_resp)
}


# Other
questions$USE3

# Positive
sus_scores = data.frame(USE2 = positive_sus(user_resp$USE2, sus_scale),
                        USE4 = positive_sus(user_resp$USE4, sus_scale),
                        USE6 = positive_sus(user_resp$USE6, sus_scale),
                        USE10 = positive_sus(user_resp$USE10, sus_scale),
                        USE12 = positive_sus(user_resp$USE12, sus_scale),
                        # Negative
                        USE5 = negative_sus(user_resp$USE5, sus_scale),
                        USE7 = negative_sus(user_resp$USE7, sus_scale),
                        USE11 = negative_sus(user_resp$USE11, sus_scale),
                        USE13 = negative_sus(user_resp$USE13, sus_scale),
                        Q112 = negative_sus(user_resp$Q112, sus_scale))
# Add up the converted responses for each user and multiply that total by 2.5. 
# This converts the range of possible values from 0 to 100 instead of from 0 to 40.
sus_scores$TOT = rowSums(sus_scores, na.rm = TRUE) * 2.5

mean(sus_scores$TOT)

summary(sus_scores$TOT)

plot(sus_scores$TOT, 1:length(sus_scores$TOT), pch=20, xlim=c(0,100))

# cumulative density function for SLR values in 2050
plot(cdf2050_heter, lwd=3, ylab="Cumulative density", col=test.colors[3],
     xlab="Projected sea-level 2050 [m]", main="", xlim=c(-0.1,0.8))
lines(cdf2050_boot, lwd=3, col=test.colors[1])
lines(cdf2050_homo, lwd=3, col=test.colors[2])
put.fig.letter("c.",font=2)

cdftot = ecdf(sus_scores$TOT)
cdftot(sus_scores$TOT)
plot(cdftot, main="", ylab="Cumulative density",
     xlab="System Usability Scale", xlim=c(0,100))

prob = cdftot(sus_scores$TOT)
int = order(prob, decreasing=FALSE)
plot(sus_scores$TOT[int], prob[int], type="l", main="", ylab="Cumulative density",
     xlab="System Usability Scale", xlim=c(0,100))
points(sus_scores$TOT[int], prob[int], pch=20)

susTab = read.csv("susTable.csv", skip=2, header=TRUE)
rdYlGn = brewer.pal(6, "RdYlGn")

png(file="eval/Fig_SUS.png", family="Helvetica", res=300, 
    units="in", width=6, height=4, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(1.5,0.75,0,0.75))
plot(sus_scores$TOT, (1:length(sus_scores$TOT))/1.5, pch=20, xlim=c(-15,100), 
     ylab="", xlab="", xaxs="i", axes=F, type="n")
axis(1, at = seq(from = 0, to = 100, by = 10), cex.axis = 1, lwd.tick = 2, 
     col = "gray44", font = 2)

# Grades
rect(susTab$SUS_low[3], 5, susTab$SUS_high[1], 10, col="gray50", border=NA) # A
rect(susTab$SUS_low[6], 5, susTab$SUS_high[4], 10, col="gray75", border=NA) # B
rect(susTab$SUS_low[9], 5, susTab$SUS_high[7], 10, col="gray50", border=NA) # C
rect(susTab$SUS_low[10], 5, susTab$SUS_high[10], 10, col="gray75", border=NA) # D
rect(susTab$SUS_low[12], 5, susTab$SUS_high[11], 10, col="gray50", border=NA) # F
text(-15, 7.5, "Grade:", adj=c(0,0.5))
text(mean(c(susTab$SUS_low[3], susTab$SUS_high[1])), 7.5, "A", adj=c(0.5,0.5), cex=0.8)
text(mean(c(susTab$SUS_low[6], susTab$SUS_high[4])), 7.5, "B", adj=c(0.5,0.5), cex=0.8)
text(mean(c(susTab$SUS_low[9], susTab$SUS_high[7])), 7.5, "C", adj=c(0.5,0.5), cex=0.8)
text(mean(c(susTab$SUS_low[10], susTab$SUS_high[10])), 7.5, "D", adj=c(0.5,0.5), cex=0.8)
text(mean(c(susTab$SUS_low[12], susTab$SUS_high[11])), 7.5, "F", adj=c(0.5,0.5), cex=0.8)

# Adjective
rect(susTab$SUS_low[1], 15, susTab$SUS_high[1], 20, col=rdYlGn[6], border=NA) # Best imaginable
rect(susTab$SUS_low[6], 15, susTab$SUS_high[2], 20, col=rdYlGn[5], border=NA) # Excellent
rect(susTab$SUS_low[9], 15, susTab$SUS_high[7], 20, col=rdYlGn[4], border=NA) # Good
rect(susTab$SUS_low[10], 15, susTab$SUS_high[10], 20, col=rdYlGn[3], border=NA) # Ok
rect(susTab$SUS_low[11], 15, susTab$SUS_high[11], 20, col=rdYlGn[2], border=NA) # Poor
rect(susTab$SUS_low[12], 15, susTab$SUS_high[12], 20, col=rdYlGn[1], border=NA) # Worst imaginable
text(-15, 17.5, "Adjective:", adj=c(0,0.5))
text(mean(c(susTab$SUS_low[1], susTab$SUS_high[1])), 17.5, "Best imaginable", adj=c(0.5,0.5), cex=0.8)
text(mean(c(susTab$SUS_low[6], susTab$SUS_high[2])), 17.5, "Excellent", adj=c(0.5,0.5), cex=0.8)
text(mean(c(susTab$SUS_low[9], susTab$SUS_high[7])), 17.5, "Good", adj=c(0.5,0.5), cex=0.8)
text(mean(c(susTab$SUS_low[10], susTab$SUS_high[10])), 17.5, "Ok", adj=c(0.5,0.5), cex=0.8)
text(mean(c(susTab$SUS_low[11], susTab$SUS_high[11])), 17.5, "Poor", adj=c(0.5,0.5), cex=0.8)
text(mean(c(susTab$SUS_low[12], susTab$SUS_high[12])), 17.5, "Worst imaginable", adj=c(0.5,0.5), cex=0.8)

# Acceptable
rect(susTab$SUS_low[7], 25, susTab$SUS_high[1], 30, col=rdYlGn[6], border=NA) # Acceptable
rect(susTab$SUS_low[10], 25, susTab$SUS_high[8], 30, col=rdYlGn[3], border=NA) # Marginal
rect(susTab$SUS_low[12], 25, susTab$SUS_high[11], 30, col=rdYlGn[1], border=NA) # Not acceptable
text(-15, 27.5, "Acceptable:", adj=c(0,0.5))
text(mean(c(susTab$SUS_low[7], susTab$SUS_high[1])), 27.5, "Acceptable", adj=c(0.5,0.5), cex=0.8)
text(mean(c(susTab$SUS_low[10], susTab$SUS_high[8])), 27.5, "Marginal", adj=c(0.5,0.5), cex=0.8)
text(mean(c(susTab$SUS_low[12], susTab$SUS_high[11])), 27.5, "Not acceptable", adj=c(0.5,0.5), cex=0.8)

# SUS scores
points(sus_scores$TOT, (1:length(sus_scores$TOT))/1.5, pch=20)
text(-15, 0, "SUS Score:", adj=c(0,0.5))
abline(v=mean(sus_scores$TOT), lwd=2)
# mtext("SUS Score:", side = 2, line = 0, las=1, at=0)
dev.off()

indScore = colMeans(sus_scores[,1:10], na.rm = TRUE)*25

png(file="eval/Fig_SUS_IND.png", family="Helvetica", res=300, 
    units="in", width=6, height=4, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,0.75,0,0.75))
plot(indScore, 1:10, pch=20, xlab="SUS Score", 
     ylab="", axes=F, type="n")
axis(1, at = seq(from = 0, to = 100, by = 10), cex.axis = 1, lwd.tick = 2, 
     col = "gray44", font = 2)
text(indScore, 1:10, labels=colnames(sus_scores[,1:10]))
abline(v=68, lty=2, lwd=2)
dev.off()

mot_scale = c("Extremely unlikely", "Somewhat unlikely", 
              "Neither likely nor unlikely", "Somewhat likely", "Extremely likely")

motTab = user_resp[grep("MOT", colnames(user_resp))]
match(motTab[,1], mot_scale)

scale_mot = as.data.frame(apply(motTab, 2, match, table = mot_scale)-1)
scale_mot$tot = rowSums(scale_mot, na.rm=TRUE)*5

png(file="eval/Fig_MOT.png", family="Helvetica", res=300, 
    units="in", width=6, height=4, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,0.75,0,0.75))
plot(scale_mot$tot, (1:length(scale_mot$tot))/1.5, pch=20, xlim=c(0,100), 
     ylab="", xlab="Based on SUS score algorithm", axes=F, type="n")

# Grades
rect(0, par("usr")[1], 50, par("usr")[2], col=rdYlGn[1], border=NA) # A
rect(50, par("usr")[1], 75, par("usr")[2], col=rdYlGn[3], border=NA) # B
rect(75, par("usr")[1], 100, par("usr")[2], col=rdYlGn[6], border=NA) # C
text(mean(c(0, 50)), 7.5, "Not motivated", adj=c(0.5,0.5), cex=0.8)
text(mean(c(51, 75)), 7.5, "Marginal", adj=c(0.5,0.5), cex=0.8)
text(mean(c(76, 100)), 7.5, "Motivated", adj=c(0.5,0.5), cex=0.8)

# SUS scores
axis(1, at = seq(from = 0, to = 100, by = 10), cex.axis = 1, lwd.tick = 2, 
     col = "gray44", font = 2)
points(scale_mot$tot, (1:length(scale_mot$tot))/1.5, pch=20)
# text(-15, 0, "SUS Score:", adj=c(0,0.5))
# abline(v=mean(sus_scores$TOT), lwd=2)
# mtext("SUS Score:", side = 2, line = 0, las=1, at=0)
dev.off()


# Timing ------------------------------------------------------------------

timing = data.frame(exp1 = as.numeric(user_resp$X.timing1_Page.Submit), 
                    emp1 = as.numeric(user_resp$timing2_Page.Submit),
                    acc1 = as.numeric(user_resp$timing3_Page.Submit),
                    chal1 = as.numeric(user_resp$timing4_Page.Submit),
                    exp2 = as.numeric(user_resp$timing5_Page.Submit),
                    emp2 = as.numeric(user_resp$timing6_Page.Submit),
                    acc2 = as.numeric(user_resp$timing7_Page.Submit),
                    chal2 = as.numeric(user_resp$timing8_Page.Submit),
                    km2 = as.numeric(user_resp$timing9_Page.Submit))
timing$tot = rowSums(timing)
timing$totacc = timing$acc1 + timing$acc2

png(file="eval/Fig_Time.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(empaccexp$Total, timing$tot/60, pch=20, xlab="Total correct responses", 
     ylab="Total response time (minutes)", xlim=c(0,(totalexpress+totalaccuracy+totalemphasis)))

dev.off()

png(file="eval/Fig_AccTime.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(empaccexp$Accuracy, timing$totacc/60, pch=20, xlab="Accuracy correct responses", 
     ylab="Accuracy response time (minutes)", xlim=c(0,totalaccuracy))

dev.off()

# More than 50% of accuracy questions correct (above 3.5 vs below 3.5)
acclow = timing$totacc[which(empaccexp$Accuracy <= 3)]/60
acchigh = timing$totacc[which(empaccexp$Accuracy > 3)]/60

# More than 50% of total questions correct (above 8.5 vs below 8.5)
totlow = timing$tot[which(empaccexp$Total <= 8)]/60
tothigh = timing$tot[which(empaccexp$Total > 8)]/60

timeTab = matrix(c(summary(totlow), summary(tothigh), summary(acclow), summary(acchigh)), 
       byrow = TRUE, ncol = 6)
timeTab = as.data.frame(round(timeTab, 1))
colnames(timeTab) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
rownames(timeTab) = c("Less than 50% total correct", "More than 50% total correct",
                      "Less than 50% accuracy correct", "More than 50% accuracy correct")
timeTab$No = c(length(totlow), length(tothigh), length(acclow), length(acchigh))
colnames(timeTab)[7] = "No. of responses"

# Decision Scenarios ------------------------------------------------------

chalop1 = c("Do nothing (protect against 0 heavy rainfall events per year)", 
            "Install erosion control measures (protect against 3 heavy rainfall events per year)",
            "Pave the driveway (protect against 6 heavy rainfall events per year)")

chalop2 = c("No, I would not purchase a flood insurance policy",
            "Yes, I would purchase an annual flood insurance policy")


riskAd = data.frame(CHAL1 = (match(user_resp$CHAL1.1, chalop1)-1)/2,
                    CHAL2 = match(user_resp$CHAL2.1, chalop2)-1)
riskAd$TOT = rowSums(riskAd)

png(file="eval/Fig_RISK.png", family="Helvetica", res=300,
    units="in", width=6, height=4, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,0.75,0,0.75))
plot(riskAd$TOT, (1:length(riskAd$TOT))/1.5, pch=20, xlim=c(0,2), 
     ylab="", xlab="Risk adverse", axes=F, type="n")

# Grades
rect(0, par("usr")[1], 0.5, par("usr")[4], col=rdYlGn[1], border=NA) # A
rect(0.5, par("usr")[1], 1.5, par("usr")[4], col=rdYlGn[3], border=NA) # B
rect(1.5, par("usr")[1], 2, par("usr")[4], col=rdYlGn[6], border=NA) # C
text(mean(c(0, 0.5)), 7.5, "Less risk adverse", adj=c(0.5,0.5), cex=0.8)
text(mean(c(0.5, 1.5)), 7.5, "Marginal", adj=c(0.5,0.5), cex=0.8)
text(mean(c(1.5, 2)), 7.5, "More risk adverse", adj=c(0.5,0.5), cex=0.8)

# SUS scores
axis(1, at = seq(from = 0, to = 2, by = 0.5), cex.axis = 1, lwd.tick = 2, 
     col = "gray44", font = 2)
points(riskAd$TOT, (1:length(riskAd$TOT))/1.5, pch=20)
dev.off()

png(file="eval/Fig_RISKTot.png", family="Helvetica", res=300,
    units="in", width=6, height=4, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,0,0.75))

plot(riskAd$TOT, empaccexp$Total, pch=20, ylab="Total correct responses", 
     xlab="Risk adversion", ylim=c(0,(totalexpress+totalaccuracy+totalemphasis)),
     xlim=c(0,2))
dev.off()

riskTotlow = riskAd$TOT[which(empaccexp$Total <= 8)]
riskTothigh = riskAd$TOT[which(empaccexp$Total > 8)]

riskTab = matrix(c(summary(riskTotlow), summary(riskTothigh)), byrow = TRUE, ncol = 6)
riskTab = as.data.frame(round(riskTab, 1))
colnames(riskTab) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
rownames(riskTab) = c("Less than 50% total correct", "More than 50% total correct")
riskTab$No = c(length(riskTotlow), length(riskTothigh))
colnames(riskTab)[7] = "No. of responses"

png(file="eval/Fig_MOTClim.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(empaccexp$Total, scale_mot$MOT1, pch=20, ylab="Seek climate resources", 
     xlab="Total correct responses", ylim=c(0,5), 
     xlim=c(0,(totalexpress+totalaccuracy+totalemphasis)))
dev.off()

png(file="eval/Fig_CMOTSci.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(empaccexp$Total, scale_mot$MOT2, pch=20, ylab="Seek science resources", 
     xlab="Total correct responses", ylim=c(0,5), 
     xlim=c(0,(totalexpress+totalaccuracy+totalemphasis)))
dev.off()

png(file="eval/Fig_MOTStat.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(empaccexp$Total, scale_mot$MOT3, pch=20, ylab="Seek statistics resources", 
     xlab="Total correct responses", ylim=c(0,5), 
     xlim=c(0,(totalexpress+totalaccuracy+totalemphasis)))
dev.off()


png(file="eval/Fig_MOTFlood.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(riskAd$TOT, scale_mot$MOT4, pch=20, ylab="Seek flooding resources", 
     xlab="Risk adversion", ylim=c(0,5), xlim=c(0,2))
dev.off()


png(file="eval/Fig_RiskClim.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(riskAd$TOT, as.numeric(concerned[,1]), pch=20, ylab="Climate literacy", 
     xlab="Risk adversion", ylim=c(-10,10), xlim=c(0,2), col=concerned[,2])

dev.off()

png(file="eval/Fig_ClimTot.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(empaccexp$Total, as.numeric(concerned[,1]), pch=20, xlab="Total correct responses", 
     ylab="Climate literacy", xlim=c(0,(totalexpress+totalaccuracy+totalemphasis)),
     ylim=c(-10,10), col=concerned[,2])

dev.off()

png(file="eval/Fig_SUSTot.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(sus_scores$TOT, empaccexp$Total, pch=20, ylab="Total correct responses", 
     xlab="SUS score", ylim=c(0,(totalexpress+totalaccuracy+totalemphasis)),
     xlim=c(0,100))

dev.off()

# tt = data.frame(risk = riskAd$TOT, gen = user_resp$DEM2)
# table(tt)

# Double bar plot to make
des = c("Female", "Male", "Non-binary / third gender")
plot(riskAd$TOT, match(user_resp$DEM2, des), pch=20, ylab="", 
     xlab="Risk adversion", xlim=c(0,2), yaxt="n")
axis(2, at=1:3, labels=des)  

# Age
tt = data.frame(score = empaccexp$Total, gen = user_resp$DEM2)

Age = data.frame(Female = 100*(length(which((tt[tt$gen == "Female", ]$score > 8) == TRUE))/
                           nrow(tt[tt$gen == "Female", ])),
           Male = 100*(length(which((tt[tt$gen == "Male", ]$score > 8) == TRUE))/
                         nrow(tt[tt$gen == "Male", ])),
           Other = 100*(length(which((tt[tt$gen == "Non-binary / third gender", ]$score > 8) == TRUE))/
                          nrow(tt[tt$gen == "Non-binary / third gender", ])))

barplot(as.matrix(Age))



# plot(as.numeric(rownames(tabtt)), tabtt[,"Female"], pch=20, xlab="Score", col="red")
# points(as.numeric(rownames(tabtt)), tabtt[,"Male"], pch=20, col="blue")

# Participants who select more risk-averse decisions in the decision scenarios, 
# have higher climate literacy scores and higher interpretation scores and are more 
# likely to be female, young, wealthy, educated, and politically left.

# Income

# Education

# politics
tt = data.frame(risk = riskAd$TOT, gen = user_resp$DEM7)
table(tt)
      

jargon = mat.or.vec(nrow(empDat), 1)
for(i in 1:nrow(empDat)){
  
  # Emphasis ----------------------------------------------------------------
  jargon[i] = sum(as.integer(empDat[i,c(1,3,5,6)] == intTable$Answer[grep("EMP", intTable$ID)][c(1,3,5,6)]))
}
      
png(file="eval/Fig_EmphasisJargon.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

barplot(jargon, horiz=TRUE, xlim=c(0,4), 
        main="Emphasis: specialized terminology and elements", xlab="Correct responses", ylab="Responders",
        font.main = 1, yaxs="i")
box()

legend("bottomright",legend=c(paste("Mean:", round(mean(jargon),1)),
                              paste("Min:", min(jargon)),
                              paste("Max:", max(jargon)),
                              paste("No. of Ques.:", 4)), 
       bty="n", cex=0.8)
dev.off()      

# Participants in Group 3 (area chart) will misinterpret average trends and comparisons between scenario
# averages more than participants in Groups 1&2. However, participants will score more accurately on
# questions about ranges than participants in Groups 1 & 2.

acc_ans = mat.or.vec(nrow(empDat), 3)
for(i in 1:nrow(empDat)){
  
 # Accuracy ----------------------------------------------------------------
  acc_ans[i,1] = sum(as.integer(accDat[i,c(3,7)] == intTable$Answer[grep("ACC", intTable$ID)][c(3,7)] | 
                              accDat[i,c(3,7)] == intTable$Answer2[grep("ACC", intTable$ID)][c(3,7)]))
  acc_ans[i,2] = sum(as.integer(accDat[i,c(1,5)] == intTable$Answer[grep("ACC", intTable$ID)][c(1,5)]))
  
  acc_ans[i,3] = sum(as.integer(accDat[i,c(2,6)] == intTable$Answer[grep("ACC", intTable$ID)][c(2,6)]))
}
acc_ans = data.frame(acc_ans)
colnames(acc_ans) = c("comp", "ave", "range")

acc_ans$trend = acc_ans$comp + acc_ans$ave

acc_freq = mat.or.vec(nrow(empDat), 3)
acc_int = mat.or.vec(nrow(empDat), 3)
for(i in 1:nrow(empDat)){
  
  # Accuracy ----------------------------------------------------------------
  acc_freq[i,1] = as.integer(accDat[i,3] == intTable$Answer[grep("ACC", intTable$ID)][3] | 
                                  accDat[i,3] == intTable$Answer2[grep("ACC", intTable$ID)][3])
  acc_freq[i,2] = as.integer(accDat[i,1] == intTable$Answer[grep("ACC", intTable$ID)][1])
  
  acc_freq[i,3] = as.integer(accDat[i,2] == intTable$Answer[grep("ACC", intTable$ID)][2])
  
  acc_int[i,1] = as.integer(accDat[i,7] == intTable$Answer[grep("ACC", intTable$ID)][7] | 
                               accDat[i,7] == intTable$Answer2[grep("ACC", intTable$ID)][7])
  acc_int[i,2] = as.integer(accDat[i,5] == intTable$Answer[grep("ACC", intTable$ID)][5])
  
  acc_int[i,3] = as.integer(accDat[i,6] == intTable$Answer[grep("ACC", intTable$ID)][6])
}
acc_freq = data.frame(acc_freq)
acc_int = data.frame(acc_int)
colnames(acc_freq) = c("comp", "ave", "range")
colnames(acc_int) = c("comp", "ave", "range")
acc_freq$tot = rowSums(acc_freq)
acc_int$tot = rowSums(acc_int)

plot(acc_freq$tot, col="red", pch=20)
points(acc_int$tot, col="blue")

plot(acc_freq$tot, acc_int$tot)

hist(acc_freq$tot - acc_int$tot)
hist(acc_int$tot - acc_freq$tot)

png(file="eval/Fig_AccTrend.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

barplot(acc_ans$trend, horiz=TRUE, xlim=c(0,4), 
        main="Accuracy: Ave. trend and comparison", xlab="Correct responses", ylab="Responders",
        font.main = 1, yaxs="i")
box()

legend("bottomright",legend=c(paste("Mean:", round(mean(acc_ans$trend),1)),
                              paste("Min:", min(acc_ans$trend)),
                              paste("Max:", max(acc_ans$trend)),
                              paste("No. of Ques.:", 4)), 
       bty="n", cex=0.8)
dev.off()   

png(file="eval/Fig_AccRange.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

barplot(acc_ans$range, horiz=TRUE, xlim=c(0,2), 
        main="Accuracy: Range", xlab="Correct responses", ylab="Responders",
        font.main = 1, yaxs="i")
box()

legend("bottomright",legend=c(paste("Mean:", round(mean(acc_ans$range),1)),
                              paste("Min:", min(acc_ans$range)),
                              paste("Max:", max(acc_ans$range)),
                              paste("No. of Ques.:", 2)), 
       bty="n", cex=0.8)
dev.off()   

acc_chnge = mat.or.vec(nrow(empDat), 2)
for(i in 1:nrow(empDat)){
  
  # Accuracy ----------------------------------------------------------------
  acc_ans1 = as.integer(accDat[i,7] == intTable$Answer[grep("ACC", intTable$ID)][7] | 
                                  accDat[i,7] == intTable$Answer2[grep("ACC", intTable$ID)][7])
  
  acc_ans2 = sum(as.integer(accDat[i,c(5:6)] == intTable$Answer[grep("ACC", intTable$ID)][c(5:6)]))
  acc_chnge[i,1] = acc_ans1 + acc_ans2
  
  km_ans1 = as.integer(kmDat[i,2] == intTable$Answer[grep("KM", intTable$ID)][2] | 
                          kmDat[i,2] == intTable$Answer2[grep("KM", intTable$ID)][2])
  
  km_ans2 = sum(as.integer(kmDat[i,c(1,3)] == intTable$Answer[grep("KM", intTable$ID)][c(1,3)]))
  acc_chnge[i,2] = km_ans1 + km_ans2
  
}
acc_chnge = data.frame(acc_chnge)
colnames(acc_chnge) = c("org", "change")

barplot(t(as.matrix(acc_chnge)), beside = T, col=c("gray", "black"))

kmdif = acc_chnge$change - acc_chnge$org

png(file="eval/Fig_KM.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

barplot(kmdif, horiz=TRUE, xlim=c(-3,3), 
        main="Accuracy: Alt format", xlab="Correct responses", ylab="Responders",
        font.main = 1, yaxs="i")
box()

legend("bottomright",legend=c(paste("Mean:", round(mean(kmdif),1)),
                              paste("Min:", min(kmdif)),
                              paste("Max:", max(kmdif)),
                              paste("No. of Ques.:", 3)), 
       bty="n", cex=0.8)
dev.off()

length(kmdif[kmdif > 0])
length(kmdif[kmdif < 0])


# ---------------
# Is there a relationship between time and accuracy
# Can time predict accuracy?
time = timing$tot/60
linearMod = lm(empaccexp$Total ~ time)
summary(linearMod)
adRsq = 0.1385
f2 = sqrt(adRsq)
pwr.f2.test(u = 1, f2=f2, sig.level = 0.05, power =0.9)
# 29+2 = 31
# ---------------
acc_chnge

mh0 = mean(acc_chnge$org)
mh1 = mean(acc_chnge$change)
sd0 = sd(acc_chnge$org)
sd1 = sd(acc_chnge$change)
sdpooled = sqrt((sd0^2 + sd1^2)/2)
e.size_one = (mh1 - mh0)/sd1
e.size_two = (mh1 - mh0)/sdpooled

hist(acc_chnge$org)
hist(acc_chnge$change)

powers = seq(0.7, 0.95, 0.01)
os = 
  ts = 
  ps = rep(NA, length(powers))
for(i in 1:length(powers)){
  os[i] = pwr.t.test(d=e.size_one, sig.level=0.05, power=powers[i], type="one.sample",
             alternative="two.sided")$n
  
  ts[i] = pwr.t.test(d=e.size_two, sig.level=0.05, power=powers[i], type="two.sample",
             alternative="two.sided")$n
  
  ps[i] = pwr.t.test(d=e.size_two, sig.level=0.05, power=powers[i], type="paired", #(10)
             alternative="two.sided")$n
}
wil = os*1.15
mann = ts*1.15
pwill = ps*1.15

plot(powers, os, type="l", ylim = c(min(os, ts, ps), max(os, ts, ps)))
lines(powers, ts, col="red")
lines(powers, ps, col="blue")
#-----------
mh0 = mean(acc_freq$tot)
mh1 = mean(acc_int$tot)
sd0 = sd(acc_freq$tot)
sd1 = sd(acc_int$tot)
sdpooled = sqrt((sd0^2 + sd1^2)/2)
e.size_one = (mh1 - mh0)/sd1
e.size_two = (mh1 - mh0)/sdpooled

powers = seq(0.7, 0.95, 0.01)
os = 
  ts = 
  ps = rep(NA, length(powers))
for(i in 1:length(powers)){
  os[i] = pwr.t.test(d=e.size_one, sig.level=0.05, power=powers[i], type="one.sample",
                     alternative="two.sided")$n
  
  ts[i] = pwr.t.test(d=e.size_two, sig.level=0.05, power=powers[i], type="two.sample",
                     alternative="two.sided")$n
  
  ps[i] = pwr.t.test(d=e.size_two, sig.level=0.05, power=powers[i], type="paired", #(10)
                     alternative="two.sided")$n
}
wil = os*1.15
mann = ts*1.15
pwill = ps*1.15

plot(powers, os, type="l", ylim = c(min(os, ts, ps), max(os, ts, ps)))
lines(powers, ts, col="red")
lines(powers, ps, col="blue")

plot(acc_freq$tot - acc_int$tot, type="l")
t.test(acc_freq$tot, acc_int$tot)

my_data <- data.frame( 
  group = rep(c("Freq", "Int"), each = length(acc_freq$tot)),
  var = c(acc_freq$tot,  acc_int$tot)
)

library("ggpubr")
ggboxplot(my_data, x = "group", y = "var", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Variable", xlab = "Groups")

sd(c(126,
  88,
  53.1,
  98.5,
  88.3,
  82.5,
  105,
  41.9))

mh0 = mean(acc_chnge$org)
mh1 = mean(acc_chnge$change)
sd0 = sd(acc_chnge$org)
sd1 = sd(acc_chnge$change)
mdiff = mean(acc_chnge$change - acc_chnge$org)
sdiff=sd(acc_chnge$change - acc_chnge$org)
sd.diff = mdiff/sdiff

sdpooled = sqrt((sd0^2 + sd1^2)/2)
e.size = (mh1 - mh0)/sdpooled

e.size2 = (mh1 - mh0)/sd.diff


pwr.t.test(d=e.size, sig.level=0.05, power=0.70, type="paired", #(128)
           alternative="less")
pwr.t.test(d=e.size, sig.level=0.05, power=0.80, type="paired", #(168)
           alternative="less")
pwr.t.test(d=e.size, sig.level=0.05, power=0.90, type="paired", #(232)
           alternative="less")

pwr.t.test(d=e.size2, sig.level=0.05, power=0.70, type="paired", #(10)
           alternative="two.sided")
pwr.t.test(d=e.size2, sig.level=0.05, power=0.80, type="paired", #(11)
           alternative="two.sided")
pwr.t.test(d=e.size2, sig.level=0.05, power=0.90, type="paired", #(14)
           alternative="two.sided")

no.graphs = 3
e.size

pwr.anova.test(k = 3, f = 0.1, sig.level = 0.05, power = 0.9)

o1 = c(6.3, 2.8, 7.8, 7.9, 4.9)
o2 = c(9.9,4.1,3.9,6.3,6.9)
o3 = c(5.1,2.9,3.6,5.7,4.5)
o4 = c(1,2.8,4.8,3.9,1.6)

mean(c(sum(o1), sum(o2), sum(o3), sum(o4)))

test = c(o1, o2, o3, o4)

sum( (test - mean(test) )^2 )

Is there a difference in accuracy across 3 different graph types?
  
# https://statsandr.com/blog/anova-in-r/
# 1. Is there a difference in accuracy across 3 graph types?
  # a. Test for equality of the variances with a Levene's. Visually test for normal 
  #    distribution and homogeneity
  # b. Calculate mean and standard deviation
  # c. Perform one-way ANOVA to answer the question
  # d. Perform post-hoc tests, tukey HSD and Dunnett to determine which  groups 
  #    are different
  
# Does accuracy depend on time spent and/or graph type?
# a. measuring and testing the relationship between time spent and accuracy,
# b. measuring and testing the relationship between graph type and accuracy, and
# c. potentially check whether the relationship between time spent and accuracy 
#    is different for graph 1, graph 2, and graph 3 (which is equivalent to 
#    checking whether the relationship between graph type and accuracy depends 
#    on the time spent)

Highest score: Group 2
Exp: 2 best, 1 and 3 equal
Emp: 1 and 2 equal, 3 worst
acc: 2 best, 3, 1 worst

g1 = sum(c(0, 0, -1))
g2 = sum(c(1, 0, 1))
g3 = sum(c(0, -1, 0))

(totalexpress+totalaccuracy+totalemphasis)
mean(empaccexp$Total/17)*100
sd(empaccexp$Total/17)*100

(mean(empaccexp$Total/17)*100)*2

6.255102*2
sd(empaccexp$Total)

# Set plotting dimensions
mm_TO_inches = function(mm){
  mm * 0.039370
}

single_column = mm_TO_inches(84)
med_column = mm_TO_inches(129)
double_column = mm_TO_inches(174)
maximum_width = mm_TO_inches(234)
column_height=2.7

png(file="../pilotHist.png", family="Helvetica", units="in", res=300,
           width=double_column, height=column_height*2, pointsize=11)
par(mfrow=c(2,2), mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
hist(empaccexp$Emphasis, xlab="Emphasis")

hist(empaccexp$Accuracy, xlab="Accuracy")

hist(empaccexp$Expression, xlab="Expression")

hist(empaccexp$Total, xlab="Total")
dev.off()


# Correlation -------------------------------------------------------------

# Is there a correlation between accuracy and time spent?
cat = cor(empaccexp$Total, timing$tot/60)
nat = pwr.r.test(r=cat, sig.level =0.05, power=0.95)$n

png(file="../eval/stat_accuracyTime.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(empaccexp$Total, timing$tot/60, xlab="Total Accuracy", ylab="Total time", 
     pch=20)
abline(lm(timing$tot/60 ~ empaccexp$Total))
legend("topleft",legend=c(paste("r:", round(cat,2)),
                              paste("N:", round(nat,2))), 
       bty="n", cex=0.8)
dev.off()

# Is there a correlation between accuracy and climate literacy?
ccl = cor(empaccexp$Total, as.numeric(concerned[, 1]))
ncl = pwr.r.test(r=ccl, sig.level =0.05, power=0.95)$n
# plot(empaccexp$Total, as.numeric(concerned[, 1]))

png(file="../eval/stat_accuracyClimate.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(empaccexp$Total, as.numeric(concerned[, 1]), xlab="Total Accuracy", 
     ylab="Climate literacy", pch=20)
abline(lm(as.numeric(concerned[, 1]) ~ empaccexp$Total))
legend("topleft",legend=c(paste("r:", round(ccl,2)),
                          paste("N:", round(ncl,2))), 
       bty="n", cex=0.8)
dev.off()

# Is there a correlation between decision scenario options and climate literacy?
cdcl = cor(as.numeric(concerned[, 1]), riskAd$TOT)
ndcl = pwr.r.test(r=cdcl, sig.level =0.05, power=0.95)$n

png(file="../eval/stat_ClimateDecision.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(as.numeric(concerned[, 1]), riskAd$TOT, ylab="Protective decision", 
     xlab="Climate literacy", pch=20)
abline(lm(riskAd$TOT ~ as.numeric(concerned[, 1])))
legend("topleft",legend=c(paste("r:", round(cdcl,2)),
                          paste("N:", round(ndcl,2))), 
       bty="n", cex=0.8)
dev.off()

# Is there a correlation between accuracy and:
# Income

# Political preference
crp = cor(riskAd$TOT, as.numeric(user_resp$DEM7), use = "na.or.complete")
ncrp = pwr.r.test(r=crp, sig.level =0.05, power=0.95)$n

png(file="../eval/stat_decisionPolitical.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(riskAd$TOT, as.numeric(user_resp$DEM7), xlab="Protective decisions", 
     ylab="Politics", pch=20)
abline(lm(as.numeric(user_resp$DEM7) ~ riskAd$TOT))
legend("topleft",legend=c(paste("r:", round(crp,2)),
                          paste("N:", round(ncrp,2))), 
       bty="n", cex=0.8)
dev.off()

ccp = cor(as.numeric(concerned[, 1]), as.numeric(user_resp$DEM7), use = "na.or.complete")
nccp = pwr.r.test(r=ccp, sig.level =0.05, power=0.95)$n

png(file="../eval/stat_ClimLitPolitical.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(as.numeric(concerned[, 1]), as.numeric(user_resp$DEM7), xlab="Climate Literacy", 
     ylab="Politics", pch=20)
abline(lm(as.numeric(user_resp$DEM7) ~ as.numeric(concerned[, 1])))
legend("topleft",legend=c(paste("r:", round(ccp,2)),
                          paste("N:", round(nccp,2))), 
       bty="n", cex=0.8)
dev.off()

cap = cor(empaccexp$Total, as.numeric(user_resp$DEM7), use = "na.or.complete")
ncap = pwr.r.test(r=cap, sig.level =0.05, power=0.95)$n

png(file="../eval/stat_ClimLitAccuracy.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(empaccexp$Total, as.numeric(user_resp$DEM7), xlab="Total Accuracy", 
     ylab="Politics", pch=20)
abline(lm(as.numeric(user_resp$DEM7) ~ empaccexp$Total))
legend("topleft",legend=c(paste("r:", round(cap,2)),
                          paste("N:", round(ncap,2))), 
       bty="n", cex=0.8)
dev.off()

# Kruskal-Wallis test in R
# http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
# Employment
# Education
# Race
# Age
# Gender
# Location

# Is there a correlation between overall accuracy and seeking climate information? 
cam1 = cor(empaccexp$Total, scale_mot$MOT1)
ncam1 = pwr.r.test(r=cam1, sig.level =0.05, power=0.95)$n
# plot(empaccexp$Total, scale_mot$MOT1)

png(file="../eval/stat_accuracyClimateMotivation.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(empaccexp$Total, scale_mot$MOT1, xlab="Total Accuracy", 
     ylab="Climate Motivation", pch=20)
abline(lm(scale_mot$MOT1 ~ empaccexp$Total))
legend("topleft",legend=c(paste("r:", round(cam1,2)),
                          paste("N:", round(ncam1,2))), 
       bty="n", cex=0.8)
dev.off()

# Is there a correlation between overall accuracy and seeking science/stat information? 
cam23 = cor(empaccexp$Total, scale_mot$MOT2+scale_mot$MOT3, use = "na.or.complete")
ncam23 = pwr.r.test(r=cam23, sig.level =0.05, power=0.95)$n
# plot(empaccexp$Total, scale_mot$MOT2+scale_mot$MOT3)

png(file="../eval/stat_accuracyStatSciMotivation.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(empaccexp$Total, scale_mot$MOT2+scale_mot$MOT3, xlab="Total Accuracy", 
     ylab="Stat/Science Motivation", pch=20)
abline(lm(scale_mot$MOT2+scale_mot$MOT3 ~ empaccexp$Total))
legend("topleft",legend=c(paste("r:", round(cam23,2)),
                          paste("N:", round(ncam23,2))), 
       bty="n", cex=0.8)
dev.off()

# Is there a correlation between decision options and seeking protective resource information? 
crm4 = cor(riskAd$TOT, scale_mot$MOT4)
ncrm4 = pwr.r.test(r=crm4, sig.level =0.05, power=0.95)$n
# plot(empaccexp$Total, scale_mot$MOT1)

png(file="../eval/stat_decisionProtectiveMotivation.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(riskAd$TOT, scale_mot$MOT4, xlab="Protective decisions", 
     ylab="Protective Motivation", pch=20)
abline(lm(scale_mot$MOT4 ~ riskAd$TOT))
legend("topleft",legend=c(paste("r:", round(crm4,2)),
                          paste("N:", round(ncrm4,2))), 
       bty="n", cex=0.8)
dev.off()

# Is there a correlation between overall accuracy and usefulness?
csus = cor(empaccexp$Total, sus_scores$TOT)
nsus = pwr.r.test(r=csus, sig.level =0.05, power=0.95)$n

png(file="../eval/stat_accuracyUsefulness.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(empaccexp$Total, sus_scores$TOT, xlab="Total Accuracy", 
     ylab="Usefulness", pch=20)
abline(lm(sus_scores$TOT ~ empaccexp$Total))
legend("topleft",legend=c(paste("r:", round(csus,2)),
                          paste("N:", round(nsus,2))), 
       bty="n", cex=0.8)
dev.off()

# Is there a correlation between overall accuracy and decision options? 
crt = cor(riskAd$TOT, empaccexp$Total)
ncrt = pwr.r.test(r=crt, sig.level =0.05, power=0.95)$n
# plot(riskAd$TOT, empaccexp$Total)

png(file="../eval/stat_decisionAccuracy.png", family="Helvetica", res=300, 
    units="in", width=4, height=2.5, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.5,2.5,1.5,0.75))

plot(riskAd$TOT, empaccexp$Total, xlab="Protective decisions", 
     ylab="Total Accuracy", pch=20)
abline(lm(empaccexp$Total ~ riskAd$TOT))
legend("topleft",legend=c(paste("r:", round(crt,2)),
                          paste("N:", round(ncrt,2))), 
       bty="n", cex=0.8)
dev.off()

# Is there a correlation between accuracy and jargon/missing elements? 
# caj = cor(empaccexp$Total, jargon)
# pwr.r.test(r=caj, sig.level =0.05, power=0.95)$n
# plot(empaccexp$Total, jargon)
# Are people failing to understand jargon/missing elements?

mean(c(20,2,100,30,25,60,100,10,10,50,50,30,20))
mean(c(20,2,30,25,10,10,30,20))

pwr.r.test(r=.2, sig.level =0.05, power=0.9)
