# --------------------------------------------------------------------------------------------------------------------
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
# --------------------------------------------------------------------------------------------------------------------

# Overall accuracy --------------------------------------------------------
# name: block name
# var: black id in answer form
# id: block id number
calcAccuracy = function(name, var, id){
  ovacc = data.frame(
    # Expressiveness 
    exp1 = as.integer(split.by.block[[name]][paste0("EXP1",id)] == ans$Answer[which(ans$ID == "EXP1" & ans$VAR == var)]),
    exp2 = as.integer(split.by.block[[name]][paste0("EXP2",id)] == ans$Answer[which(ans$ID == "EXP2" & ans$VAR == var)]),
    exp3 = as.integer(split.by.block[[name]][paste0("EXP3",id)] == ans$Answer[which(ans$ID == "EXP3" & ans$VAR == var)]),
    exp4 = as.integer(split.by.block[[name]][paste0("EXP4",id)] == ans$Answer[which(ans$ID == "EXP4" & ans$VAR == var)]),
    # Emphasis
    emp1 = as.integer(split.by.block[[name]][paste0("EMP1",id)] == ans$Answer[which(ans$ID == "EMP1" & ans$VAR == var)]),
    emp2 = as.integer(split.by.block[[name]][paste0("EMP2",id)] == ans$Answer[which(ans$ID == "EMP2" & ans$VAR == var)]),
    emp3 = as.integer(split.by.block[[name]][paste0("EMP3",id)] == ans$Answer[which(ans$ID == "EMP3" & ans$VAR == var)]),
    emp4 = as.integer(split.by.block[[name]][paste0("EMP4",id)] == ans$Answer[which(ans$ID == "EMP4" & ans$VAR == var)]),
    emp5 = as.integer(split.by.block[[name]][paste0("EMP5",id)] == ans$Answer[which(ans$ID == "EMP5" & ans$VAR == var)]),
    emp6 = as.integer(split.by.block[[name]][paste0("EMP6",id)] == ans$Answer[which(ans$ID == "EMP6" & ans$VAR == var)]),
    # Accuracy
    acc1 = as.integer(split.by.block[[name]][paste0("ACC1",id)] == ans$Answer[which(ans$ID == "ACC1" & ans$VAR == var)]),
    acc2 = as.integer(split.by.block[[name]][paste0("ACC2",id)] == ans$Answer[which(ans$ID == "ACC2" & ans$VAR == var)]),
    acc3 = as.integer(split.by.block[[name]][paste0("ACC3",id)] == ans$Answer[which(ans$ID == "ACC3" & ans$VAR == var)]),
    acc4 = as.integer(split.by.block[[name]][paste0("ACC4",id)] == ans$Answer[which(ans$ID == "ACC4" & ans$VAR == var)]),
    acc5 = as.integer(split.by.block[[name]][paste0("ACC5",id)] == ans$Answer[which(ans$ID == "ACC5" & ans$VAR == var)]),
    acc6 = as.integer(split.by.block[[name]][paste0("ACC6",id)] == ans$Answer[which(ans$ID == "ACC6" & ans$VAR == var)]),
    acc7 = as.integer(split.by.block[[name]][paste0("ACC7",id)] == ans$Answer[which(ans$ID == "ACC7" & ans$VAR == var)])
  )
  ovacc$tot = rowSums(ovacc)
  return(ovacc)
}

# Accuracy time -----------------------------------------------------------
# name: block name
# id: block id number
calcAccuracyTime = function(name, id){
  
  acctime = split.by.block[[name]][,c(paste0("X.timing1_Page.Submit",id), 
                                      paste0("timing2_Page.Submit",id), 
                                      paste0("timing3_Page.Submit",id), 
                                      paste0("timing5_Page.Submit",id),
                                      paste0("timing6_Page.Submit",id), 
                                      paste0("timing7_Page.Submit",id))]
  acctime = as.data.frame(apply(acctime, 2, as.numeric))
  acctime$tot = rowSums(acctime)
  
  return(acctime)
}

# Climate literacy --------------------------------------------------------
# name: block name
# id: block id number
calcClimLit = function(name, id){
  climLit = data.frame(
    # Expressiveness 
    clim1 = clim$CLIM1_CAT[match(unlist(split.by.block[[name]][paste0("CLIM1",id)]), clim$CLIM1)],
    clim2 = clim$CLIM2_CAT[match(unlist(split.by.block[[name]][paste0("CLIM2",id)]), clim$CLIM2)],
    clim4 = clim$CLIM4_CAT[match(unlist(split.by.block[[name]][paste0("CLIM4",id)]), clim$CLIM4)],
    clim7 = clim$CLIM7_CAT[match(unlist(split.by.block[[name]][paste0("CLIM7",id)]), clim$CLIM7)],
    clim8 = clim$CLIM8_CAT[match(unlist(split.by.block[[name]][paste0("CLIM8",id)]), clim$CLIM8)],
    clim9 = clim$CLIM9_CAT[match(unlist(split.by.block[[name]][paste0("CLIM9",id)]), clim$CLIM9)],
    clim10 = clim$CLIM10_CAT[match(unlist(split.by.block[[name]][paste0("CLIM10",id)]), clim$CLIM10)]
  )
  climLit$tot = rowSums(climLit)
  
  climLit$col = ifelse(climLit$tot > 0, redBlue[1], NA)
  climLit$col = ifelse(climLit$tot < 0, redBlue[length(redBlue)], climLit$col)
  climLit$col = ifelse(climLit$tot == 0, "black", climLit$col)
  
  return(climLit)
}

# System Usability Scale (SUS) --------------------------------------------
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

sus_scale = c("strongly disagree", "somewhat disagree", 
              "neither agree nor disagree", "somewhat agree", "strongly agree")

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

calcstraightScores = function(name, id, sus_scale){
  
  # Returns vector position 1:5 (Strongly disagree to strong agree)
  sus_score = data.frame(
    USE2 = match(unlist(split.by.block[[name]][paste0("USE2",id)]), sus_scale),
  USE3 = match(unlist(split.by.block[[name]][paste0("USE3",id)]), sus_scale),
  USE4 = match(unlist(split.by.block[[name]][paste0("USE4",id)]), sus_scale),
  USE5 = match(unlist(split.by.block[[name]][paste0("USE5",id)]), sus_scale),
  USE6 = match(unlist(split.by.block[[name]][paste0("USE6",id)]), sus_scale),
  USE7 = match(unlist(split.by.block[[name]][paste0("USE7",id)]), sus_scale),
  USE8 = match(unlist(split.by.block[[name]][paste0("USE8",id)]), sus_scale),
  USE9 = match(unlist(split.by.block[[name]][paste0("USE9",id)]), sus_scale),
  USE10 = match(unlist(split.by.block[[name]][paste0("USE10",id)]), sus_scale),
  USE11 = match(unlist(split.by.block[[name]][paste0("USE11",id)]), sus_scale)
  )
  
  sus_score$same = apply(sus_score, 1, function(X){length(unique(X)) == 1})

  return(sus_score)
}

calcSusScores = function(name, id){
  # Positive
  sus_scores = data.frame(USE2 = positive_sus(unlist(split.by.block[[name]][paste0("USE2",id)]), sus_scale),
                          USE3 = positive_sus(unlist(split.by.block[[name]][paste0("USE3",id)]), sus_scale),
                          USE5 = positive_sus(unlist(split.by.block[[name]][paste0("USE5",id)]), sus_scale),
                          USE7 = positive_sus(unlist(split.by.block[[name]][paste0("USE7",id)]), sus_scale),
                          USE9 = positive_sus(unlist(split.by.block[[name]][paste0("USE9",id)]), sus_scale),
                          # Negative
                          USE4 = negative_sus(unlist(split.by.block[[name]][paste0("USE4",id)]), sus_scale),
                          USE6 = negative_sus(unlist(split.by.block[[name]][paste0("USE6",id)]), sus_scale),
                          USE8 = negative_sus(unlist(split.by.block[[name]][paste0("USE8",id)]), sus_scale),
                          USE10 = negative_sus(unlist(split.by.block[[name]][paste0("USE10",id)]), sus_scale),
                          USE11 = negative_sus(unlist(split.by.block[[name]][paste0("USE11",id)]), sus_scale))
  # Add up the converted responses for each user and multiply that total by 2.5. 
  # This converts the range of possible values from 0 to 100 instead of from 0 to 40.
  sus_scores$TOT = rowSums(sus_scores, na.rm = TRUE) * 2.5
  
  return(sus_scores)
}

calcSusReg = function(name, id){
  # Positive
  sus_scores = data.frame(USE2 = match(unlist(split.by.block[[name]][paste0("USE2",id)]), sus_scale),
                          USE3 = match(unlist(split.by.block[[name]][paste0("USE3",id)]), sus_scale),
                          USE5 = match(unlist(split.by.block[[name]][paste0("USE5",id)]), sus_scale),
                          USE7 = match(unlist(split.by.block[[name]][paste0("USE7",id)]), sus_scale),
                          USE9 = match(unlist(split.by.block[[name]][paste0("USE9",id)]), sus_scale),
                          # Negative
                          USE4 = match(unlist(split.by.block[[name]][paste0("USE4",id)]), sus_scale),
                          USE6 = match(unlist(split.by.block[[name]][paste0("USE6",id)]), sus_scale),
                          USE8 = match(unlist(split.by.block[[name]][paste0("USE8",id)]), sus_scale),
                          USE10 = match(unlist(split.by.block[[name]][paste0("USE10",id)]), sus_scale),
                          USE11 = match(unlist(split.by.block[[name]][paste0("USE11",id)]), sus_scale))
  
  return(sus_scores)
}

# Protective decisions ----------------------------------------------------

chalop1 = data.frame(ans = c("do nothing (protect against 0 heavy rainfall events per year)", 
                             "install basic erosion control measures that protect against 2 heavy rainfall events per year",
                             "install improved erosion control measures that protect against 4 heavy rainfall events per year",
                             "pave the driveway (protect against 120 heavy rainfall events over 30 years)"),
                     val = 0:3)

chalop2 = data.frame(ans = c("no", "yes"), val = c(0,1))

# calcProtective = function(name, id){
#   riskAd = data.frame(CHAL1 = chalop1$val[match(unlist(split.by.block[[name]][paste0("CHAL1.1",id)]), chalop1$ans)],
#                       CHAL2 = chalop2$val[match(unlist(split.by.block[[name]][paste0("CHAL2.1",id)]), chalop2$ans)])
#   riskAd$TOT = rowSums(riskAd)
#   
#   return(riskAd)
# }

calcProtective = function(name, id){
  riskAd = data.frame(CHAL1 = chalop1$val[match(unlist(split.by.block[[name]][paste0("CHAL1.1",id)]), chalop1$ans)],
                      CHAL2 = chalop2$val[match(unlist(split.by.block[[name]][paste0("CHAL2.1",id)]), chalop2$ans)])
  #riskAd$TOT = rowSums(riskAd)
  
  return(riskAd)
}

protectiveTable = function(name, id){
  pTab = data.frame(name = name, CHAL1_ans = split.by.block[[name]][paste0("CHAL1.1",id)],
                      CHAL1_txt = split.by.block[[name]][paste0("CHAL1.2",id)],
                      CHAL1_con = split.by.block[[name]][paste0("CHAL1.3",id)],
                      CHAL1_lik = split.by.block[[name]][paste0("CHAL1.4",id)],
                      CHAL2_ans = split.by.block[[name]][paste0("CHAL2.1",id)],
                      CHAL2_txt = split.by.block[[name]][paste0("CHAL2.2",id)],
                      CHAL2_con = split.by.block[[name]][paste0("CHAL2.3",id)],
                      CHAL2_lik = split.by.block[[name]][paste0("CHAL2.4",id)])
  colnames(pTab) = c("name", "CHAL1_ans", "CHAL1_txt", "CHAL1_con", "CHAL1_lik", 
                     "CHAL2_ans", "CHAL2_txt", "CHAL2_con", "CHAL2_lik")
  return(pTab)
}

# Frequency vs Intensity --------------------------------------------------

calcFreqVSInten = function(name, var, id){
  
  # Frequency 
  freq = data.frame(
    exp1 = as.integer(split.by.block[[name]][paste0("EXP1",id)] == ans$Answer[which(ans$ID == "EXP1" & ans$VAR == var)]),
    exp2 = as.integer(split.by.block[[name]][paste0("EXP2",id)] == ans$Answer[which(ans$ID == "EXP2" & ans$VAR == var)]),
    # emp1 = as.integer(split.by.block[[name]][paste0("EMP1",id)] == ans$Answer[which(ans$ID == "EMP1" & ans$VAR == var)]),
    # emp2 = as.integer(split.by.block[[name]][paste0("EMP2",id)] == ans$Answer[which(ans$ID == "EMP2" & ans$VAR == var)]),
    # emp3 = as.integer(split.by.block[[name]][paste0("EMP3",id)] == ans$Answer[which(ans$ID == "EMP3" & ans$VAR == var)]),
    acc1 = as.integer(split.by.block[[name]][paste0("ACC1",id)] == ans$Answer[which(ans$ID == "ACC1" & ans$VAR == var)]),
    acc2 = as.integer(split.by.block[[name]][paste0("ACC2",id)] == ans$Answer[which(ans$ID == "ACC2" & ans$VAR == var)]),
    acc3 = as.integer(split.by.block[[name]][paste0("ACC3",id)] == ans$Answer[which(ans$ID == "ACC3" & ans$VAR == var)])
  )
  freq$tot = rowSums(freq)
  
  # Intensity
  inten = data.frame(
    exp3 = as.integer(split.by.block[[name]][paste0("EXP3",id)] == ans$Answer[which(ans$ID == "EXP3" & ans$VAR == var)]),
    exp4 = as.integer(split.by.block[[name]][paste0("EXP4",id)] == ans$Answer[which(ans$ID == "EXP4" & ans$VAR == var)]),
    # emp4 = as.integer(split.by.block[[name]][paste0("EMP4",id)] == ans$Answer[which(ans$ID == "EMP4" & ans$VAR == var)]),
    # emp5 = as.integer(split.by.block[[name]][paste0("EMP5",id)] == ans$Answer[which(ans$ID == "EMP5" & ans$VAR == var)]),
    # emp6 = as.integer(split.by.block[[name]][paste0("EMP6",id)] == ans$Answer[which(ans$ID == "EMP6" & ans$VAR == var)]),
    # acc4 = as.integer(split.by.block[[name]][paste0("ACC4",id)] == ans$Answer[which(ans$ID == "ACC4" & ans$VAR == var)]),
    acc5 = as.integer(split.by.block[[name]][paste0("ACC5",id)] == ans$Answer[which(ans$ID == "ACC5" & ans$VAR == var)]),
    acc6 = as.integer(split.by.block[[name]][paste0("ACC6",id)] == ans$Answer[which(ans$ID == "ACC6" & ans$VAR == var)]),
    acc7 = as.integer(split.by.block[[name]][paste0("ACC7",id)] == ans$Answer[which(ans$ID == "ACC7" & ans$VAR == var)])
  )
  inten$tot = rowSums(inten)
  
  return(list(freq = freq, inten = inten))
}

# Motivation --------------------------------------------------------------

calcMotivation = function(name, id){
  mot = data.frame( 
    clim = ifelse(split.by.block[[name]][paste0("MOT1_1",id)]=="", 0, 1),
    science = ifelse(split.by.block[[name]][paste0("MOT1_2",id)]=="", 0, 1),
    stat = ifelse(split.by.block[[name]][paste0("MOT1_3",id)]=="", 0, 1),
    prot = ifelse(split.by.block[[name]][paste0("MOT1_4",id)]=="", 0, 1)
  )
  colnames(mot) = c("clim", "sci", "stat", "prot")
  return(mot)
}

# grep("climate change", split.by.block$Area_Freq$MOT1)
# strsplit(split.by.block$Area_Freq$MOT1, ",")
# Jargon ------------------------------------------------------------------
calcJargon = function(name, var, id){
  
  # Frequency 
  jar = data.frame(
    emp3 = as.integer(split.by.block[[name]][paste0("EMP3",id)] == ans$Answer[which(ans$ID == "EMP3" & ans$VAR == var)]),
    emp6 = as.integer(split.by.block[[name]][paste0("EMP6",id)] == ans$Answer[which(ans$ID == "EMP6" & ans$VAR == var)])
  )
  jar$tot = rowSums(jar)
  
  return(jar)
}

returnVar = function(name, id, var){
  varval = split.by.block[[name]][paste0(var,id)]
  
  return(varval)
}

emp3ans = data.frame(ans = c("historical measurements",
                             "model results over the historical period", 
                             "a 100% accurate prediction of the future",
                             "a representation of the future based on how much carbon dioxide might be released into the atmosphere",
                             "I don't know."),
                     val = 0:4)

calcJargVal = function(name, id, val){
  riskAd = data.frame(val = emp3ans$val[match(unlist(split.by.block[[name]][paste0(val,id)]), emp3ans$ans)])
  
  return(riskAd)
}

# Read in code topics (secondary coding) ----------------------------------

# Function to add secondary codes to data.frames based on the primary code
add_topic = function(df, topics){
  df$topic = NA
  # df$decision = NA
  for(i in 1:nrow(topics)){
    # df$topic[which(df$word == topics$Code[i])] = topics$What.influences.participant.decisions.[i]
    df$topic[which(df$word == topics$Code[i])] = topics$AI.Analysis.of.Scenario.Justification..ChatGPT.[i]
    # df$decision[which(df$topic == topics$What.influences.participant.decisions.[i])] = topics$tricode[i]
  }
  return(df)
}

# Function to create tables of code frequency and wordclouds --------------
word_freq = function(list_of_codes, color_codes, filename, filename2, 
                     plot.legend = TRUE){
  # list_of_codes = combined_df
  # Convert word list to a table organized by frequency
  tab <- table(list_of_codes)
  tab <- data.frame(word = names(tab), count = as.numeric(tab))
  tab <- arrange(tab, desc(count))
  tab <- add_topic(tab, topics)
  # print(tab)
  
  tab$color = NA
  for(l in 1:nrow(color_codes)){
    tab$color[which(tab$topic == color_codes$code[l])] = color_codes$colors[l]
  }
  
  # create word cloud
  wordcloud(words = tab$word, freq = tab$count, scale = c(3,.5), min.freq = 1,
            max.words=200, ordered.colors=TRUE, random.order=FALSE, rot.per=0,
            colors = tab$color)
  if(plot.legend){
    legend("topleft", legend=unique(tab$topic), pch=15, 
           col = unique(tab$color), ncol=2)
  }
  
  write.csv(tab, filename, row.names = FALSE)
  
  # Create tables for secondary coding
  combined_x = data.frame(word = list_of_codes)
  # colnames(combined_x) = c("quote", "word")
  combined_x = add_topic(combined_x, topics)
  top <- table(combined_x$topic)
  top <- data.frame(word = names(top), count = as.numeric(top))
  top <- arrange(top, desc(count))
  write.csv(top, filename2, row.names = FALSE)
  
  return(tab)
}

# Calculate the percentage of each secondary code for each subgroup and return tabkes for plotting --------------
group_perc = function(textchoices, codename, varname, groupstr, subgroupstr, color_codes, scenname, dirname){
  par(mar = c(0, 0, 0, 0))
  drive_tab = lapply(X=1:length(groupstr), 
                     function(X){word_freq(textchoices[,codename][textchoices[,varname] == groupstr[X]], 
                                           color_codes, 
                                           paste0(dirname, "prim_coding_", scenname, subgroupstr[X], ".csv"),
                                           paste0(dirname, "sec_coding_", scenname, subgroupstr[X], ".csv"))})
  names(drive_tab) = subgroupstr
  
  # primary
  prim = lapply(drive_tab, function(X){(X$count/sum(X$count))*100})
  prim_tab = list()
  
  for(X in 1:length(subgroupstr)){
    prim_tab[[X]] = drive_tab[[X]][ ,1:3]
    prim_tab[[X]]$percent = prim[[X]]
    
    unused = data.frame(word = setdiff(topics$Code, prim_tab[[X]]$word))
    unused$percent = unused$count = 0
    unused = add_topic(unused, topics)
    prim_tab[[X]] = rbind(prim_tab[[X]], unused)
  }
  
  prim.df = do.call(rbind.data.frame, prim_tab)
  prim.df$level = unlist(lapply(X=1:length(subgroupstr), function(X){rep(subgroupstr[X], times=dim(prim_tab[[X]])[1])}))
  
  # secondary
  sec_tab = data.frame(topic = unique(topics$AI.Analysis.of.Scenario.Justification..ChatGPT.))
  
  for(X in 1:length(subgroupstr)){
    for(i in 1:length(sec_tab$topic)){
      sec_tab[i, (X+1)] = sum(drive_tab[[subgroupstr[X]]]$count[which(drive_tab[[subgroupstr[X]]]$topic == sec_tab$topic[i])])
    }
  }
  colnames(sec_tab) = c("topic", paste0(subgroupstr, "count"))
  sec_tab$color = color_codes$colors[match(sec_tab$topic, color_codes$code)]
  
  # Set percentages
  groupcount = rep(NA, length(subgroupstr))
  pertab = sec_tab
  for(X in 1:length(subgroupstr)){
    groupcount[X] = sum(pertab[ ,(X+1)])
    for(i in 1:length(pertab$topic)){
      pertab[i, (X+1)] = round(pertab[i, (X+1)]/groupcount[X]*100, 1)
    }
  }
  colnames(pertab) = c("Theme", paste0(groupstr, " (n=", groupcount, ")"), "color")
  
  sec_text = data.frame(topic = rep(pertab$Theme, length(subgroupstr)), 
                        percent = unlist(pertab[ ,2:(length(subgroupstr)+1)]),
                        groups = as.vector(sapply(subgroupstr, rep, times=length(pertab$Theme))))
  
  return(list(pertab = pertab, sec_text = sec_text, prim.df=prim.df))
}
# -------------------------------------------------------------------------
# Functions for plotting
# https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

addSmallLegend <- function(myPlot, pointSize = 5, textSize = 8, spaceLegend = 0.75) {
  myPlot +
    guides(shape = guide_legend(nrow=2, byrow=TRUE, override.aes = list(size = pointSize)),
           color = guide_legend(nrow=2, byrow=TRUE, override.aes = list(size = pointSize)),
           fill = guide_legend(nrow=2, byrow=TRUE))+
    theme(legend.title = element_text(size = textSize), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}
