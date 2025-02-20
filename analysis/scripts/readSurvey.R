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

# Read in the results -----------------------------------------------------
responses = read.csv("../data/HeavyPrecip_June+5,+2024_11.24.csv")
questions = responses[1,]
responses = responses[2:nrow(responses), ]

# Keep only the good responses
responses = responses[which(responses$gc == 1), ]

# Remove low quality responses
remove = read.csv("../data/responses2remove_5June2024.csv")
remove_ind = match(remove$responses, responses$ResponseId)
responses = responses[-remove_ind, ]

# Identify the integers for each survey version ---------------------------
commitind = which(colnames(responses) == "COMMIT")
headerind = 1:commitind
oppind = which(colnames(responses) == "opp")
endind = oppind:ncol(responses)

# Find the columns associated with each survey version
AF = (commitind+1):(which(colnames(responses) == "DEM8_AF")+1)
AI = (which(colnames(responses) == "DEM8_AF")+2):(which(colnames(responses) == "DEM8_AI")+1)
BF = (which(colnames(responses) == "DEM8_AI")+2):(which(colnames(responses) == "DEM8_BF")+1)
BI = (which(colnames(responses) == "DEM8_BF")+2):(which(colnames(responses) == "DEM8_BI")+1)
XF = (which(colnames(responses) == "DEM8_BI")+2):(which(colnames(responses) == "DEM8_XF")+1)
XI = (which(colnames(responses) == "DEM8_XF")+2):(which(colnames(responses) == "DEM8_XI")+1)

# Split the survey from each block ----------------------------------------
block = c("Area_Freq", "Area_Int", "Bar_freq", "Bar_int", "Box_freq", "Box_int")

split.by.block = lapply(block, function(X){responses[which(responses$Block == X), ]})
names(split.by.block) = block

split.by.block$Area_Freq = split.by.block$Area_Freq[ , c(headerind, AF, endind)]
split.by.block$Area_Int = split.by.block$Area_Int[ , c(headerind, AI, endind)]
split.by.block$Bar_freq = split.by.block$Bar_freq[ , c(headerind, BF, endind)]
split.by.block$Bar_int = split.by.block$Bar_int[ , c(headerind, BI, endind)]
split.by.block$Box_freq = split.by.block$Box_freq[ , c(headerind, XF, endind)]
split.by.block$Box_int = split.by.block$Box_int[ , c(headerind, XI, endind)]

print("Average time")
dur_mins = as.numeric(responses$Duration..in.seconds.)/60
summary(dur_mins)
summary(as.numeric(unlist(lapply(split.by.block, '[[', 6)))/60)

# Extract the questions ---------------------------------------------------
questionslist = list()
questionslist$Area_Freq = questions[1, c(headerind, AF, endind)]
questionslist$Area_Int = questions[1, c(headerind, AI, endind)]
questionslist$Bar_freq = questions[1, c(headerind, BF, endind)]
questionslist$Bar_int = questions[1, c(headerind, BI, endind)]
questionslist$Box_freq = questions[1, c(headerind, XF, endind)]
questionslist$Box_int = questions[1, c(headerind, XI, endind)]

##########################################################################
# END
##########################################################################
