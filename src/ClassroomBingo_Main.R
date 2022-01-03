library(combinat)
source('src/ClassroomBingoFunctions.R')
source('src/ClassroomBingoAnalysis.R')

###
#' Run case as originally presented in Signficance magazine (Dec 2021):
#' 5 possible outcomes to go on the bingo cards, cards of size 5.
#' Vary the probabilities of the outcomes.
###

save_plots <- TRUE
plotDir <- "plots"
if (save_plots && !dir.exists(plotDir)) dir.create(plotDir)

###
# setup parameters
###
noutcomes <- 5
card_size <- 5
outcome_labels <- as.character(2:6)

###
# CASE 1.
# Original problem probabilities, for dice sums 2,3,4,5,6
###
probs <- c(1, 4, 10, 12, 9)/36
out1 <- ClassroomBingoAnalysis(noutcomes = noutcomes,
                               probs = probs,
                               outcome_labels = outcome_labels,
                               card_size = card_size,
                               epsilon = 1e-8,
                               save_plots = save_plots,
                               caseLabel = 1,
                               plotDir = plotDir)
# See which is biggest at each roll
apply(out1$cumprob_mat, 1, which.max)

###
# CASE 2.
# A case with ties and a switching dynamic where number of rolls matters.
# Maintains same order (5 is highest probability outcome) as original case,
# for easier comparisons.
###
probs <- c(1, 4, 6, 20, 5)/36  
out2 <- ClassroomBingoAnalysis(noutcomes = noutcomes,
                               probs = probs,
                               outcome_labels = outcome_labels,
                               card_size = card_size,
                               epsilon = 1e-8,
                               save_plots = save_plots,
                               caseLabel = 2,
                               plotDir = plotDir)
# See which is biggest at each roll
apply(out2$cumprob_mat, 1, which.max)

###
# CASE 3.
# All outcomes are equally likely.
# (This means those outcomes in same multinomial equivalence class have same
# probabilities.)
###
probs <- rep(7.2, 5)/36   
out3 <- ClassroomBingoAnalysis(noutcomes = noutcomes,
                               probs = probs,
                               outcome_labels = outcome_labels,
                               card_size = card_size,
                               epsilon = 1e-8,
                               save_plots = save_plots,
                               caseLabel = 3,
                               plotDir = plotDir)

###
# CASE 4.
# A very extreme case.
###
probs <- c(1, 1, 1, 32, 1)/36
out4 <- ClassroomBingoAnalysis(noutcomes = noutcomes,
                               probs = probs,
                               outcome_labels = outcome_labels,
                               card_size = card_size,
                               epsilon = 1e-8,
                               save_plots = save_plots,
                               caseLabel = 4,
                               plotDir = plotDir)

###
# CASE 5.
# Sanity check - should get cumulative probabilities of 0's and 1's.
###
probs <-  c(0, 0, 0, 36, 0)/36
out5 <- ClassroomBingoAnalysis(noutcomes = noutcomes,
                               probs = probs,
                               outcome_labels = outcome_labels,
                               card_size = card_size,
                               epsilon = 1e-8,
                               save_plots = save_plots,
                               caseLabel = 5,
                               plotDir = plotDir)

###
# CASE 6.
# Test a case with 4 outcomes and bingo cards of size 6.
# Note: this takes much longer to run.
# probs <-  c(1,2,3,7,9,16,11)/49
###
probs <- c(1, 2, 3, 10)/16
out6 <- ClassroomBingoAnalysis(noutcomes = 4,
                               probs = probs,
                               outcome_labels = 1:4,
                               card_size = 6,
                               epsilon = 1e-8,
                               save_plots = save_plots,
                               caseLabel = 6,
                               plotDir = plotDir)

