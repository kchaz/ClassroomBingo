source('ClassroomBingo_RunSingleAnalysis.R')

#' Run case as originally presented in signficance magazine
#' 5 possible outcomes to go on the bingo cards cards of size 5
#' vary the probabilities of the outcomes

#setup parameters
noutcomes = 5
card_size = 5
outcome_labels = 2:6


#original problem probabilities
probs <- c(1,4,10,12,9)/36  #original problem, these are for dice sums 2,3,4,5,6

cum_mat <- ClassroomBingoAnalysis(noutcomes = noutcomes,
                                   probs = probs,
                                   outcome_labels = outcome_labels,
                                   card_size = card_size,
                                   epsilon = 1e-8,
                                   save_plots = F)
#see which is biggest at each roll
apply(cum_mat, 1, which.max)

#case with tie and switching dynamic where # of rolls matters
#maintains same order (5 is highest probability outcome) as original case
#for eaiser comparisons
probs <-  c(1,4,6,20,5)/36  
cum_mat <- ClassroomBingoAnalysis(noutcomes = noutcomes,
                       probs = probs,
                       outcome_labels = outcome_labels,
                       card_size = card_size,
                       epsilon = 1e-8,
                       save_plots = F)
#see which is biggest at each roll
apply(cum_mat, 1, which.max)



#all same 
probs <-  rep(7.2,5)/36     # means those in same multinomial equivalence class have same probs
cum_mat <- ClassroomBingoAnalysis(noutcomes = noutcomes,
                       probs = probs,
                       outcome_labels = outcome_labels,
                       card_size = card_size,
                       epsilon = 1e-8,
                       save_plots = F)

  
#very extreme casse
probs <-  c(1,1,1,32,1)/36  #extreme case
cum_mat <- ClassroomBingoAnalysis(noutcomes = noutcomes,
                       probs = probs,
                       outcome_labels = outcome_labels,
                       card_size = card_size,
                       epsilon = 1e-8,
                       save_plots = F)


#sanity check - should get 0's and 1's
probs <-  c(0,0,0,36,0)/36
cum_mat <- ClassroomBingoAnalysis(noutcomes = noutcomes,
                       probs = probs,
                       outcome_labels = outcome_labels,
                       card_size = card_size,
                       epsilon = 1e-8,
                       save_plots = F)



#test a case with 7 outcomes and bingo cards of size 4
probs <-  c(1,2,3,7,9,16,11)/49
cum_mat <- ClassroomBingoAnalysis(noutcomes = 7,
                                  probs = probs,
                                  outcome_labels = 1:7,
                                  card_size = 4,
                                  epsilon = 1e-8,
                                  save_plots = F)

