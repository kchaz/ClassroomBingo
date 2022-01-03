
source('ClassroomBingoFunctions.R')


# TODO: add ability to specify that it should save the resulting plots 
# with some kind of handling of file names so that don't overwrite when run
# this function multiple times for different set-ups


ClassroomBingoAnalysis <- function(noutcomes, probs, outcome_labels, cardsize,
  epsilon = 1e-8, save_plots = FALSE) {
  #' DESCRIPTION
  #'
  #'
  #' ARGUMENTS
  #' noutcomes:  number of possible outcomes from rolling the dice
  #' probs:  numeric vector giving probabilities for the possible outcomes.
  #'   Must have length noutcomes. Elements must sum to 1.
  #' outcome_labels:  character vector giving names for the outcomes (e.g.,
  #'   2:6 are possible dice sums for original problem). These are used for
  #'   labeling plots.
  #' cardsize:  numeric vector of length 1, giving number of spaces on bingo
  #'   card to be filled by selecting some outcomes
  #' epsilon: numeric vector of length 1; determines tolerance for detecting
  #'   ties for maximum-probability card. Default is 1e-8.
  #' save_plots: logical flag. If TRUE (the default), each plot is saved to a
  #' file.
  #'
  #' VALUE
  #'
  ###
  # Get all possible bingo cards for given card size and number of outcomes
  ###
  cards <- get_all_bingo_cards(noutcomes = noutcomes, cardsize = cardsize)


  #Identify best board(s) for cardsize rolls (the minimum possible # of rolls to win)
  initial_probs = pwin(cards = cards, probs = probs, nrolls = cardsize)
  initial_best_prob = max(initial_probs)
  
  
  #get inidices of card with probability within episilon of max probability 
  #(account for possible rounding error). Also get corresponding cards.
  initial_best_inds <- which(initial_probs >= initial_best_prob - epsilon)
  initial_best_cards <- cards[,initial_best_inds]
  
  #chart just for minimum rolls to win
  dotchart(initial_probs, 
           pch = 21, 
           bg = "lightblue",
           ylab = "Board Index", 
           xlab = sprintf("Probability of winning in %s rolls", cardsize),
           main = sprintf("Probabilities of winning in %s rolls for all possible boards", cardsize),
           pt.cex = 1.3, 
           cex.main = 1.5,
           cex.lab = 1.1,
           cex = 1.1,
           frame.plot = F
  )
  
  # Get matrix of probabilities for minimum # possible to win to min + 30 rolls
  nrollsvec <-  cardsize:(cardsize + 30)
  cum_mat = get_pwin_matrix(cards = cards,
                            probs = probs,
                            nrollsvec = nrollsvec)
  
  
  #plot cumulative probability graph
  plot_card_prob_trajectories(nrollsvec = nrollsvec,
                              mat = cum_mat,
                              cumulative = T,
                              initial_best_inds = initial_best_inds,
                              initial_best_cards = initial_best_cards,
                              legend_loc ="topleft",
                              outcome_labels = outcome_labels)
  
  #plot probability graph
  cum_to_prob <- function(v){
    n = length(v)
    return(c(v[1], v[2:n] - v[seq_len(n-1)]))
  }
  prob_mat = apply(cum_mat, MARGIN = 2, cum_to_prob)
  plot_card_prob_trajectories(nrollsvec = nrollsvec,
                              mat = prob_mat,
                              cumulative = F,
                              initial_best_inds = initial_best_inds,
                              initial_best_cards = initial_best_cards,
                              legend_loc = "topright",
                              outcome_labels = outcome_labels)
  

  
  #plot cumulative probability graph with equivalence coloring
  equiv_mat = get_equivalence_class_mat(cards)
  plot_card_prob_trajectories(nrollsvec = nrollsvec,
                              mat = cum_mat,
                              cumulative = T,
                              initial_best_inds = initial_best_inds,
                              initial_best_cards = initial_best_cards,
                              legend_loc ="topleft",
                              outcome_labels = outcome_labels,
                              color_by_equiv_mat = T,
                              equiv_mat = equiv_mat)
  
  
}

