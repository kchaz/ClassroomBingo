
source('ClassroomBingoFunctions.R')


#TO DO: add ability to specify that it should save the resulting plots 
#with some kind of handling of file names so that don't override when run this function
#multiple times for different set-ups





ClassroomBingoAnalysis <- function(noutcomes, probs,  outcome_labels, card_size,
                                   epsilon = 1e-8,
                                   save_plots = F){
  #' noutcomes:  is number of outcomes in random process under consideration
  #' probs: is probability of each outcome (must have length noutcomes)
  #' outcome_labels : names for each outcome (e.g. 2:6 are possible dice sums for original problem)
  #'    these are used in labeling plots
  #' card_size: is the number of spaces on a bingo card formed with those outcomes
  #' epsilon: determines tolerance in detecting ties for maximum probability board
  #' save_plots: TO DO
  
  #get all possible bingo cards for given card size and number of outcomes
  cards = get_all_bingo_cards(noutcomes = 5, card_size = 5)


  #Identify best board(s) for card_size rolls (the minimum possible # of rolls to win)
  initial_probs = pwin(cards = cards, probs = probs, nrolls = card_size)
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
           xlab = sprintf("Probability of winning in %s rolls", card_size),
           main = sprintf("Probabilities of winning in %s rolls for all possible boards", card_size),
           pt.cex = 1.3, 
           cex.main = 1.5,
           cex.lab = 1.1,
           cex = 1.1,
           frame.plot = F
  )
  
  #get matrix of probabilities for 5-35 rolls
  nrollsvec <-  5:35
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

