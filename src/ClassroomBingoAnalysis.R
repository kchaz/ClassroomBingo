
# source('ClassroomBingoFunctions.R')

ClassroomBingoAnalysis <- function(noutcomes, probs, outcome_labels, cardsize,
  nrollsvec = cardsize:(cardsize + 30), epsilon = 1e-8, save_plots = FALSE,
  caseLabel = "", plotDir = ".", ...) {
  #' DESCRIPTION
  #'       For a specified problem configuration, computes win probabilities
  #' for all possible bingo cards. Generates 4 plots:
  #' 1. Dotplot of card index versus win probability for number of rolls equal
  #' to the number of spaces on bingo card (i.e., cardsize).
  #' 2. Line plot of cumulative win probability versus number of rolls, with
  #' one line for each possible card.
  #' 3. Line plot of pointwise win probability versus number of rolls, with
  #' one line for each possible card.
  #' 4. Line plot of cumulative win probability versus number of rolls, with
  #' one line for each possible card, and lines colored by card equivalence
  #' class as defined by function get_equivalence_class_mat.
  #'
  #' ARGUMENTS
  #' noutcomes:  numeric vector of length 1, giving number of possible
  #'   outcomes from rolling the dice
  #' probs:  numeric vector giving probabilities for the possible outcomes.
  #'   Must have length noutcomes. Elements must sum to 1.
  #' outcome_labels:  character vector giving names for the outcomes (e.g.,
  #'   2:6 are possible dice sums for original problem). These are used for
  #'   labeling plots.
  #' cardsize:  numeric vector of length 1, giving number of spaces on bingo
  #'   card to be filled by selecting some outcomes
  #' nrollsvec:  numeric vector, giving numbers of rolls, i.e., numbers
  #'   of outcomes to generate. Default is cardsize:(cardsize + 30).
  #' epsilon: numeric vector of length 1; determines tolerance for detecting
  #'   ties for maximum-probability card. Default is 1e-8.
  #' save_plots: logical flag. If TRUE (the default), each plot is saved to a
  #'   file.
  #' caseLabel  character string, used as part of plot filenames. Default is "".
  #'   Ignored if save_plots is FALSE.
  #' plotDir  character string giving the path to a directory where you want
  #'   to save plot files. The default is ".", i.e., the current working
  #'   directory. Ignored if save_plots is FALSE.
  #' ...  optional arguments to function pdf; ignored if save_plots is FALSE.
  #'
  #' VALUE
  #' list with components:
  #' cards:  numeric matrix giving all possible bingo cards. See function
  #'   get_all_bingo_cards for details.
  #' cumprob_mat:  numeric matrix with length(nrollsvec) rows and ncol(cards)
  #'   columns. Each row corresponds to a number of rolls, and each column to
  #'   a card. The (i,j)th element gives the probability that the jth card
  #'   will win in nrollsvec[i] rolls or fewer. See function get_pwin_matrix
  #'   for details. 
  #' noutcomes:       value of argument noutcomes
  #' probs:           value of argument probs
  #' outcome_labels:  value of argument outcome_labels
  #' cardsize:        value of argument cardsize
  #' nrollsvec:       value of argument nrollvec
  #' epsilon:         value of argument epsilon
  #' save_plots:      value of argument save_plots
  #' caseLabel:       value of argument caseLabel
  #' plotDir:         value of argument plotDir
  #' call:            image of the current call to this function. 
  #' 
  ###
  # Construct filenames for plots
  ###
    pnames <- paste0("Case", caseLabel, "-Plot0", 1:4, ".pdf") 
    pnames <- file.path( plotDir, pnames )

  ###
  # Get all possible bingo cards for given card size and number of outcomes
  ###
  cards <- get_all_bingo_cards(noutcomes = noutcomes, cardsize = cardsize)

  ###
  # Identify best card(s) for cardsize rolls (the minimum possible # of rolls
  # to win)
  ###
  initial_probs <- pwin(cards = cards, probs = probs, nrolls = cardsize)
  initial_best_prob <- max(initial_probs)
  
  ###
  # Get indices of card(s) with probability within episilon of max probability 
  # (account for possible rounding error). Also get corresponding cards.
  ###
  initial_best_inds <- which(initial_probs >= initial_best_prob - epsilon)
  initial_best_cards <- cards[, initial_best_inds]
  
  ###
  # Dotchart of win probabilities, just for minimum number of rolls to win
  ###
  if (save_plots) pdf(pnames[1], ...)
  dotchart(initial_probs, 
           pch = 21, 
           bg = "lightblue",
           ylab = "Card Index", 
           xlab = sprintf("Probability of winning in %s rolls", cardsize),
           main = sprintf(paste("Probabilities of winning in %s rolls for",
             "all possible cards"), cardsize),
           pt.cex = 1.3, 
           cex.main = 1.5,
           cex.lab = 1.1,
           cex = 1.1,
           frame.plot = FALSE
  )
  if (save_plots) dev.off()
  
  ###
  # Get matrix of probabilities for all values in nrollsvec.
  ###
  cum_mat <- get_pwin_matrix(cards = cards,
                            probs = probs,
                            nrollsvec = nrollsvec)
  
  ###
  # Plot cumulative probability graph
  ###
  if (save_plots) pdf(pnames[2], ...)
  plot_card_prob_trajectories(nrollsvec = nrollsvec,
                              mat = cum_mat,
                              cumulative = TRUE,
                              initial_best_inds = initial_best_inds,
                              initial_best_cards = initial_best_cards,
                              legend_loc ="topleft",
                              outcome_labels = outcome_labels)
  if (save_plots) dev.off()
  
  ###
  # Plot probability graph
  ###
  if (FALSE) {
    cum_to_prob <- function(v) {
      n <- length(v)
      return(c(v[1], v[2:n] - v[seq_len(n-1)]))
    }
    prob_mat <- apply(cum_mat, MARGIN = 2, cum_to_prob)
  }
  # TODO: TEST THIS NEXT LINE:
  prob_mat <- rbind(cum_mat[1, ], diff(cum_mat[-1, ])
  if (save_plots) pdf(pnames[3], ...)
  plot_card_prob_trajectories(nrollsvec = nrollsvec,
                              mat = prob_mat,
                              cumulative = FALSE,
                              initial_best_inds = initial_best_inds,
                              initial_best_cards = initial_best_cards,
                              legend_loc = "topright",
                              outcome_labels = outcome_labels)
  if (save_plots) dev.off()
  
  ###
  # Plot cumulative probability graph with equivalence coloring
  ###
  equiv_mat <- get_equivalence_class_mat(cards)
  if (save_plots) pdf(pnames[4], ...)
  plot_card_prob_trajectories(nrollsvec = nrollsvec,
                              mat = cum_mat,
                              cumulative = TRUE,
                              initial_best_inds = initial_best_inds,
                              initial_best_cards = initial_best_cards,
                              legend_loc ="topleft",
                              outcome_labels = outcome_labels,
                              color_by_equiv_mat = TRUE,
                              equiv_mat = equiv_mat)
  if (save_plots) dev.off()

  ###
  # Return list of some useful stuff, but do it quietly.
  ###
  out <- list(cards = cards,
	      cumprob_mat = cum_mat,
	      noutcomes = noutcomes,
	      probs = probs,
	      outcome_labels = outcome_labels,
	      cardsize = cardsize,
	      nrollsvec = nrollsvec,
	      epsilon = epsilon,
	      save_plots = save_plots,
	      caseLabel = caseLabel,
	      plotDir = plotDir,
	      call = match.call())
  return(invisible(out))
}

