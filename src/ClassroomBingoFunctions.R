library(combinat)

get_all_bingo_cards <- function(noutcomes, cardsize) {
  #' DESCRIPTION
  #'       Generates all possible bingo cards of a given size and a given 
  #' number of allowed values (i.e., outcomes).
  #' 
  #' ARGUMENTS
  #' noutcomes:  numeric vector of length 1, giving number of possible
  #'   outcomes from rolling the dice
  #' cardsize:  numeric vector of length 1, giving number of spaces on bingo
  #'   card to be filled by selecting some outcomes
  #' 
  #' VALUE
  #'       numeric matrix with noutcomes rows and one column for each possible
  #' bingo card with cardsize spaces. The bingo cards are encoded by counts
  #' for each possible outcome. 
  #' 
  #' For example, if noutcomes = 5, column 02210 represents a card with zero
  #' of outcome 1, two of outcome 2, two of outcome 3, one of outcome 4, and
  #' zero of outcome 5. Notice that this ignores the order in which outcomes
  #' appear on a card. Thus, e.g., when possible outcomes are (2, 3, 4, 5, 6),
  #' a card with outcomes 33445 is equivalent to a card with outcomes 34543; 
  #' both these cards are represented by the encoding 02210.
  #' 
  cards <- xsimplex(p = noutcomes, n = cardsize) 
  return(cards)
}


### TODO:
### 1. REVISIT THIS FUNCTION. NEED TO DEFINE "C". AND, LAST 3 LINES PROBABLY
### CAN BE REPLACED WITH SOMETHING SIMPLER, E.G. USING FUNCTION match??
### 2. NEED TO FINISH DOCUMENTATION.
get_equivalence_class_mat <- function(cards) {
  #' DESCRIPTION
  #' 
  #' ARGUMENTS
  #' cards:  numeric matrix representing possible bingo cards, such as returned
  #'   by function get_all_bingo_cards.
  #' 
  #' VALUE
  #'  
  #'  
  ###
  # Sort each column to get single representation of cards with same groupings
  # (same number of doubles etc.)
  ###
  sorted <- apply(cards, MARGIN = 2, FUN = sort)

  ###
  # If there are not more outcomes than the alphabet, convert to a letters
  # representation that clearly distinguishes equivalence class representation
  # from roll counts and bingo cards.
  ###
  if (nrow(cards) <= 26) { 
    sorted <- apply(sorted, 2, function(col) { letters[x2u(col)] })
    strings <- apply(sorted, 2, paste, collapse = "")
  } else {
    # add commas if using numeric representation
    strings <- apply(sorted, 2, paste, collapse = ",")
  }

  ###
  # Identify the unique strings (equivalence classes)
  ###

  classes <- unique(strings)
  ###
  # Create C x ncols(cards) logical matrix with TRUEs encoding which class
  # each card belongs to.
  ###
  equiv_mat <- t(unlist(lapply(classes, FUN = function(c){c == strings},
    USE.NAMES = TRUE)))
  return(equiv_mat)
}


pwin <- function(cards, probs, nrolls) {
  #' DESCRIPTION
  #'       For each bingo card in a set of cards, calculates the probability of
  #' winning in nrolls rolls or fewer, given specified outcome probabilities.
  #' 
  #' ARGUMENTS
  #' cards:  numeric matrix representing possible bingo cards, such as returned
  #'   by function get_all_bingo_cards. Must have length(probs) rows. Each
  #'   column represents a bingo card. Ordinarily should have
  #'   nsimplex( p = length(probs), n = nrolls) columns, representing all
  #'   possible cards for a given cardsize and number of possible outcomes
  #'   (length(probs)), but this is not enforced. All elements must be
  #'   non-negative. If a vector, is coerced to be a 1-column matrix.
  #' probs:  numeric vector of probabilities for the possible outcomes.
  #'   Elements must sum to 1.
  #' nrolls:  numeric vector of length 1, giving number of rolls, i.e., number
  #'   of outcomes to generate.
  #'
  #' VALUE
  #'       numeric vector of length ncol(cards); the ith element gives the
  #' probability that the ith card will win in nrolls rolls or fewer. If
  #' nrolls < unique(colSums(cards)) - that is, if the number of rolls is less
  #' than the number of positions on a card, will return a vector of all 0's.
  #'  
  ###
  # Check inputs
  ###
  eps <- 1e-8
  cards <- as.matrix(cards)
  stopifnot( is.numeric(cards) )
  stopifnot( all(cards >= 0) )
  stopifnot( is.numeric(probs) )
  stopifnot( all(probs >= 0 & probs <= 1) )
  stopifnot( abs(sum(probs) - 1) < eps )
  stopifnot( is.numeric(nrolls) )
  stopifnot( length(nrolls) == 1 )
  ncards <- ncol(cards)
  noutcomes <- length(probs)
  if (nrow(cards) != noutcomes) {
    stop(paste("Number of rows in cards matrix must equal number of outcomes",
      "(i.e., length of probs)"))
  }
  cardsize <- unique(colSums(cards))
  # NOTE BUG FIX HERE: WAS 2 instead of 1:
  if (length(cardsize) > 1) {
    stop(paste("All bingo cards must have same size (i.e., column sums of",
      "cards matrix must all be the same)"))
  }

  ###
  # If too few rolls to win, return vector of zeros
  ###
  if (nrolls < cardsize) {
    return(numeric(ncards))
  }

  ###
  # Generate all possible sets of rolls, i.e., "roll sets", for the outcomes -
  # each roll set is represented by a vector of counts for the possible
  # outcomes (no order).
  ###
  rolls <- xsimplex(p = noutcomes, n = nrolls)

  ###
  # To get probability of each outcome count vector, need to take into account
  # multiple possible roll sequences that yield that outcome count. Do this by
  # applying multinomial pdf and outcome probabilities.
  ###
  prolls <- apply(rolls, MARGIN = 2, FUN = dmultinom, p = probs) 
  
  ###
  # For each card, check which roll sets will win and add up the probabilities
  # of those roll sets to get probability of winning with that card in nrolls
  # or fewer rolls.
  # Note: a roll set is a 'winning roll set' for a card if the count for each
  # outcome in the roll set is >= the count for that outcome on the card. This
  # says nothing about in which roll in the sequence the card wins, so we have
  # a cumulative probability.
  ###
  pwin_vec <- numeric(ncards)
  for (i in seq_len(ncards)){
    iwin <- ( cards[, i] <= rolls ) # Compare every roll set to that card
    iwin <- ( colSums(iwin) == noutcomes ) # Indices for winning roll sets   
    pwin_vec[i] <- sum(prolls[iwin])
  }
  return(pwin_vec)
}


get_pwin_matrix <- function(cards, probs, nrollsvec) {
  #' DESCRIPTION
  #'       Calls function pwin for multiple nrolls values, and returns results
  #' as a numeric matrix, with one row for each nrolls value.
  #' 
  #' ARGUMENTS
  #' cards:  numeric matrix representing possible bingo cards, such as returned
  #'   by function get_all_bingo_cards. Must have length(probs) rows and
  #'   nsimplex( p = length(probs), n = nrolls) columns. Each column represents
  #'   a bingo card.
  #' probs:  numeric vector of probabilities for the possible outcomes.
  #' nrollsvec:  numeric vector, giving numbers of rolls, i.e., numbers
  #'   of outcomes to generate. Each element is passed in turn to function pwin.
  #'
  #' VALUE
  #'       numeric matrix with length(nrollsvec) rows and ncol(cards) columns.
  #' Each row corresponds to a number of rolls, and each column to a card.
  #' The (i,j)th element gives the probability that the jth card will win in
  #' nrollsvec[i] rolls or fewer.
  #' 
  nr <- length(nrollsvec)
  pwin_mat <- matrix( 0, nrow = nr, ncol = ncol(cards) )
  for (i in seq_len(nr)) {
     pwin_mat[i, ] <- pwin(cards = cards, probs = probs, nrolls = nrollsvec[i])
  }
  return(pwin_mat)
}



# get_equivalence_class_mat(cards)

plot_card_prob_trajectories <- function(nrollsvec, 
                                        mat,
                                        cumulative, 
                                        initial_best_inds, 
                                        initial_best_cards,
                                        outcome_labels,
                                        legend_loc = "top",
                                        color_by_equiv_mat = FALSE,
                                        equiv_mat = NULL) {
#'
#' Plot wrapper to avoid repeating many plotting commands.
#' Plots trajectory of each card's probability of winning (or cumulative
#' probability of winning in n or fewer rolls), by rolls in nrollsvec. Must
#' specify whether matrix given contains probabilities of winning in n rolls
#' or probabilities of winning in n or fewer rolls, because this will affect
#' labeling of the plot. May need to adjust legend location manually. Also,
#' currently only written to account for at most 8 ties -- after that, colors
#' will begin to repeat themselves.
#' 
#' Optionally, specify a vector of names for the outcomes, to use in labeling.
#' For example, if the outcomes are 2, 3, 4, 5, and 6, need to specify
#' outcome_labels = 2:6; otherwise will start labeling them at 1.
#' 
#' If color_by_equiv_mat = F, plots lines in grey and highlights any best cards
#' in color. If T, colors lines by multinomial equivalence class cards with
#' same number of possible orderings of their entries (note that we really care
#' about orderings of rolls but for cardsize rolls, this is equivalent to 
#' number of orderings of numbers on card).
#'
  ###
  # Argument checking
  ###
  if (color_by_equiv_mat) {
    if (is.null(equiv_mat)) {
      stop("If color_by_equiv_mat = TRUE, equiv_mat must be given.")
    }
  }
  ###
  # Setup 
  ###
  colors <- c("blue", "orange", "cyan", "purple", "red", "green", "pink",
    "black")
  if (cumulative) {
    main <- paste("Cumulative probability of winning for each card as",
      "number of rolls increases")
    ylab <- "Probability of winning in n rolls or fewer"
  } else {
    main <- "Probability of winning in exactly n rolls for each card"
    ylab <- "Probability of winning in n rolls"
  }
  
  ###
  # Set up plot skeleton
  ###
  plot(nrollsvec,
       mat[, 1],
       type = "n",
       main = main,
       ylim = c(0, max(mat) * 1.05),
       xlab = "Number of rolls (n)",
       ylab = ylab,
       cex.main = 1.5,
       cex.lab = 1.1
  )
  
  ###
  # reference line
  ###
  abline(h = 1, lty = 2, col = "grey")
  
  
  if (color_by_equiv_mat) {
    # Color each card by equivalence class
    labels <- rownames(equiv_mat)
    num_classes <- length(labels)
    colors <- rep(colors, length.out = num_classes)
    # TODO: probably NOT necessary to apply this by column. Just do all at once?
    per_card_colors <- apply(equiv_mat, MARGIN = 2,
      FUN =  function(c) { colors[c] })
    # Plot cards
    for (i in seq_len(ncol(mat))) {
      lines(nrollsvec, mat[, i], col = per_card_colors[i], lty = 1, lwd = 1)
    }
    legend(x = legend_loc,
           legend = labels,
           col = colors,
           lty = 1,
           lwd = 2,
           cex = 1.1,
           title = "Multinomial equivalence classes"
    )
  } else {
    # ----- Plot that just colors and labels the best card(s)-----
    # Get colors for the best
    nties <- length(initial_best_inds)
    colors <- rep(colors, length.out = nties)
    
    # Plot line for each card's probability over rolls, coloring only ties
    num_best <- 0
    for (i in seq_len(ncol(mat))) {
      if (i %in% initial_best_inds) {
        num_best <- num_best + 1
        col <- colors[num_best]
        lty <- 2
        lwd <- 2
      } else {
        col <- "grey"
        lty <- 1
        lwd <- 1
      }
      lines(nrollsvec, mat[, i], col = col, lty = lty, lwd = lwd)  
    }
    
    # Create labels for legend -- convert from outcome count representation to
    # outcome set representation, i.e., convert to representation that shows
    # actual outcomes.
    if (is.vector(initial_best_cards)) { # no ties case
      outcome_inds <- x2u(initial_best_cards)
      outcome_rep <- outcome_labels[outcome_inds]
      legend <- paste(outcome_rep, collapse = ",")
    } else {
      outcome_inds <- apply(initial_best_cards, # convert representation
                            MARGIN = 2,
                            x2u)
      outcome_rep <- apply(outcome_inds,
                          MARGIN = 2,
                          function(i) { outcome_labels[i] })
      legend <- apply(outcome_rep, 
                     MARGIN = 2,
                     paste,
                     collapse = ",")
    }
    legend(x = legend_loc,
           legend = legend,
           col = colors[seq_len(nties)],
           lty = 2,
           lwd = 2,
           cex = 1.1,
           title = sprintf("Highest Probability Card at n = %d", min(nrollsvec))
    )
  }
} # end of overall function

