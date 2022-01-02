library(combinat)




get_all_bingo_cards <- function(noutcomes, card_size){
  #' noutcomes: number of possible outcomes from rolling the dice
  #' cardsize: number of spaces on bingo card - to be filled by selecting some outcomes
  #' returns: matrix of size noutcomes x total number of possible cards
  #' each column of matrix represents a possible bingo card. card is encoded with count
  #' for each outcome. 
  #' 
  #' For example, if noutcomes = 5, column 02210 represents a card with zero of outcome 1,
  #' two of outcome 2, two of outcome 3, and one of outcome 4, and zero of outcome 5.
  #' notice that this ignores the order in which outcomes appear on the bingo card. Thus
  #' the card with entries filled in 22334 is equivalent to the card with entries filled 23432 etc.
  #' 
  cards = xsimplex(p = noutcomes, n = card_size) 
  return(cards)
}




get_equivalence_class_mat <- function(cards){
  #sort each column to get single representation of cards with same groupings (same number of doubles etc.)
  sorted = apply(cards, MARGIN = 2, FUN = sort)

  
  #if there are not more outcomes than the alphabet, convert to a letters representation that
  #clearly distinguishes equivalence class rep from roll counts and bingo cards 
  if (nrow(cards) <= 26){ 
    sorted = apply(sorted, 2, function(col){letters[x2u(col)]})
    strings = apply(sorted, 2, paste, collapse = "")
  } else {
    strings = apply(sorted, 2, paste, collapse = ",") #add commas if using # rep
  }
  
  
  #identify the unique strings (equiv classes)
  classes = unique(strings)
  
  #get C x ncols(cards) matrix of T and F with T's encoding which class each card belongs to
  equiv_mat = t(sapply(classes, FUN = function(c){c == strings}, USE.NAMES = T))
  
  return(equiv_mat)
}







pwin <- function(cards, probs, nrolls){
  #' cards: a matrix of dimension length(probs) x nsimplex( p = length(probs), n = nrolls). Each column represents a card
  #' probs: a numeric vector of probabilities for each possible outcome
  #' nrolls: number of rolls to calculate win probabilities for.  If less than number of spots on card, will return vector of 0's for every card
  #' returns: a vector of probabilities for each card to win in nrolls rolls or fewer (minimum 5)

  ncards <- ncol(cards)
  noutcomes <- length(probs)
  if (nrow(cards) != noutcomes){stop("number of rows in cards matrix must equal number of outcomes (length of probs)")}
  card_size = unique(colSums(cards))
  if (length(card_size) > 2){stop("all bingo cards must have same size (column sums of cards matrix must all be the same)")}
  
  #if too few rolls to win, return vector of zeros
  if (nrolls < card_size){return(numeric(ncards))}
  
  #generate all possible rolls for the outcomes - rolls represented with counts for each outcome (no order).
  rolls <- xsimplex(p = noutcomes, n = nrolls)
  
  #to get probabilities of each outcome count vector, need to take into account multiple possible 
  #roll sequences that yield that outcome count. Do this by applying multinomial pdf and outcome probabilities
  prolls <- apply(rolls, MARGIN = 2, FUN = dmultinom, p = probs) 
  
  #for each card, check how many of the rolls will win for that card and add up the probabilities
  #of those rolls to get probability of winning with that card in nrolls or fewer
  #note: a roll is a 'winning roll' for a card if the count for each outcome is >= the count for that
  #outcome on the card. This says nothing about in which roll in the sequence the card wins 
  #so we have a cumulative probability
  pwin_vec <- numeric(ncards)
  for (i in seq_len(ncards)){
    iwin <- cards[,i] <= rolls #compare every roll to that card
    iwin <- colSums(iwin) == noutcomes #indices for winning rolls   
    pwin_vec[i] <- sum(prolls[iwin])
  }
  
  return(pwin_vec)
}


get_pwin_matrix <- function(cards, probs, nrollsvec){
  #' cards: a matrix of dimension length(probs) x nsimplex( p = length(probs), n = nrolls). Each column represents a card
  #' probs: a numeric vector of probabilities for each possible outcome
  #' nrollsvec: vector of nrolls arguments as in pwin() function
  #' returns: matrix of dimensions length(nrollsvec) x ncol(cards). Each row corresponds to
  #' a number of rolls, each column to a card, and each value is probability of winning in that
  #' many rolls or fewer for the given card
  nr = length(nrollsvec)
  pwin_mat = matrix(0,ncol = ncol(cards),  nrow = nr)
  for (i in seq_len(nr)){
     pwin_mat[i,] = pwin(cards = cards,
                 probs = probs,
                 nrolls = nrollsvec[i])
  }
  return(pwin_mat)
}





#' plot wrapper to avoid repeating many plotting commands
#' plots trajectory of each card's probability of winning (or cumulative probability
#' of winning in n or less) over rolls in nrollsvec. Must specify whether matrix given
#' has probabilities of winning in n rolls or probabilities of winning in n or fewer rolls
#' because this will affect labeling of the plot. May need to adjust legend location
#' manually. Also, currently only written to account for at most 8 ties -- after that, colors will 
#' begin to repeat themselves.
#' 
#' Optionally, specify a vector of names for each outcome - for use in labeling.
#' For example, if the outcomes are 2, 3, 4, 5, and 6, need to specify outcome_labels = 2:6
#' becuase otherwise will start labeling them at 1
#' 
#' if color_by_equiv_mat = F, plots lines in grey and highlights any best cards in color
#' if T, colors lines by multinomial equivalence class cards with same # of possible orderings of
#' their entries (note that we really care about orderings of rolls but for card_size rolls,
#' this is equivalent to # of orderings of numbers on card)


plot_card_prob_trajectories <- function(nrollsvec, 
                                        mat,
                                        cumulative, 
                                        initial_best_inds, 
                                        initial_best_cards,
                                        outcome_labels,
                                        legend_loc = "top",
                                        color_by_equiv_mat = F,
                                        equiv_mat = NULL){
  
  #argument checking
  if (color_by_equiv_mat){if (is.null(equiv_mat)){stop("If color_by_equiv_mat = T, equiv_mat must be given")}}
  
  
  #setup 
  colors = c("blue","orange","cyan","purple","red","green","pink","black")

  
  if (cumulative){
    main = "Cumulative probability of winning for each card as number of rolls increases"
    ylab = "Probability of winning in n rolls or fewer"
  }else{
    main = "Probabilities of winning in exactly n rolls for each card"
    ylab = "Probability of winning in n rolls"
  }
  
  #set up plot
  plot(nrollsvec,
       mat[,1],
       type = "n",
       main = main,
       ylim = c(0, max(mat)*1.05),
       xlab = "Number of rolls (n)",
       ylab = ylab,
       cex.main = 1.5,
       cex.lab = 1.1
  )
  
  #reference line
  abline(h = 1, lty = 2, col = "grey")
  
  
  if (color_by_equiv_mat){
    #color each card by class
    labels = rownames(equiv_mat)
    num_classes = length(labels)
    colors = rep(colors, length.out = num_classes)
    per_card_colors = apply(equiv_mat, 
                            MARGIN = 2, 
                            FUN =  function(c){colors[c]})
    #plot cards
    for (i in seq_len(ncol(mat))){
      lines(nrollsvec, mat[,i], col = per_card_colors[i],
            lty = 1,
            lwd = 1)
    }
    
    legend(x = legend_loc,
           legend = labels,
           col = colors,
           lty = 1,
           lwd = 2,
           cex = 1.1,
           title = "Multinomial equivalence classes"
    )
    
    
    
  }else{ #------------Plot which just colors and labels the best card(s)------------------------------
    #just get colors for the best
    nties = length(initial_best_inds)
    colors = rep(colors, length.out = nties)
    
    #plot line for each card's probability over rolls, coloring only ties
    num_best = 0
    for (i in seq_len(ncol(mat))){
      if (i %in% initial_best_inds){
        num_best = num_best + 1 #track # best cards plotted for color purposes
        col = colors[num_best]
        lty = 2
        lwd = 2
      }else{
        col = "grey"
        lty  = 1
        lwd = 1
      }
      
      lines(nrollsvec, mat[,i], col = col, lty = lty, lwd = lwd)  
    }
    
    #create labels for legend -- convert from outcome count representation to outcome set representation
    #i.e. convert to representation that shows actual outcomes
    if (is.vector(initial_best_cards)){ #no ties case
      
      outcome_inds = x2u(initial_best_cards)
      outcome_rep = outcome_labels[outcome_inds]
      legend = paste(outcome_rep, collapse = ",")
      
    } else {
      
      outcome_inds = apply(initial_best_cards, #convert representation
                            MARGIN = 2,
                            x2u)
      outcome_rep = apply(outcome_inds,
                          MARGIN = 2,
                          function(i){outcome_labels[i]})
      legend = apply(outcome_rep, 
                     MARGIN = 2,
                     paste,
                     collapse = ",")
    }
    legend(x = legend_loc,
           legend = legend,
           col = colors[1:nties],
           lty = 2,
           lwd = 2,
           cex = 1.1,
           title = sprintf("Highest Probability Card at n = %d", min(nrollsvec))
    )
  }
    
  
  

  
} #end of overall function







