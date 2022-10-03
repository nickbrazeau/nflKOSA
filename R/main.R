

#' @title Cost Function
cost <- function(propsch, winrecord = winrecord) {
  # join together for binomial
  tm1 <- winrecord %>%
    dplyr::rename(homemascot = mascot)
  tm2 <- winrecord %>%
    dplyr::rename(oppmascot = mascot)
  tmp <- dplyr::left_join(propsch, tm1, by = "homemascot") %>%
    dplyr::left_join(., tm2, by = "oppmascot")
  # out
  wk <- log((1 - tmp$wins.x/(tmp$wins.x + tmp$wins.y))) # 1 minus for minimization instead of maximization
  wi <- rev(1:18) # weights for more cost of losing early (and getting knocked out)
  return(sum(wk*wi))
}


#' @title initial proposal
init_proposal <- function(schs) {
  newsch <- data.frame(opp = c("a", "a")) # just for while to work
  while(any(duplicated(newsch$opp))) { # can't have repeat teams in KO
    keep <- tapply(1:nrow(schs), factor(schs$wk, levels = 1:18), function(x){sample(x,1)})
    newsch <- schs[keep, ]
  }
  return(newsch)
}

#' @title subsequent proposals
#' @details allow to propose the same value again...
proposal <- function(currsch, schs){
  # partition into 3/18 weeks to change
  wkschng <- sample(1:18, 3)
  # init
  newsch <- currsch
  newsch$opp[wkschng] <- "a" # just for while to work
  while(any(duplicated(newsch$opp))) { # can't have repeat teams in KO
  tmp <- schs %>%
    dplyr::filter(wk %in% wkschng)
  fill <- tmp[tapply(1:nrow(tmp), factor(tmp$wk), function(x){sample(x,1)}), ]
  # now curr is new
  newsch[wkschng,] <- fill
  }
  return(newsch)
}

#' @title simulated annealing
#' @export
KOSA_sim_anneal <- function(schs = schs,
                            winrecord = winrecord,
                            iters = 1e3, coolingB = 1e-3, initTemp = 1) {

  # STORAGE
  costrun <- rep(NA, iters)
  # INIT
  currpos <- init_proposal(schs)
  Temp <- initTemp
  currcost <- cost(currpos, winrecord = winrecord)
  # RUN
  for(i in 1:iters) {
  # PROPOSE
    newpos <- proposal(currpos, schs = schs)
  # CALC COST
    newcost <- cost(newpos, winrecord = winrecord)
  # ACCEPT MOVE?
    u <- runif(1)
    p <- min(
             exp(-(newcost-currcost))/Temp,
             1)
    accept <- u <= p

  ## UPDATES
    # update current position and cost
    currpos <- if(accept){newpos}else{currpos}
    currcost <- if(accept){newcost}else{currcost}
    costrun[iters] <- currcost
    # update temp
    Temp <- Temp/(1  +Temp*coolingB)
  }
  # out
  picks <- currpos %>%
    dplyr::mutate(wk = as.numeric(wk)) %>%
    dplyr::arrange(wk) %>%
    dplyr::select(c("wk", "homemascot", "oppmascot")) %>%
    dplyr::mutate(homemascot = stringr::str_to_sentence(homemascot),
                  oppmascot = stringr::str_to_sentence(oppmascot))
  colnames(picks) <- c("week", "winner", "loser")

  out <- list(
    picks = picks,
    costrun = costrun
  )

  return(out)
}
