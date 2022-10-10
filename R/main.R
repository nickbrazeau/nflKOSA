

#' @title Cost Function
#' @param propsch list; Proposed X-week scheduled to evaluate
#' @param winrecord dataframe; Win/Loss record for teams
#' @param Weights numeric vector; Weights to place on each week (may want to upweight early versus late weeks)
cost <- function(propsch, winrecord = winrecord, weights = rev(1:18)) {
  # coding choice for win record
  winrecord <- winrecord %>%
    dplyr::mutate(wins = dplyr::case_when(yr == 2021 ~ wins*10,
                                          yr == 2020 ~ wins*9,
                                          yr == 2019 ~ wins*8,
                                          yr == 2018 ~ wins*7,
                                          yr == 2017 ~ wins*6,
                                          yr == 2016 ~ wins*5,
                                          yr == 2015 ~ wins*4,
                                          yr == 2014 ~ wins*3,
                                          yr == 2013 ~ wins*2,
                                          yr == 2012 ~ wins*1))

  # join together for binomial
  tm1 <- winrecord %>%
    dplyr::rename(winmascot = mascot)
  tm2 <- winrecord %>%
    dplyr::rename(losemascot = mascot)
  tmp <- dplyr::left_join(propsch, tm1, by = "winmascot") %>%
    dplyr::left_join(., tm2, by = "losemascot")
  # out
  wk <- log((1 - tmp$wins.x/(tmp$wins.x + tmp$wins.y))) # 1 minus for minimization instead of maximization
  wi <- weights # weights for more cost of losing early (and getting knocked out)
  return(sum(wk*wi))
}


#' @title initial proposal
#' @inheritParams cost
init_proposal <- function(propsch) {
  newsch <- data.frame(lose = c("a", "a")) # just for while to work
  while(any(duplicated(newsch$lose))) { # can't have repeat teams in KO
    keep <- tapply(1:nrow(propsch), factor(propsch$wk, levels = 1:18), function(x){sample(x,1)})
    newsch <- propsch[keep, ]
  }
  return(newsch)
}

#' @title subsequent proposals
#' @inheritParams cost
#' @param schs list; all possible NFL schedules
#' @param weeks numeric vector; NFL weeks to consider in SA
#' @param rpl numeric; Number of weeks to consider to replace in SA
#' @details Note, replacement function allows for same schedule to be proposed again (in expectation)
proposal <- function(currsch, schs, weeks = 1:18, rpl = 3){
  # partition into X/N weeks to change
  wkschng <- sample(weeks, rpl)
  # init
  newsch <- currsch
  newsch$lose[wkschng] <- "a" # just for while to work
  while(any(duplicated(newsch$lose))) { # can't have repeat teams in KO
    tmp <- schs %>%
      dplyr::filter(wk %in% wkschng)
    fill <- tmp[tapply(1:nrow(tmp), factor(tmp$wk), function(x){sample(x,1)}), ]
    # now curr is new
    newsch[wkschng,] <- fill
  }
  return(newsch)
}

#' @title simulated annealing
#' @param iters numeric; iterations for SA
#' @param coolingB numeric; cooling factor for "slow decrease" or logarithmic cooling strategy for SA
#' @param initTemp numeric; initial temperature in SA
#' @inheritParams proposal
#' @inheritParams cost
#' @export
KOSA_sim_anneal <- function(schs = schs,
                            winrecord = winrecord,
                            weeks = 1:18, rpl = 3,
                            weights = rev(1:18),
                            iters = 1e3, coolingB = 1e-3, initTemp = 1) {

  # STORAGE
  costrun <- rep(NA, iters)
  # INIT
  currpos <- init_proposal(schs)
  Temp <- initTemp
  currcost <- cost(currpos, winrecord = winrecord, weights = weights)
  # RUN
  for(i in 1:iters) {
    # PROPOSE
    newpos <- proposal(currpos, schs = schs, weeks = weeks, rpl = rpl)
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
    dplyr::select(c("wk", "winmascot", "losemascot")) %>%
    dplyr::mutate(winmascot = stringr::str_to_sentence(winmascot),
                  losemascot = stringr::str_to_sentence(losemascot))
  colnames(picks) <- c("week", "winner", "loser")

  out <- list(
    picks = picks,
    costrun = costrun
  )

  return(out)
}
