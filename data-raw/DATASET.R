## .................................................................................
## Purpose: Scrape NFL Data
## Author: Nick Brazeau
## .................................................................................
library(tidyverse)
library(rvest)
library(RCurl)

#......................
# get all teams
#......................
# thanks to https://www.profootballnetwork.com/printable-nfl-schedule/ for putting
# together a printer accessible version of the NFL schedule
tmsdecoder <- readr::read_csv("data-raw/team_decoder.csv")
losemascot <- tmsdecoder %>%
  dplyr::select(c("mascot", "schlet")) %>%
  dplyr::rename(lose = schlet,
                losemascot = mascot)
#............................................................
# tidy schedule
#...........................................................
schs <- readr::read_csv("data-raw/2022nflsch.csv")
schs <- schs %>%
  tidyr::pivot_longer(., cols = -c("Team"), names_to = "wk", values_to = "lose") %>%
  dplyr::rename(team = Team) %>%
  dplyr::mutate(schlet = tolower(team),
                lose = tolower(stringr::str_replace_all(string = lose, pattern = "vs.\\s|@|\\s|", replacement = "")),
                wk = stringr::str_replace_all(string = wk, pattern = "Wk\\s", replacement = "")
                ) %>%
  dplyr::filter(lose != "bye") %>%
  dplyr::left_join(., tmsdecoder, by = "schlet") %>%
  dplyr::rename(win = schlet,
                winmascot = mascot) %>%
  dplyr::select(c("wk", "win", "winmascot", "lose")) %>%
  dplyr::left_join(., losemascot, by = "lose")
#......................
# need to duplicate/switch who is "win" because we do wins based on X pos
#......................
schsreverse <- schs
colnames(schsreverse) <- c("wk", "lose", "losemascot", "win", "winmascot")
schs <- dplyr::bind_rows(schs, schsreverse)

usethis::use_data(schs, overwrite = T)


#............................................................
# scrape records
#...........................................................
get_wins <- function(tms, yrs) {
  # be kind
  Sys.sleep(3)
  # url
  base <- paste0("https://www.pro-football-reference.com/teams/", tms, "/", yrs, ".htm")
  pg <- rvest::read_html(base)
  # get table
  bstbl <- pg %>% rvest::html_nodes("table") %>%
    .[2] %>%
    rvest::html_table(fill = TRUE) %>%
    as.data.frame()
  wins <- sum(bstbl[,6] == "W")
  # out
  return(data.frame(pfrlet = tms, yr = yrs, wins = wins))
}

# run it
winrecord <- mapply(get_wins,
                    yrs = sort(rep(2012:2021, length(tmsdecoder$pfrlet))),
                    tms = tmsdecoder$pfrlet,
                    SIMPLIFY = F)

# making a coding decision here to weight recent records
winrecord <- winrecord %>%
  dplyr::bind_rows() %>%
  dplyr::group_by(pfrlet) %>%
  dplyr::summarise(wins = sum(wins)) %>%
  dplyr::left_join(., tmsdecoder, by = "pfrlet") %>%
  dplyr::select(-c("espnlet", "site", "pfrlet"))

# save out
usethis::use_data(winrecord, overwrite = T)
