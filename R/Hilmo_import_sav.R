#' A Hilmo import .sav function
#'
#' This function allows you to import sav files in more tidy format and it creates ID column from lahtopaiva and tnro
#' @param filename the name of your file you wish to import
#' @keywords sav hilmo import
#' @export
#' @examples
#' Hilmo_import_sav()

Hilmo_import_sav <- function(filename) {

  library(tidyverse)
  library(haven)

  data <- read_sav(filename)

  data %>%
    select(SUKUP,IKA, TMPKOODI, KOODI1, lahtopvm, tulopvm, tnro, PALTU, KOKU) %>%
    mutate(ID=rep(NA)) %>%
    unite(ID, tnro, lahtopvm, sep="", remove=F) %>%
    mutate(ID = str_replace_all(ID,"/", "")) %>%
    mutate(pvm=as.Date(tulopvm, format= "%d/%m/%Y")) %>%
    separate(tulopvm, into=c("day","month","year"), sep="/") %>%
    select(-day, -month) %>%
    select(ID, tnro, SUKUP, IKA, PALTU, KOKU, TMPKOODI, KOODI1, pvm, year)

}
