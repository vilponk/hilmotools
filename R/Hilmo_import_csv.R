
#' A Hilmo import .csv function
#'
#' This function allows you to import csv files in more tidy format and it creates ID column from lahtopaiva and tnro
#' @param filename the name of your file you wish to import
#' @keywords csv hilmo import
#' @export
#' @examples
#' Hilmo_import_csv()


Hilmo_import_csv <- function(filename) {

  library(tidyverse)

  data <- read_delim(filename,";", escape_double = FALSE, trim_ws = TRUE)

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
