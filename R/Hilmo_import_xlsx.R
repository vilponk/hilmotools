#' A Hilmo import .xlsx function
#'
#' This function allows you to import xlsx files in more tidy format and it creates ID column from lahtopaiva and tnro
#'
#' Remember that this import function has different order in the lahtopvm column and if you have problems,
#' it should be changed depending on the data
#' @param filename the name of your file you wish to import
#' @keywords xlsx hilmo import
#' @export
#' @examples
#' Hilmo_import_xlsx()

Hilmo_import_xlsx <- function(filename) {

  library(readxl)
  library(tidyverse)

  data <- read_excel(filename)

    data %>%
    select(SUKUP,IKA, TMPKOODI, KOODI1, lahtopvm, tulopvm, tnro, PALTU, KOKU) %>%
    mutate(ID=rep(NA)) %>%
    unite(ID, tnro, lahtopvm, sep="", remove=F) %>%
    mutate(ID = str_replace_all(ID,"-", "")) %>%
    mutate(pvm=as.Date(tulopvm, format= "%d/%m/%Y")) %>%
    separate(tulopvm, into=c("day","month","year"), sep="/") %>%
    select(-day, -month) %>%
    select(ID, tnro, SUKUP, IKA, PALTU, KOKU, TMPKOODI, KOODI1, pvm, year)
}
