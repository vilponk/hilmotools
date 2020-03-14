#' A Hilmo tidy function
#'
#' This function allows you to change the data from long to wide format
#'
#' Every row represents one patient
#' @param dataname the name of your data you are working with
#' @keywords  hilmo tidy
#' @export
#' @examples
#' Hilmo_tidy()

Hilmo_tidy <- function(dataname) {

  dataname%>%
    group_by(tnro) %>%
    group_by(tnro) %>%
    summarise(SUKUP=toString(unique(SUKUP)),
              IKA=list(IKA),
              KOKU=toString(unique(KOKU)),
              TMPKOODI=list(TMPKOODI),
              KOODI1 = list(KOODI1),
              pvm=toString(as.Date(pvm, format= "%Y-%m-%d")),
              year=list(toString(year)),
              incluusio=list(toString(incluusio))) %>%
  mutate(hoitojakso_n = lengths(incluusio),
         KOKU=as.numeric(KOKU))


}
