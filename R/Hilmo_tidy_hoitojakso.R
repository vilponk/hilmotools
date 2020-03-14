#' A Hilmo hoitojakso tidy function
#'
#' This function allows you to change the data from long to wide format by adding all diagnose codes into one row.
#'
#' Every row represents one hoitojakso
#' @param dataname the name of your data you are working with
#' @keywords  hilmo tidy hoitojakso
#' @export
#' @examples
#' Hilmo_tidy_hoitojakso()
#'
Hilmo_tidy_hoitojakso <- function(dataname) {

  dataname %>%
    group_by(ID) %>%
    summarise(tnro=toString(unique(tnro)),
              SUKUP=toString(unique(SUKUP)),
              IKA=toString(unique(IKA)),
              KOKU=toString(unique(KOKU)),
              TMPKOODI=list(unique(TMPKOODI)),
              KOODI1 = list(unique(KOODI1)),
              pvm= toString(unique(pvm)),
              year= toString(unique(year)),
              incluusio= toString(unique(incluusio))) %>%
    mutate(SUKUP=factor(SUKUP, levels=c(1,2), labels=c("Male", "Female")),
           IKA=as.numeric(IKA),
           pvm=as.Date(pvm, format= "%Y-%m-%d"),
           year=as.factor(year)) %>%
  arrange(pvm)


}
