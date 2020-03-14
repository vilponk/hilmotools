#' A date difference calculator (Hilmo)
#'
#' This function generates necessary columns and calculates the difference between the primary operation and secondary operation
#'
#' incluusio: Includion variable, "Levy" as primary operation and "Poisto" as secondary.
#' hoitojakso_n: Number of hospital care periods
#' oper_seq: sequence of the operations (1 primary, 2 secondary)
#' levy_pos and poisto_pos: position of the primary (levy) or secondary (poisto) operations in Incluusio column
#' pvm1:pvmx: Operation dates from pvm list-column
#' levy_date: Date of primary operation
#' poisto_date: Date of secondary operation
#' datediff: Date difference between the primary and secondary operations
#' @param df the name of your data you are working with
#' @param x The number of maximum number of treatment (hoitojakso_n)
#' @keywords  date difference calculator hilmo
#' @export
#' @examples

datecalc <- function(df, x) {

data1 <- df %>%
  mutate(oper_seq = map(df$incluusio, match, table=c("Levy"), nomatch=2))


data1$levy_pos <- rep(NA)
data1$poisto_p <- rep(NA)

for (i in seq_along(data1$oper_seq)) {
  data1$levy_pos[[i]] <- match(c(1), data1$oper_seq[[i]])
  data1$poisto_p[[i]] <- match(c(2), data1$oper_seq[[i]])
}


data1 <- data1 %>%
  mutate(levy_pos = as.numeric(levy_pos),
         poisto_p = as.numeric(poisto_p),
         hoitojakso_n = as.numeric(hoitojakso_n))

data1 <- data1 %>%
  mutate(pos_ok = case_when(levy_pos <= poisto_p |
                              hoitojakso_n == 1 |
                              is.na(levy_pos) |
                              is.na(poisto_p) |
                              levy_pos==hoitojakso_n ~ 1,
                            TRUE ~ 0))

wrapVector <- function(x, n) {
  stopifnot(n <= length(x))
  if (n == length(x))
    return(rep(NA, n))
  else
    return(c(x[(n+1):length(x)], rep(NA, n)))
}

matchSequence <- function(seq, vec) {
  matches <- seq[1] == vec
  if (length(seq) == 1) return(which(matches))
  for (i in 2:length(seq)) {
    matches <- cbind(matches, seq[i] == wrapVector(vec, i - 1))
  }
  which(rowSums(matches) == i)
}

data1$poisto_pos <- rep(NA)

for (i in seq_along(data1$oper_seq)) {
  data1$poisto_pos[[i]] <- ifelse(data1$pos_ok[[i]] == 0, matchSequence(c(1, 2), data1$oper_seq[[i]]) + 1, paste(data1$poisto_p[[i]]))
}

library(data.table)
setDT(data1)[, paste0("pvm", 1:x) := tstrsplit(pvm, ", ")]

data1 <-  data1 %>%
  mutate_at(vars(matches("pvm")), as.Date)

data1 <- data1 %>%
  mutate(time1 = ifelse(is.na(levy_pos), paste(""), paste("pvm", levy_pos, sep="")),
         time2 = ifelse(poisto_pos=="NA", paste(""), paste("pvm", poisto_pos, sep="")))

data1[["id"]] <- seq_len(nrow(data1))

data2 <- data1 %>%
  gather("column", "levy_date", pvm1:pvm9) %>%
  filter(column == time1) %>%
  select(id, levy_date) %>%
  right_join(data1, by = "id")

data3 <- data2 %>%
  gather("column", "poisto_date", pvm1:pvm9) %>%
  filter(column == time2) %>%
  select(id, poisto_date) %>%
  right_join(data2, by = "id") %>%
  select(-id, -poisto_p, -pos_ok, -time1, -time2) %>%
  select(-levy_date, levy_date) %>%
  select(-poisto_date, poisto_date)

data4 <- data3 %>%
  mutate(datediff = difftime(poisto_date, levy_date, units="days")/365) %>%
  mutate(datediff = case_when(datediff >=0 ~ paste(datediff),
                              TRUE ~ paste(NA)))

}
