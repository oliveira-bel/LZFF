#helper-formatPed

check_ped_ascii <- function(pedObj) {
  dadosPedTemp <- sapply(pedObj$ped, paste0, collapse = " ")
  stopifnot(all(grepl("^[ -~]+$", dadosPedTemp)))
  invisible(TRUE)
}

# Helper: pedigree (não ASCII)
ped_ascii_bad <- function() {
  list(
    ped = data.frame(
      id   = c(3, 4),
      sire = c(9, 10),
      dam  = c("ç", "0"),
      stringsAsFactors = FALSE
    )
  )
}


