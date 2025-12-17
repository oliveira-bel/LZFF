#helper-formatPed

helper_ped_ascii <- function(n = 10) {

  ped <- data.frame(
    id   = 1:n,
    sire = sample(c(0, 1:(n - 1)), n, replace = TRUE),
    dam  = sample(c(0, 1:(n - 1)), n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  list(ped = ped)
}

