#helper-rrcData
helper_recode_data <- function() {

  dados <- data.frame(
    id    = 1:6,
    sire  = c(0, 1, 1, 2, 2, 0),
    sex   = c("M", "F", "F", "M", "M", "F"),
    grupo = c("A", "A", "B", "B", "C", "C"),
    trait = c(10.2, 11.5, 10.2, 9.8, 11.5, 9.8))
  list( dados = dados,colsPdg = c(1, 2),colsTrts  = 5)}

recode_dados <- function(dados, colsPdg, colsTrts) {

  for (i in 1:length(dados)) {
    if (all(i != c(colsPdg, colsTrts))) {
      tempData <- unique(dados[[i]])
      codes <- 1:length(tempData)
      mapa <- data.frame(tempData, codes)
      names(mapa) <- c("Original.Codes", "Recode")
      index <- match(dados[[i]], mapa$Original.Codes)
      dados[, i] <- mapa$Recode[index]
    }
  }

  dados
}

