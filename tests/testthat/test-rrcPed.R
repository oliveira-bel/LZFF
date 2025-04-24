rawPedigree<-read.table(testthat::test_path("dataForTests", "ped.txt"),
                        sep = " ", header = TRUE, strip.white = FALSE,
                        na.strings = c("", " ", "NA"), fill = TRUE)

test_that("Date conversion is working for text files", {
  rawPedigree<-data.frame(rawPedigree[,c(2, 4, 3, 1)])
  expect_equal(class(rawPedigree[,4]), "character")
  rawPedigree[,4]<-as.Date(rawPedigree[,4])
  expect_equal(class(rawPedigree[,4]), "Date")
  })

test_that("Removal of duplicated registers is working", {
  expect_equal(nrow(rawPedigree), 6)
  betterPedigree<-unique(rawPedigree)
  expect_equal(nrow(betterPedigree), 5)
})

test_that("Checking if the same code is present in sire and dam columns is working",{
  rawPedigree[1,4]<-3487
  bad<-match(rawPedigree[,3], rawPedigree[,4], incomparables = NA)
  expect_error(if (!all(is.na(bad))){
    stop("Error: An animal appears in both columns: sire and dam. Please, check your pedigree.
         Function aborted!")
  })
})
