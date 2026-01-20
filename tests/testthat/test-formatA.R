test_that("formatA is working correctly", {
  # Simulating rrcData output
  simudata<-data.frame(
    id = 1:3,
    trait1 = c(10.5, 20.0, 15.2),
    trait2 = c(5.1, 8.9, 12.3)
  )

  temp_file<-tempfile(fileext = ".txt")

  formatA(udata = simudata, of = temp_file, EoL = "\n")

  # Reading the Format A output
  dataRead <- readLines(temp_file)

  # checking if the file exists
  expect_true(file.exists(temp_file))

  # Checking the content
  expect_equal(length(dataRead), 3)
  expect_equal(dataRead[1], "1 10.5 5.1")
  expect_equal(dataRead[2], "2 20 8.9")
  expect_equal(dataRead[3], "3 15.2 12.3")

  # Clear the temporary file
  unlink(temp_file)
})
