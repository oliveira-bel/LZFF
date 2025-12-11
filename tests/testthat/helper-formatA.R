g<-sample(paste0("g", 1:3), 10, replace = TRUE)
s<-sample(c(0, 1), 10, replace = TRUE)
testData<-data.frame(id = 1:10, sex = s, gc = g, t1 = rnorm(10),
                  t2 = rnorm(10, 5), t3 = rnorm(10, 100, 12))

formatA(testData)
testData1<-rrcData(local = "formatted_file", s = " ", d = ".", h = FALSE, colsPed = 1,
                   colsTraits = c(4:6))
