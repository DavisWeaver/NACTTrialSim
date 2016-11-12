trialnumber <- NULL
fiveyearsurvival <- NULL
numberofsimulations <- 1000

for(i in 1:numberofsimulations) {
  n <- i
  survive5years <- PDSSim()
  trialnumber <- c(trialnumber, n)
  fiveyearsurvival <- c(fiveyearsurvival, survive5years)
}