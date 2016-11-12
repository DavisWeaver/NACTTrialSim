
PDSSim <- function(survive5years.prob = 0.2077, n = 1000){
  
  ###Define Hazard ratios associated with each surgical outcome ###
  
  suboptimal.hr <- 1.093567
  complete.hr <- 0.900901
  optimal.hr <- 0.792
  
  ###Define Probabilities of each surgical outcome occuring###
  
  suboptimal.prob <- 0.56
  optimal.prob <- 0.24
  complete.prob <- 0.2
  
  ### Define vectors for hazard ratios and probabilities ###
  
  hazardratios <- c(suboptimal.hr, optimal.hr, complete.hr)
  probabilities <- c(suboptimal.prob, optimal.prob, complete.prob)
  
  ### Define 1 year rate of death ###
  
  survive1year.prob <- survive5years.prob ^ (1/5)
  die1year.prob <- 1 -  survive1year.prob
  die1year.rate <- -log(1-die1year.prob)
  
  ### Initialize output vectors and define samplesize for Trial ###
  
  survive.vector <- NULL
  ones <- c(1,1,1,1,1)
  samplesize <- c(1:n)
  
  
  ##Initiate Trial ##
  
  for(i in samplesize) {
    
    patientnumber = i
    hazard.ratio <- sample(hazardratios, 1, replace = FALSE, prob = probabilities)
    
    die.rate <- die1year.rate*hazard.ratio
    die.prob <- 1-exp(-die.rate)
    survive.prob <- 1 - die.prob
    
    survive <- rbinom(5, 1, prob = survive.prob)
    
    ###Populate Output Vectors ###
    
    if(isTRUE(all.equal(ones, survive))) {
      survive.vector <- c(survive.vector, 1)
    }
    if(!isTRUE(all.equal(ones, survive))) {
      survive.vector <- c(survive.vector, 0)
    }
    
    
    
  }
  sum(survive.vector)/length(samplesize)
}

