

## functions related to SRB ##
#------------------------------------------------------------------------------

GetSRB <- function (b.male, # number of male birth
                    b.female, # number of female birth
                    lim.u.srb = NA, #20, # upper limit of SRB when no female birth
                    lim.l.srb = NA #0.05 # lower limit of SRB when no male birth
                    ) {
  srb <- ifelse(b.male == 0, lim.l.srb,
                ifelse(b.female == 0, lim.u.srb, b.male / b.female))
  
  return(srb)
}
#GetSRB(b.male = , b.female = , lim.u.srb = 20, lim.l.srb = 0.05)
#------------------------------------------------------------------------------

GetSRBfromAllandMale <- function (b.male, # number of male birth
                                  b.all # number of all birth
                                  ) {
  # in case number of all births is smaller than male births due to rounding
  b.female <- max(b.all - b.male, 0)
  srb <- GetSRB(b.male = b.male, b.female = b.female)
  
  return(srb)
}
#GetSRBfromAllandMale(b.male = , b.all = )
#------------------------------------------------------------------------------

GetSexSpecificBirthfromAllandSRB <- function (b.all, # total number of birth
                                              srb # SRB for the corresponding total birth
                                              ) {
  # get sex-specific birth given total birth and SRB
  prob.male <- srb / (1 + srb) #prob of giving a male live birth
  b.male <- b.all * prob.male
  b.female <- b.all - b.male
  
  result <- list(b.male = b.male, b.female = b.female)
  return(result)
}#end of GetSexSpecificBirthfromAllandSRB function
#------------------------------------------------------------------------------
SimSRBforCI <- function (b.male, # number of male birth
                         b.female, # number of female birth
                         n.sim = 1000 # number of simulations
) {
  ## get simulations of SRB given the probability of getting a male birth
  ## based on the observed female and male birth
  b.all <- ceiling(b.male + b.female)
  b.all[b.all < 1] <- 1
  
  # prob of having a male baby
  prob.male <- b.male / b.all
  prob.male[b.female == 0] <- 1
  prob.male[b.male == 0] <- 0.05
  
  seed.number <- round(b.female * 10 + b.male) + n.sim
  seed.number <- ifelse(seed.number > 10^8, seed.number / 1000, seed.number)
  set.seed(seed.number)
  
  male.n <- rbinom(n.sim, b.all, prob.male)
  
  srb.n <- rep(NA, n.sim)
  for (n in 1:n.sim) {
    srb.n[n] <- GetSRBfromAllandMale(b.male = male.n[n], b.all = b.all)
  }#end of n loop
  
  se.srb <- sd(srb.n, na.rm = TRUE)
  se.logsrb <- sd(log(srb.n), na.rm = TRUE)
  result <- list(se.srb = se.srb, se.logsrb = se.logsrb, sim.srb = srb.n)
  return(result)
}
#SimSRBforCI(b.male = , b.female = , n.sim = 1000)
#------------------------------------------------------------------------------

SimSRBforPI <- function (
  b.male, # number of male birth
  b.female, # number of female birth
  prob.male.exp = prob.male0, # prob of male newborns given SRB = 1.05
  n.sim = 1000 # number of simulations
) {
  ## get simulations of SRB given the probability of getting a male birth
  ## based on the observed female and male birth
  
  b.all <- max(ceiling(b.male + b.female), 1)
  
  seed.number <- round(b.female * 10 + b.male) + n.sim
  seed.number <- ifelse(seed.number > 10^8, seed.number / 1000, seed.number)
  set.seed(seed.number)
  
  male.exp.n <- rbinom(n.sim, b.all, prob.male.exp)
  srb.exp.n <- rep(NA, n.sim)
  for (n in 1:n.sim) {
    srb.exp.n[n] <- GetSRBfromAllandMale(b.male = male.exp.n[n], b.all = b.all)
  }#end of n loop
  
  return(srb.exp.n)
}
#SimSRBforPI(b.male = , b.female = , prob.male.exp = prob.male0, n.sim = 1000)
#------------------------------------------------------------------------------


