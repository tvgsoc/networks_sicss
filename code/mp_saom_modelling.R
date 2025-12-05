###
### Please run the whole chunk of the previous file "Data cleaning.R". before opening this file
###

library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(tidyr)
library(igraph)
library(RSiena)
library(tibble)

setwd("/Users/huijia/Desktop/Dissertation/code")


siena07ToConvergence <- function(alg, dat, eff, threshold = 0.25, maxRuns = 10, ...) {
  require(RSiena)
  ans <- siena07(alg, data = dat, effects = eff, ...)
  run <- 1
  while (ans$tconv.max > threshold && run < maxRuns) {
    message("Re-running, tconv.max = ", round(ans$tconv.max, 3))
    ans <- siena07(alg, data = dat, effects = eff, prevAns = ans, ...)
    run <- run + 1
  }
  ans
}

###### --- Function for M1 (Base: density + recip) --- #######
prepare_and_run_base <- function(net) {
  dep <- sienaDependent(net)
  dat <- sienaDataCreate(dep)
  eff <- includeEffects(getEffects(dat), density, recip)
  deg_cap <- max(apply(net, 3, function(m) max(rowSums(m)))) + 5
  
  dep_name <- names(dat$depvars)[1]  # directly get the dependent variable name
  
  alg <- sienaAlgorithmCreate(
    n3 = 3000, firstg = 0.01,
    MaxDegree = setNames(deg_cap, dep_name)
  )
  siena07ToConvergence(alg, dat, eff)
}

###### --- Function for M2 (density + recip + samePARTY) --- ######
prepare_and_run_party <- function(net, party = NULL, covariates_df = NULL,
                                  id_col = "id", party_col = "party"){
  
  dep <- sienaDependent(net, type = "oneMode")
  actors <- dimnames(net)[[1]]
  
  if (is.null(party)) {
    stopifnot(!is.null(covariates_df))
    party <- covariates_df[[party_col]][ match(actors, covariates_df[[id_col]]) ]
  }
  
  party_cov <- coCovar(as.integer(factor(party)))
  dat <- sienaDataCreate(dep, party = party_cov)
  eff <- includeEffects(getEffects(dat), density, recip, sameX, interaction1 = "party")
  dep_name <- names(dat$depvars)[1]
  maxdeg <- max(apply(net, 3, function(m) max(rowSums(m)))) + 4
  alg <- sienaAlgorithmCreate(n3 = 3000, firstg = 0.02, MaxDegree = setNames(maxdeg, dep_name))
  siena07ToConvergence(alg, dat, eff)
  
}


###### --- Function for Cross-layer coevolution: mentions <-> retweets --- ######
prepare_and_run_coevolution <- function(net_rt, net_men, n3 = 3000, firstg = 0.02) {
  rt_dep  <- sienaDependent(net_rt,  type = "oneMode")
  men_dep <- sienaDependent(net_men, type = "oneMode")
  dat <- sienaDataCreate(rt = rt_dep, men = men_dep)
  
  # Effects: baseline + cross-layer
  eff <- getEffects(dat)
  eff <- includeEffects(eff, name = "rt",  density, recip)
  eff <- includeEffects(eff, name = "men", density, recip)
  eff <- includeEffects(eff, name = "rt",  crprod, interaction1 = "men")  
  eff <- includeEffects(eff, name = "men", crprod, interaction1 = "rt")   
  
  # Algorithm
  dep_names  <- names(dat$depvars)       
  maxdeg_rt  <- max(apply(net_rt,  3, function(m) max(rowSums(m)))) + 6
  maxdeg_men <- max(apply(net_men, 3, function(m) max(rowSums(m)))) + 6
  alg <- sienaAlgorithmCreate(
    n3 = n3, firstg = firstg,
    MaxDegree = setNames(c(maxdeg_rt, maxdeg_men), dep_names)
  )
  
  siena07ToConvergence(alg, dat, eff)
}
#-----------------------------------------------------------------------------------------

# --- Mentions layer ---
MEN_M1 <- prepare_and_run_base(mention_net)
MEN_M2 <- prepare_and_run_party(
  net = mention_net,
  covariates_df = covariates,
  id_col = "username",   
  party_col = "party"   
)
MEN_M1
MEN_M2

# --- Retweet layer ---
RT_M1  <- prepare_and_run_base(retweet_net)
RT_M2 <- prepare_and_run_party(
  net = retweet_net,
  covariates_df = covariates,
  id_col = "username",
  party_col = "party"
)
RT_M1
RT_M2

# --- Cross-layer coevolution (mentions ↔ retweets) ---
COEV_RT_MEN <- prepare_and_run_coevolution(retweet_net, mention_net)
COEV_RT_MEN 

save(MEN_M1, file = "MEN_M1_results.RData")
save(MEN_M2, file = "MEN_M2_results.RData")
save(RT_M1, file = "RT_M1_results.RData")
save(RT_M2, file = "RT_M2_results.RData")
save(COEV_RT_MEN, file = "COEV_RT_MEN_results.RData")

#-----------------------------------------------------------------------------------------
### Test for time heterogeneity
sienaTimeTest(MEN_M2, effects = 3) 
sienaTimeTest(MEN_M2, effects = 2) 


sienaTimeTest(RT_M2, effects = 3) 
sienaTimeTest(RT_M2, effects = 2) 

####### === estimate new models with time dummies === ######

#### RT full --------- 
# [M2 above was built with memory-saving and score-test settings that remove information and change how effects are treated
# Time Dummy SAOMs need a full, ordinary fit with all simulation statistics
# So I had to re-estimate M2_2(same parater as M2) outside the helper (no lessMem, no special test/fix settings), and then use M2_2 
# as prevAns for the time-dummy model.
retweet_dep <- sienaDependent(retweet_net, type = "oneMode")
rt_actors   <- dimnames(retweet_net)[[1]]
rt_party_vec <- covariates$party[ match(rt_actors, covariates$username) ]
rt_party_cov <- coCovar(as.integer(factor(rt_party_vec)))
rt_data <- sienaDataCreate(retweet_dep, party = rt_party_cov)

rt_eff_1 <- getEffects(rt_data)
rt_eff_1 <- includeEffects(rt_eff_1, density, recip, sameX, interaction1 = "party")
rt_dep_name <- names(rt_data$depvars)[1]
rt_maxdeg   <- max(apply(retweet_net, 3, function(m) max(rowSums(m)))) + 4
rt_alg <- sienaAlgorithmCreate(n3 = 3000, firstg = 0.01, MaxDegree = setNames(rt_maxdeg, rt_dep_name))

RT_M2_2 <- siena07ToConvergence(rt_alg, rt_data, rt_eff_1)
RT_M2_2 

#### RT: recip + sameX(party) time-heterogeneous -----------
rt_eff_td <- getEffects(rt_data)
rt_eff_td <- includeEffects(
  rt_eff_td,
  density, recip,
  sameX, interaction1 = "party"
)
# time dummies for sameX(party): periods 2 and 3
rt_eff_td <- includeTimeDummy(
  rt_eff_td,
  sameX,
  interaction1 = "party",
  timeDummy    = "2 3"
)
# time dummies for reciprocity: periods 2 and 3
rt_eff_td <- includeTimeDummy(
  rt_eff_td,
  recip,
  timeDummy    = "2 3"
)

rt_time_alg <- sienaAlgorithmCreate(
  projname  = "RT_time",
  n3        = 3000,
  firstg    = 0.02,
  MaxDegree = setNames(rt_maxdeg, rt_dep_name)
)

RT_time <- siena07(
  rt_time_alg,
  data       = rt_data,
  effects    = rt_eff_td,
  prevAns    = RT_M2_2,
  returnDeps = TRUE,
  batch      = TRUE
)

RT_time




#### MEN full ----- (Same situation as retweet)
mention_dep <- sienaDependent(mention_net, type = "oneMode")
men_actors  <- dimnames(mention_net)[[1]]
men_party_vec <- covariates$party[ match(men_actors, covariates$username) ]
men_party_cov <- coCovar(as.integer(factor(men_party_vec)))

men_data <- sienaDataCreate(mention_dep, party = men_party_cov)
men_eff_1 <- getEffects(men_data)
men_eff_1 <- includeEffects(men_eff_1, density, recip, sameX, interaction1 = "party")
men_dep_name <- names(men_data$depvars)[1]
men_maxdeg   <- max(apply(mention_net, 3, function(m) max(rowSums(m)))) + 4
men_alg <- sienaAlgorithmCreate(n3 = 3000, firstg = 0.01, MaxDegree = setNames(men_maxdeg, men_dep_name))

MEN_M2_2 <- siena07ToConvergence(men_alg, men_data, men_eff_1)


#### MEN: recip constant; sameX(party) time-heterogeneous ----
men_eff_td <- getEffects(men_data)
men_eff_td <- includeEffects(
  men_eff_td,
  density, recip,
  sameX, interaction1 = "party"
)
men_eff_td <- includeTimeDummy(
  men_eff_td,
  sameX,
  interaction1 = "party",
  timeDummy    = "2 3"
)
men_time_alg <- sienaAlgorithmCreate(
  projname  = "MEN_time",
  n3        = 3000,
  firstg    = 0.02,
  MaxDegree = setNames(men_maxdeg, men_dep_name)
)
MEN_time <- siena07ToConvergence(
  men_time_alg,
  men_data,
  men_eff_td,
  prevAns = MEN_M2_2,  
  batch   = TRUE
)
MEN_time







