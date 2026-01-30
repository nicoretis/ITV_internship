#Run immutable scenario 
source("./no_plasticity.R")
source("./other_functions.R")

set.seed(42)

#case1=immutable trait during their life, no sd (all animals equal)
case1<-run_simulations(immutable_scenario, 500, #number of simulations
                       100, #years of simulations
                       0, 1, #mean temp, sd temp
                       0, 1, 0, #intercept trait, slope trait, sd trait
                       0.28, 1, 0) #intercept demo, slope demo, sd demo
saveRDS(case1, "case1.rds")

#case2= immutable trait during their life, small standar deviation of 0.15
case2<-run_simulations(immutable_scenario, 500, #number of simulations
                       100, #years of simulations
                       0, 1, #mean temp, sd temp
                       0, 1, 0.15, #intercept trait, slope trait, sd trait
                       0.28, 1, 0) #intercept demo, slope demo, sd demo
saveRDS(case2, "case2.rds")


#case3= immutable trait during their life, small standar deviation of 0.30
case3<-run_simulations(immutable_scenario, 500, #number of simulations
                       100, #years of simulations
                       0, 1, #mean temp, sd temp
                       0, 1, 0.3, #intercept trait, slope trait, sd trait
                       0.28, 1, 0) #intercept demo, slope demo, sd demo
saveRDS(case3, "case3.rds")


#case4= immutable trait during their life, small standar deviation of 0.45
case4<-run_simulations(immutable_scenario, 500, #number of simulations
                       100, #years of simulations
                       0, 1, #mean temp, sd temp
                       0, 1, 0.45, #intercept trait, slope trait, sd trait
                       0.28, 1, 0) #intercept demo, slope demo, sd demo
saveRDS(case4, "case4.rds")