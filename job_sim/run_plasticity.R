#run plastic scenarios
source("./total_plasticity.R")
source("./other_functions.R")

set.seed(42)

#case5= flexible trait, every year a new trait, no sd (all animals equal)
case5<-run_simulations(plastic_scenario, 500, #number of simulations
                       100, #years of simulations
                       0, 1, #mean temp, sd temp
                       0, 1, 0, #intercept trait, slope trait, sd trait
                       0.28, 1, 0) #intercept demo, slope demo, sd demo
saveRDS(case5, "case5.rds")

#case6= flexible trait (new every y, every year a new trait, small sd=0.15
case6<-run_simulations(plastic_scenario, 500, #number of simulations
                       100, #years of simulations
                       0, 1, #mean temp, sd temp
                       0, 1, 0.15, #intercept trait, slope trait, sd trait
                       0.28, 1, 0) #intercept demo, slope demo, sd demo
saveRDS(case6, "case6.rds")

#case7= flexible trait (new every y, every year a new trait, small sd=0.3
case7<-run_simulations(plastic_scenario, 500, #number of simulations
                       100, #years of simulations
                       0, 1, #mean temp, sd temp
                       0, 1, 0.3, #intercept trait, slope trait, sd trait
                       0.28, 1, 0) #intercept demo, slope demo, sd demo
saveRDS(case7, "case7.rds")

#case8= flexible trait (new every y, every year a new trait, small sd=0.45
case8<-run_simulations(plastic_scenario, 500, #number of simulations
                       100, #years of simulations
                       0, 1, #mean temp, sd temp
                       0, 1, 0.45, #intercept trait, slope trait, sd trait
                       0.28, 1, 0) #intercept demo, slope demo, sd demo
saveRDS(case8, "case8.rds")