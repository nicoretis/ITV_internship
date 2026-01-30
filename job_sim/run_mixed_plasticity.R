#run plastic scenarios
source("./mix_plasticity.R")
source("./other_functions.R")

set.seed(42)

#case9= mixed update trait, every year a partially new trait, no sd (all animals equal)
case9<-run_simulations(mix_plasticity_scenario, 500, #number of simulations
                       100, #years of simulations
                       0, 1, #mean temp, sd temp
                       0, 1, 0, #intercept trait, slope trait, sd trait
                       0.28, 1, 0, #intercept demo, slope demo, sd demo
                       0.5) #coefficient of mix fixed-plastic
saveRDS(case9, "case9.rds")

#case10= mixed update trait, small sd=0.15
case10<-run_simulations(mix_plasticity_scenario, 500, #number of simulations
                       100, #years of simulations
                       0, 1, #mean temp, sd temp
                       0, 1, 0.15, #intercept trait, slope trait, sd trait
                       0.28, 1, 0, #intercept demo, slope demo, sd demo
                       0.5) #coefficient of mix fixed-plastic
saveRDS(case10, "case10.rds")

#case11= mixed update trait, medium sd=0.3
case11<-run_simulations(mix_plasticity_scenario, 500, #number of simulations
                        100, #years of simulations
                        0, 1, #mean temp, sd temp
                        0, 1, 0.3, #intercept trait, slope trait, sd trait
                        0.28, 1, 0, #intercept demo, slope demo, sd demo
                        0.5) #coefficient of mix fixed-plastic
saveRDS(case11, "case11.rds")

#case12= mixed update trait, medium sd=0.45
case12<-run_simulations(mix_plasticity_scenario, 500, #number of simulations
                        100, #years of simulations
                        0, 1, #mean temp, sd temp
                        0, 1, 0.45, #intercept trait, slope trait, sd trait
                        0.28, 1, 0, #intercept demo, slope demo, sd demo
                        0.5) #coefficient of mix fixed-plastic
saveRDS(case12, "case12.rds")