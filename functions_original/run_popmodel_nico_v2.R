pacman::p_load('colorednoise')
#------------------------------------------------------------------------------#
#----------------         POPULATION MODEL               ----------------------#
#------------------------------------------------------------------------------#

## This code contains the simulation model. 

## -----------------------------------------------------------------------------
## CALCULATION FUNCTIONS
## -----------------------------------------------------------------------------

## Reaction Norm Calculation Function ------------------------------------------

#int -> I guess it is the intercept of the equation I think it should be set to 0
#beta -> coefficient for the linear equation
#beta2 -> coefficient for the quadratic equation
#clim.val -> I guess it is just the simulated temperature value

#NICO
#This function take in input the temperature and it calculates the trait, it is indeed the reaction norm
#shape will be only linear, but I can keep the different shapes, I guess I have to find a placeholder for beta2

calculate_trait <- function(
    shape, int, beta, beta2, clim.val
  ) { 
  
  #NICO: for now I will modify only the linear part and keep untouched the other functions.
  if (shape == "linear") {
    trait_value <- int + (beta * clim.val) #nico: here we have to loop over all the animals, is there a way to not use a loop? 
  } 
  
  if (shape == "quadratic") {
    trait_value <- int + (beta * clim.val) + (beta2 * clim.val^2) #quadratic equation contains as well the linear
  } 
  
  if (shape == "sigmoid") { 
    ## re-centered adjusted logistic regression from -2 to 2 with inflection point 0
    ## see NonLinearity_emp project, file test_sigmoid.R
    trait_value <- (1 / (1 + exp(-5 * (int + beta * clim.val))) - 0.5) * 4  #This value are so because they need to recenter it
  }
  
  return(trait_value)
}

#NICO
#Similar function but it takes as input the trait value and it returns the demographic value, what exactly represent?
#Here we have as well the demo.type that can be "survival" or others (I guess Fecundity)

## Trait-Demographic Rate Calculation Function ---------------------------------

calculate_demographic <- function(
    shape, int, beta, beta2, trait.val, demo.type
  ) {
  #NICO: for now I will modify only the linear part and keep untouched the other functions.
  if (shape == "linear") {
    #nico: here we have to loop over all the animals, is there a way to not use a loop? 
    demo_value <- int + (beta * trait.val)
  }
  
  if (shape == "quadratic") {
    demo_value <- int + (beta * trait.val) + (beta2 * trait.val^2)
  }
#Only for shape sigmoid we have a differentiation for survival or reproduction, I have to take a look over the test_sigmoid.R
  if (shape == "sigmoid") {
    
    if (demo.type == "survival") { 
      ## standard logistic regression
      demo_value <- (1 / (1 + exp(int + beta * trait.val)))
    } else {
      ## re-centered adjusted logistic regression from -2 to 2 with inflection point 0
      ## see NonLinearity_emp project, file test_sigmoid.R
      demo_value <- (1 / (1 + exp(-5 * (int + beta * trait.val))) - 0.5) * 4
    }
  }
  
  return(demo_value)
}




## -----------------------------------------------------------------------------
## Model Calculation
## -----------------------------------------------------------------------------



#NICO
#So they run the simulation for a total of 200 years but the first 100 years are of burn-in. During the burn-in period there is no difference in the mean temperature, so no specie should go extinct
#big function here to come... 

## Population dynamics model is summarized into a function
## The parametrization is set up for some default parameters 
run_popmodel <- function(
    
  ## FUNCTION ARGUMENTS ########################################################
  n = c(100, 100),         # initial population size: `c(adults, juveniles)`
  scen_years = 100,        # number of simulated years
  burnin_years = 100,      # duration of the burn-in period in years
  
  ## Climate parameters --------------------------------------------------------
  #NICO
  #Climate parameter for now I'll keep just keep constant mean and linear mean (increasing every year). Standard deviation for now is just constant to 0.2"
  
  temp_avg = 0,            # mean temperature
  temp_sd = 0.2,           # standard deviation of the mean temperature
  temp_trend_avg = 0.02,   # slope of change in temperature (if `temp_avg != 0`)
  #temp_trend_sd = 0,       # trend in the standard deviation of the mean temperature (if `temo_sd != 0`)
  #phi = 0,                 # parameter to create white, blue, or red noise                    
  
  ## Burn-in climate parameters ------------------------------------------------
  burnin_avg = 0,          # mean temperature during burn-in period
  burnin_sd = 0.67,        # standard deviation of the mean temperature during burn-in period
  #NICO
  #Why is the burnin_sd different from rhe temp_sd?
  #burnin_phi = 0,          # noise during burn-in period
  
  ## Population parameters -----------------------------------------------------
  Dd = "Ricker",           # density dependence ("Ricker" or "Beverton-Holt"; fixed)
  b = 0.0001,              # strength of the density dependence (fixed)
  
  ## Trait parameters ----------------------------------------------------------
  
  ## Climate-trait relationships (reaction norms)
  trait_fct = "linear", # trait shape ("linear", "quadratic", or "sigmoid") 
  int_trait_juv = 0,       # intercept for temp-trait relationship in juveniles
  int_trait_adu = 0,       # intercept for temp-trait relationship in adults
  beta_trait_juv = 1,      # beta for temp-trait relationship in juveniles
  beta_trait_adu = 1,      # beta for temp-trait relationship in adults
  beta2_trait_juv = 0,     # beta square for temp-trait relationship in juveniles
  beta2_trait_adu = 0,     # beta square for temp-trait relationship in adults
  
  ## Demographic rate parameters -----------------------------------------------
  
  ## Trait-demographic rates relationships
  demo_fct = "linear",  # demographic rate shape ("linear", "quadratic", or "sigmoid")
  
  ## Juvenile survival
  beta_surv_juv = 1,       # beta for trait-survival relationship in juveniles
  beta2_surv_juv = 0,      # beta square for trait-survival relationship in juveniles
  int_surv_juv = 0,        # intercept for trait-survival relationship in juveniles
  
  #NICO
  #Transition probability value depends on the life-history of the animal
  ## Transition probabilities
  transition_prob = 0.5,   # fix value of the transition probability
  
  ## Adult survival
  beta_surv_adu = 1,       # beta for trait-survival relationship in adults
  beta2_surv_adu = 0,      # beta square for trait-survival relationship in adults
  int_surv_adu = 0,        # intercept for trait-survival relationship in adults
  
  ## Adult fecundity
  beta_repr_adu = 1,       # beta for trait-reproduction relationship in adults
  beta2_repr_adu = 0,      # beta square for trait-reproduction relationship in adults
  int_repr_adu = 0,        # intercept for trait-reproduction relationship in adults
  
  ## Control
  seed = NULL              # seed to set random number generator

)
#NICO
#ALL these are all the parameters of the main function! depending on the value of the parameter we decide for example the climate model we are going to create, or what type of curve we will be following

{
  
  ## FUNCTION BODY #############################################################

  if(!is.null(seed)) set.seed(seed)
  
  ## STORAGE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## scen_years + 1 to simulate x time scen_years and keep initialization record
  sum_years <- burnin_years + scen_years + 1  
  
  ## storage array for abundances of each classes for each replicate
  all_years <- matrix(0, nrow = 2, ncol = sum_years)
  
  ## storage array for total abundances for each replicate
  pop_total <- rep(0, sum_years)         

  #NICO
  #Here we are initializing an empty dataframe, specifying the type of every column
  #double is like float in python

  data_total <- data.frame(
    "time"           = integer(),
    "climate"        = double(),
    "abundance"      = double(), #It is just the sum of juvenile and adults
    "growth_rate"    = double(), 
    "trait_juv"      = double(), 
    "trait_adu"      = double(),
    "Sj"             = double(), #Survivor junior--> depends as well from the density dependency
    "Sa"             = double(), 
    "repr"           = double(), 
    "trans"          = double(),
    "temp_avg"       = double(),
    "temp_sd"        = double(),
    "temp_trend_avg" = double()
    #"temp_trend_sd"  = double(),
    #"phi"            = double()
  )

  ## CLIMATE GENERATOR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #NICO
  #We just have two condition on the if statement:
  #1) constant mean-->temp_trend_avg == 0
  #2) non-constant mean --> temp_trend_avg =! 0 but most likely it will be >0
  
  ## climate is constant during the specified burn-in period 
  climate_burnin <- suppressMessages(colorednoise::colored_noise(
    burnin_years + 1, mean = burnin_avg, sd = burnin_sd , phi = 0
  )) 
  
  ## climates for simulations based on scenarios specified in `tbl_climate`
  
  ## 1. Mean is constant -------------------------------------------------------
  if (temp_trend_avg == 0) {
    
    climate_scen <- colorednoise::colored_noise(
      scen_years, mean = temp_avg, sd = temp_sd, phi=0
    )
    
  #NICO
  #I've deleted temp_trend_sd, it is always 0, following line can be fully commented out
  #I've modified a bit the order, so it is a bit more easy to understand
    
    #if (temp_trend_sd == 0) {
      
      ## 1.1 Constant climatic fluctuation .....................................
      
      
      
     #else { 
      
      ## 1.2 + 1.3 Change in climatic fluctuation ------------------------------
       #NICO
       #The temperature fluctuation (or standard deviation) in the last simulation year is equal to the absolute value of the yearly trend in temperature SD multiplied by the number of scenario years.
      #I haven't understood why is called temp_last and not sd_temp_last
       
      #temp_last <- abs(temp_trend_sd) * scen_years 
      
      ## 1.2 Increase climatic fluctuation -------------------------------------
      # Scenario where we the sd of the temperature increases year by year
      # climate_scen <- unlist(lapply(
      #   seq(0, temp_last, temp_last/scen_years), 
      #   function(a) {  
      #     colorednoise::colored_noise(1, mean = temp_avg, sd = a, phi = phi) 
      #   } 
      # ))
      
      ## 1.3 Decrease climatic fluctuation -------------------------------------
      
      #if (temp_trend_sd < 0) { climate_scen <- rev(climate_scen) }
      
    #} 
    
    ## 2. Mean increasing ------------------------------------------------------
  } else {
    
    temp_trend_max <- temp_trend_avg * 1/2 * scen_years #The idea is that we center it again from at 0, dunno why?
    temp_avg_list <- seq(-temp_trend_max, temp_trend_max, length.out = scen_years)
    
    climate_scen <- unlist(lapply(
      1:scen_years, 
      function(a) { 
        colorednoise::colored_noise(
          1, mean = temp_avg_list[a], sd = temp_sd, phi = 0
        ) 
      }
    ))
    
  } 
    
    #NICO
    #Again we comment this out because we simplify and we don't have trends on the standard deviation
    #if (temp_trend_sd == 0) {
      
      ## 2.1 Constant climatic fluctuation -------------------------------------
      
#else {
      #NICO
      #Here we have an increase of both the mean temperature and the standard deviation: comment out!
      ## 2.2 + 2.3 Change in climatic fluctuation ------------------------------
      
      # temp_avg_list <- seq(-temp_trend_max, temp_trend_max, length.out = scen_years)
      # temp_sd_list  <- seq(0, abs(temp_trend_sd) * scen_years, length.out = scen_years)
      # 
      # ## 2.2 Increase climatic fluctuation -------------------------------------
      # 
      # climate_scen <- unlist(lapply(
      #   1:scen_years, 
      #   function(a) { 
      #     colorednoise::colored_noise(
      #       1, mean = temp_avg_list[a], sd = temp_sd_list[a], phi = phi
      #     ) 
      #   }
      # )) 
      
      # climate_scen[is.nan(climate_scen)] <- 0
      
      ## 2.3 Decrease climatic fluctuation -------------------------------------
      
    #   if (temp_trend_sd < 0) { climate_scen <- rev(climate_scen) }
    # }
  #} 

  climate <- c(climate_burnin, climate_scen)
  
  ## MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #n is the original population size, initialize the year matrix of two rows for juvenile and adults"
  
  all_years[ , 1] <- n
  pop_total[1] <- all_years[1, 1] + all_years[2, 1] #juvenile and adult
  
  #starting from the second year because first initialize (R first position is 1)
  for (t in 2:sum_years) {
    
    ## Temperature -------------------------------------------------------------
    
    ## select climate value based on period and scenario
    temperature <- climate[t]
    
    
    ## Calculation of trait average --------------------------------------------
    #NICO
    #Important consideration! Knowing that it comes from a specific distribution, here we are JUST calculating the MEAN of the trait!
    #We are not considering the real possible distribution of it, so to say the intraspecific variability, that will be my next part!"
    #Very simple: we calculate first the trait and then the survival for both juvenile and adult; only for the adult we calculate as well the fecundity, depending whether their demo.type is on "survival" or "reproduction".
    
    
    ## Trait values are calculated as function of temperature
    ## Juveniles ...............................................................
    juv_trait_mean <- calculate_trait(
      clim.val  = temperature, 
      shape     = trait_fct, 
      beta      = beta_trait_juv,  
      beta2     = beta2_trait_juv, 
      int       = int_trait_juv
    )  
    
    ## Adults ..................................................................
    adu_trait_mean <- calculate_trait(
      clim.val = temperature, 
      shape    = trait_fct, 
      beta     = beta_trait_adu,  
      beta2    = beta2_trait_adu, 
      int      = int_trait_adu
    ) 
    
    
    ## Calculation of demographic rate -----------------------------------------
    
    ## Juvenile survival .......................................................
    surv_juv_lin <- calculate_demographic(
      demo.type = "survival",
      trait.val = juv_trait_mean, 
      shape     = demo_fct, 
      beta      = beta_surv_juv, 
      beta2     = beta2_surv_juv, 
      int       = int_surv_juv
    )
    
    ## Juvenile transition (here fixed) ........................................
    # trans_juv_to_ad <- demo.calc(
    #   beta = beta_transi, beta2 = beta2_transi, 
    #   trait.val = juv_trait_mean, int.demo = int_trans
    # )
    
    ## Adult survival ..........................................................
    surv_adu_lin <- calculate_demographic(
      demo.type = "survival",
      trait.val = adu_trait_mean, 
      shape     = demo_fct, 
      beta      = beta_surv_adu, 
      beta2     = beta2_surv_adu, 
      int       = int_surv_adu
    )
    "rept_adu_lin is the amount of newborn, where is the transiction between juvenile --> adult ??"
    ## Adult fecundity .........................................................
    repr_adu_lin <- calculate_demographic(
      demo.type = "reproduction",
      trait.val = adu_trait_mean, 
      shape     = demo_fct, 
      beta      = beta_repr_adu, 
      beta2     = beta2_repr_adu, 
      int       = int_repr_adu
    )
    
    
    ## Link function -----------------------------------------------------------
    
    ## The link function is here to transform the fecundity value into a rate. 
    ## No need to logit-transform survival only if sigmoid shape is used for 
    ## survival, for linear and quadratic shapes we  still have to apply inverse 
    ## logit-transform

    if(demo_fct != 'sigmoid'){
      surv_juv_lin <- plogis(surv_juv_lin)  
      surv_adu_lin <- plogis(surv_adu_lin)      
    }
    
    
    repr_adu_trans <- exp(repr_adu_lin) ## exp link function for fecundity
    
    
    ## Density dependence ------------------------------------------------------
    ## After transformation, we apply density dependence on a rate 
    ## (not in the other way because the formula applied on a rate).
    
    ##NICO
    #corrected  probability values according to the DD
    
    ## Ricker density-dependence function ......................................
    if (Dd == "Ricker") {
      n_sum    <- sum(all_years[ , t-1])             ## abundance in previous state for density-dependence
      n_adult  <- all_years[2, t-1]                  ## abundance of adults in previous state
      surv_juv <- surv_juv_lin * exp(-b * n_sum)     ## DD juvenile survival 
      surv_adu <- surv_adu_lin * exp(-b * n_adult)   ## DD adult survival
      repr_adu <- repr_adu_trans * exp(-b * n_adult) ## DD fecundity
      #trans    <- trans_to_adu_trans                 ## DD transfer juvenile to adult
    }
    
    ## Beverton-Holt density-dependence function ...............................
    if (Dd == "Beverton-Holt") {
      n_sum    <- sum(all_years[ , t-1])             ## abundance in previous state for density-dependence
      surv_juv <- surv_juv_lin / (1 + (b * n_sum))   ## DD juvenile survival
      surv_adu <- surv_adu_lin / (1 + (b * n_adult)) ## DD adult survival
      repr_adu <- repr_adu_lin / (1 + (b * n_adult)) ## DD fecundity
      #trans    <-  trans_juv_to_ad                   ## DD transfer juvenile to adult
    }
    
    
    ## Population dynamics -----------------------------------------------------
    ## The matrix is constructed based on the demographic rates with density dependence.
    
    #NICO
    #How to simply read this matrix:
    #First row: survived juvenile (that stays juvenile) + new prole (made of course by adults)
    #Second row: survived juvenile that will move to adult + survived adult
    
    
    dem_rates <- c(surv_juv * (1 - transition_prob), repr_adu,
                   surv_juv * transition_prob,       surv_adu)
    
    A <- matrix(dem_rates, nrow = 2, ncol = 2, byrow = TRUE)
    
    
    ## Metrics -----------------------------------------------------------------
    ## abundance is estimated based on abundance in previous time step
    
    #NICO
    #A comment, I'm not sure how necessary is to have a matrix considering we are running a for loop (over t=years)
    #We are not truly saving much time, maybe I could fix this.
    all_years[ , t] <- all_years[ , t-1] %*% A
    
    ## to avoid potential errors, we replace NA and NaN by 0
    all_years[is.nan(all_years)] <- 0

    ## round to the next integer (could use `floor` as well)
    all_years[ , t] <- round(all_years[ , t], 0)
    
    ## store outcomes per time step
    pop_total[t] <- sum(all_years[ , t])
    
    
    
    data_total[t, "climate"]       <- climate[t]
    data_total[t, "time"]          <- t - 1
    data_total[t, "abundance"]     <- sum(all_years[ ,t])
    
    #NICO
    #If the population at the previous year exists and > 0, than return the growth rate (between every point), otherwise NA: extinct"
    if (!is.na(pop_total[t-1]) & pop_total[t-1] > 0) { 
      data_total[t, "growth_rate"] <- pop_total[t] / pop_total[t-1]
    } else {
      data_total[t, "growth_rate"] <- NA
    }
    
    if (!is.na(pop_total[t]) & pop_total[t] > 0) {
      data_total[t, "trait_juv"]   <- juv_trait_mean
      data_total[t, "trait_adu"]   <- adu_trait_mean
      data_total[t, "Sj"]          <- surv_juv
      data_total[t, "Sa"]          <- surv_adu
      data_total[t, "repr"]        <- repr_adu
      data_total[t, "trans"]       <- transition_prob #NICO: why do we store the transition prob if it is constant and we are going to store it as well later on together with the other parameters?
    } else {
      data_total[t, "trait_juv"]   <- NA
      data_total[t, "trait_adu"]   <- NA
      data_total[t, "Sj"]          <- NA
      data_total[t, "Sa"]          <- NA
      data_total[t, "repr"]        <- NA
      data_total[t, "trans"]       <- NA
    }
    
  }  ## years close
  
  
  ## store initial values
  data_total[1, "time"]      <- 0
  data_total[1, "climate"]   <- 0
  data_total[1, "abundance"] <- sum(n[1], n[2])
  #data_total[1, "growth_rate"] <-  ## TODO: not filled, should be NA or smt else?
  data_total[1, "trait_juv"] <- 0
  data_total[1, "trait_adu"] <- 0
  
  if (Dd == "Ricker") {
    data_total[1, "Sj"]      <- int_surv_juv * exp(-b * n_sum) ## TODO: really n_sum here?
    data_total[1, "Sa"]      <- int_surv_adu * exp(-b * n_sum) ## TODO: really n_sum here?
  }
  if (Dd == "Beverton-Holt") {
    data_total[1, "Sj"]      <- int_surv_juv / (1 + (b * n_sum)) ## TODO: really n_sum here?
    data_total[1, "Sa"]      <- int_surv_adu / (1 + (b * n_adult)) ## TODO: for now n_adult here as specified above, but maybe n_sum here?
  }
  data_total[1, "repr"]      <- exp(int_repr_adu) * exp(-b * n_sum)
  data_total[1, "trans"]     <- transition_prob
  
  ## store parameters
  data_total[ , "temp_avg"]       <- temp_avg
  data_total[ , "temp_sd"]        <- temp_sd
  data_total[ , "temp_trend_avg"] <- temp_trend_avg
  #data_total[ , "temp_trend_sd"]  <- temp_trend_sd
  #data_total[ , "phi"]            <- phi
  
  return(tibble::tibble(data_total))
  
}

#NICO
#Let's rune run_popmodel to see if it is still working after me commenting everything out (:
#args(run_popmodel) #to see all the arguments of a function

big_table <- run_popmodel(
  n <- c(100,100),
  scen_years <- 100,  burnin_years <- 100,
  temp_avg <- 0,  temp_sd <- 0,
  temp_trend_avg <-0,
  burnin_avg = 0, burnin_sd = 0.67, 
  Dd = "Ricker", b = 1e-04,
  trait_fct = "linear",
  int_trait_juv = 0,  int_trait_adu = 0,
  beta_trait_juv = 1, beta_trait_adu = 1, 
  beta2_trait_juv = 0, beta2_trait_adu = 0, #unused
  demo_fct = "linear",
  int_surv_juv = 2.5, int_surv_adu = 0,
  beta_surv_juv = 1, beta_surv_adu = 1,
  beta2_surv_juv = 0, beta2_surv_adu = 0, #unused
  int_repr_adu = 3,
  beta_repr_adu = 1,
  beta2_repr_adu = 0, #unused
  transition_prob = 0.5,  
  seed = 1) 
  






