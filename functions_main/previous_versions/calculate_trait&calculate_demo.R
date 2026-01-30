set.seed(42)
#We could for example start with 200 animals, 100 juvenile and 100 adults
shape <- 'linear'
j_ani <-1:100
a_ani <- 101:200
animals <- c(j_ani,a_ani)

calculate_trait <- function(
    shape, animals, int,beta,beta_sd,clim.val
) { 
  
  #NICO: for now I will modify only the linear part and keep untouched the other functions.
  if (shape == "linear") {
    n_animals <- length(animals)
    beta_animals <- rnorm(n_animals,mean=beta,sd=beta_sd) #nico: here the user has to provide the sd and the mean
    trait_values <- int + (beta_animals * clim.val)
  } 
  
  # if (shape == "quadratic") {
  #   trait_value <- int + (beta * clim.val) + (beta2 * clim.val^2) #quadratic equation contains as well the linear
  # } 
  # 
 # if (shape == "sigmoid") {
 # # re-centered adjusted logistic regression from -2 to 2 with inflection point 0
 # # see NonLinearity_emp project, file test_sigmoid.R
 # trait_value <- (1 / (1 + exp(-5 * (int + beta * clim.val))) - 0.5) * 4  #This value are so because they need to recenter it
 # }
  
  return(trait_values)
}



calculate_demographic <- function(
    shape, animals, int, beta, beta_sd, trait_values
) {
  if (shape == "linear") {
    n_animals <- length(animals)
    beta_animals <- rnorm(n_animals,mean=beta,sd=beta_sd) #nico: here the user has to provide the sd and the mean
    demo_values <- int + (beta_animals * trait_values) #nico: the unique trait values * unique beta_animals
  }
  
  # if (shape == "quadratic") {
  #   demo_value <- int + (beta * trait.val) + (beta2 * trait.val^2)
  # }
  # 
  # if (shape == "sigmoid") {
  #   
  #   if (demo.type == "survival") { 
  #     ## standard logistic regression
  #     demo_value <- (1 / (1 + exp(int + beta * trait.val)))
  #   } else {
  #     ## re-centered adjusted logistic regression from -2 to 2 with inflection point 0
  #     ## see NonLinearity_emp project, file test_sigmoid.R
  #     demo_value <- (1 / (1 + exp(-5 * (int + beta * trait.val))) - 0.5) * 4
  #   }
  # }
  return(demo_values)
}

  
#I have to think about how the vector with animal will look like
trait_values <- calculate_trait(shape, animals, 0, 1, 1, 2) #function (shape, animals, int, beta, beta_sd, clim.val) 

demo_values <- calculate_demographic(shape, animals, 0, 1, 1 , trait_values)#function (shape, animals, int, beta, beta_sd, trait_values) 

#Oki, we have now the demographic values, let's see if they survive the year.

demo_values_p <- plogis(demo_values) #plogis transform real value into probabilities, here tho we need to be extra careful, the higher the demographic value the higher the probability of surviving. So a value of 0.97 or 97% means that there is a 97% chance of surviving.

life_events <- runif(demo_values_p, min = 0, max = 1) 

survived <- life_events < demo_values_p #if the value of the random variable is smaller than the survival probability the animal survives






