#Here functions for counting alive animals, running simulations, making plots, ecc.

#This function takes as input the table of the simulation and returns another table with a count of the animals alive in the summer of each year.
#With summer I intend the period between the reproduction event and the three animal-reduction events during the same year.

alive_animals <- function(table){
  years <- max(table$year)
  alive <- c()
  
  for (i in 0:years) {
    count <- table[birth_year <= i & year >= i, .N] #.N is a special data.table symbol that just returns the number of rows in the subset
    alive <- c(alive, count)
  }
  
  summary_table <- data.frame(
    year = 0:years,
    alive = alive
  )
  return (summary_table)}



#this function simply enough run n_sim simulations and of each one take the count of alive animals during the last year, if ==0 the population is counted as extinct.
#If the population is not extinct it averages the final count of individuals among simulations.
run_simulations <-function(func,
                           nsim,
                           years,
                           mean_t,
                           sd_t,
                           int_trait,
                           beta_trait,
                           sd_trait,
                           int_demo,
                           beta_demo,
                           sd_demo,
                           mixed_coef)
{
  
  results <- tibble()
  
  for (i in 1:nsim) {
    # Base arguments shared by all functions
    args <- list(
      years = years,
      mean_t = mean_t,
      sd_t = sd_t,
      int_trait = int_trait,
      beta_trait = beta_trait,
      sd_trait = sd_trait,
      int_demo = int_demo,
      beta_demo = beta_demo,
      sd_demo = sd_demo
    )
  
    
    
    # Run the function with dynamic arguments
    evolution <- do.call(match.fun(func), args)
    
    vector_alive <- alive_animals(evolution) |> pull(alive)
    years_with_animals<-length(vector_alive)-1
    sim_df <- tibble(
      year = 0:years_with_animals,
      simulation = i,
      alive = vector_alive
    )
    results <- bind_rows(results, sim_df)
  }
  
  return(results)
}

#same function with the coeff_var
run_simulations_mixed <-function(func,
                           nsim,
                           years,
                           mean_t,
                           sd_t,
                           int_trait,
                           beta_trait,
                           sd_trait,
                           int_demo,
                           beta_demo,
                           sd_demo,
                           mixed_coef)
{
  
  results <- tibble()
  
  for (i in 1:nsim) {
    evolution <- func(years=years,
                      mean_t=mean_t,
                      sd_t=sd_t,
                      int_trait=int_trait,
                      beta_trait=beta_trait,
                      sd_trait=sd_trait,
                      int_demo=int_demo,
                      beta_demo=beta_demo,
                      sd_demo=sd_demo,
                      mixed_coef=mixed_coef)
    
    vector_alive <- alive_animals(evolution) |> pull(alive)
    years_with_animals<-length(vector_alive)-1
    sim_df <- tibble(
      year = 0:years_with_animals,
      simulation = i,
      alive = vector_alive
    )
    results <- bind_rows(results, sim_df)
  }
  
  return(results)
}




#coefficient of variation is the standart deviation over mean. This function returns a vector with a c.v. for each simulation. 
coefficient_of_variation<- function(table){
  cv<- table |>
    group_by(simulation) |>
    summarise(cv= sd(alive)/mean(alive)) |> pull()
  return(cv)
}

#function that returns a table with the count of extinction after all the simulations
extinct_count <- function(table){
  count_extinct<- table |>
    group_by(simulation) |>
    summarise(extinct = n() < 101) |> #when we have less than 101 rows it means they get extinct before time.
    summarise(total_extinct = sum(extinct)) |> pull()
  return(count_extinct)}


####################
#PLOTTING FUNCTIONS#
####################

#Takes one simulation table and it returns a plot with the log count of alive animals over years
make_a_plot <- function(df){
  ggplot(df, aes(x = year, y = log10(alive))) +
    geom_point(size = 2) +
    geom_line(alpha = 0.6) +
    labs(
      x = "Year",
      y = "Log Number of Individuals Alive"
    ) +
    ggtitle("Log Count of Animals over years") +
    theme_minimal()+
    theme(
      plot.title = element_text(size = 20, face = "bold"),   # title size
      axis.title.x = element_text(size = 16),                # x-axis label size
      axis.title.y = element_text(size = 16))
}


#This function takes as input a dataframe with all the count of alive animal for each year of each simulations and returns a plot. On the X axis the years and on the Y axis the logarithm of the count of alive animals for that year.
#It uses colors to differentiate simulations, don't run with more than 20 simulations.
make_a_plot_sim <- function(df){
  ggplot(df, aes(x = year, y = log10(alive), group = simulation, color = as.factor(simulation))) +
    geom_point(size = 2) +
    geom_line(alpha = 0.6) +
    labs(
      x = "Year",
      y = "Log Number of Individuals Alive",
      color = "Simulation"
    ) +
    ggtitle("Log Count of Animals over years") +
    theme_minimal()+
    theme(
      plot.title = element_text(size = 20, face = "bold"),   # title size
      axis.title.x = element_text(size = 16),                # x-axis label size
      axis.title.y = element_text(size = 16))
}

