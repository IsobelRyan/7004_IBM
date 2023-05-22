# Stochastic Event-Based Model: Modelling Population Size of A. latezonatus In Response to Anemone Bleaching 

# import required libraries 
library(ggplot2)
library(tidyverse)
library(DoE.wrapper)


# Develop a function that models anemonefish population dynamics: 

# There are five life stages: egg, larval, recruit, juvenile and adult. The larval stage is characterised by an oceanic phase where survival is largely influenced by environmental conditions and predation. The other life stages are termed reef phases where survival is largely influenced by host quality, predation and competition.  

# In more recent times, bleaching events have become a large threat for anemone hosts. Thus, I want to develop a second function which models the effects of bleached anemone hosts on anemonefish populations. When anemones bleach, they reduce in size and health which can negatively impact anemonefishes by increasing their stress responses and reducing their recruitment and fecundity. In response to bleaching, anemonefish have been found to utilise other species of anemone hosts when their preferred species becomes limited. 



# Parameters  -------------------------------------------------------------

# Define the parameters and combine those for anemonefish and anemones as a list to be passed to the functions:

# Determine the time in years for the simulation to run over
time_in_years <- 1

# Define anemone parameters
anemone_pop_size <- 16 # the number of anemone hosts 
anemone_oral_disc_area <- rnorm(runif(anemone_pop_size, min = 100, max = 2500), # generates n random numbers between 100 and 2500
                                mean = 1000, 
                                sd = 500) # determines the size of the anemone 

# Make a list to combine all the parameters for anemone hosts:
anem_params <- list(anemone_pop_size = anemone_pop_size, 
                    anemone_oral_disc_area = anemone_oral_disc_area)

# Make a list to combine all the parameters for anemonefish
anemfish_params <- list(prob_of_suitable_partner = rnorm(sample(0:1, anemone_pop_size, replace = TRUE), 0.8, 0.1), # mean of 0.8 and sd of 0.1
                        prob_of_mating = rnorm(sample(0:1, anemone_pop_size, replace = TRUE), 0.8, 0.1),  
                        predation_rate_oceanic_phase = 0.7, 
                        predation_rate_reef_phase = 0.05, 
                        competition_oceanic_phase = 0.7, 
                        competition_reef_phase = 0.05)


# Make a list to combine all the parameters for bleaching events
bleaching_event_params <- list(prob_of_bleaching_event = 0.5,
                               anemone_mortality_threshold = 0.7) 




# Functions to model each lifestage ---------------------------------------

# Function 1: Model the initial population size of anemonefish on anemone hosts --------
anemonefish_initial_population <- function(anemfish_params, anem_params) {
  # Determine size of current anemonefish population:
  # Create an empty data frame
  df <- data.frame(time_step = integer(), 
                         anemone_oral_disc_area = numeric(),
                         carrying_capacity = integer(),
                         eggs = integer(),
                         larvae = integer(),
                         recruits = integer(),
                         juveniles = integer(),
                         adults = integer(),
                         males = integer(),
                         females = integer())
  
  # Calculate the sum of anemonefish population size and assign anemonefish group size for each anemone host:
  for (i in 1:anem_params$anemone_pop_size) {
    # Generate randomly drawn samples for the number of anemonefish for each life stage
    anemone_oral_disc_area <- sort(rnorm(runif(anem_params$anemone_pop_size, min = 100, max = 2500), mean = 1000, sd = 250))
    
    # Calculate the carrying capacity for each anemone based on size: 
    if(anemone_oral_disc_area[i] <= 500) {
      carrying_capacity <- sample(0:10, anem_params$anemone_pop_size, replace = TRUE)
    } else {
      if (anemone_oral_disc_area[i] > 500 & anemone_oral_disc_area[i] <= 2000) {
        carrying_capacity <- sample(10:30, anem_params$anemone_pop_size, replace = TRUE)
      } else {
        carrying_capacity <- sample(30:35, anem_params$anemone_pop_size, replace = TRUE)
      }
    }
    
    # Generate values for the first time step where the adults and consequently, males and females are randomly assigned initially 
    row_data <- data.frame(time_step = rep(1, anem_params$anemone_pop_size),
                           anemone_oral_disc_area = anemone_oral_disc_area,
                           carrying_capacity = carrying_capacity,
                           eggs = rep(0, anem_params$anemone_pop_size),
                           larvae = rep(0, anem_params$anemone_pop_size),
                           recruits = rep(0, anem_params$anemone_pop_size),
                           juveniles = rep(0, anem_params$anemone_pop_size),
                           adults = sort(sample(1:4, anem_params$anemone_pop_size, replace = TRUE, prob = c(0.2, 0.6, 0.6, 0.4))),
                           females = sort(sample(0:2, anem_params$anemone_pop_size, replace = TRUE, prob = c(0.1, 0.8, 0.1))))
    
    # Calculate the number of males for the current index
    row_data$males <- (row_data$juveniles + row_data$adults) - row_data$females
    
    # Add the new data to the initalised data frame
    init_pop <- rbind(df, row_data)
  }
  
  # Return the completed data frame
  return(init_pop)
}


# Function 2: Model egg production life stage --------
anemonefish_lifestage_egg <- function(init_pop, anemfish_params, anem_params) {
  
  # Assign initial population values to another object to avoid altering the initial population
  eggs_pop <- init_pop
  
  # Determine number of eggs produced by each female:
  eggs <- rep(0, nrow(eggs_pop)) # initalise egg column with 0s
  
  # Calculate number of eggs where males and females are greater than 1:
  for (i in 1:nrow(eggs_pop)) {
    if (eggs_pop$females[i] >= 1 & eggs_pop$males[i] >= 1) {
      eggs[i] <- round(eggs_pop$females[i] * anemfish_params$prob_of_suitable_partner[i] * anemfish_params$prob_of_mating[i] * 1000)
    } else {
      eggs[i] <- 0
    }
  }
  
  # Create new dataframe to add the new values for the number of eggs 
  time_step_2 <- data.frame(time_step = rep(2, anemone_pop_size), 
                            anemone_oral_disc_area = eggs_pop$anemone_oral_disc_area,
                            carrying_capacity = eggs_pop$carrying_capacity,
                            eggs = eggs,
                            larvae = eggs_pop$larvae,
                            recruits = eggs_pop$recruits,
                            juveniles = eggs_pop$juveniles,
                            adults = eggs_pop$adults,
                            males = eggs_pop$males,
                            females = eggs_pop$females)
  
  # Bind the new subset to the existing data frame
  new_pop_egg <- rbind(eggs_pop, time_step_2)
  
  # Return the new data frame 
  return(new_pop_egg)
}



# Function 3: Model the larval life stage ---------------------------------
anemonefish_lifestage_larval <- function(new_pop_egg, anemfish_params, anem_params) {
  # Determine the number of surviving larvae when considering the impacts of predation and environmental factors:
  # Assign eggs to new variable to determine the amount which survived to become larvae:
  larvae <- new_pop_egg$eggs[new_pop_egg$time_step == 2]
  for (i in 1:length(larvae)) {
    if (larvae[i] > 0) {
      # Determine the number of anemonefish eggs that successfully hatched to become larvae:
      prob_of_survival_oceanic <- exp(-anemfish_params$predation_rate_oceanic_phase - anemfish_params$competition_oceanic_phase)
      surviving_larvae <- round(larvae[i] * prob_of_survival_oceanic)
      larvae[i] <- surviving_larvae
    } else {
      larvae[i] <- 0
    }
  }
  
  # Create new dataframe to add the new values for the number of eggs
  time_step_3 <- data.frame(time_step = rep(3, length(larvae)), 
                            anemone_oral_disc_area = new_pop_egg$anemone_oral_disc_area[new_pop_egg$time_step == 2],
                            carrying_capacity = new_pop_egg$carrying_capacity[new_pop_egg$time_step == 2],
                            eggs = rep(0, length(larvae)),
                            larvae = larvae,
                            recruits = new_pop_egg$recruits[new_pop_egg$time_step == 2],
                            juveniles = new_pop_egg$juveniles[new_pop_egg$time_step == 2],
                            adults = new_pop_egg$adults[new_pop_egg$time_step == 2],
                            males = new_pop_egg$males[new_pop_egg$time_step == 2],
                            females = new_pop_egg$females[new_pop_egg$time_step == 2])
  
  # Bind the new subset to the existing data frame
  new_pop_larvae <- rbind(new_pop_egg, time_step_3)
  
  # Return the new dataframe 
  return(new_pop_larvae)
}



# Function 4: Model the recruit life stage ---------------------------
anemonefish_lifestage_recruit <- function(new_pop_larvae, anemfish_params, anem_params) {

  # Assign larvae to new object name 
  time_step_3 <- new_pop_larvae[new_pop_larvae$time_step == 3, ]
  # Specify current capacity for this time step
  current_capacity <- time_step_3$carrying_capacity
  # Intialise columns
  current_size <- numeric(nrow(time_step_3))
  spots_left <- numeric(nrow(time_step_3))
  
  # Determine the number of anemonefish larvae that successfully survived to become recruits:
  for (i in 1:nrow(time_step_3)) {
    recruits <- time_step_3[i, "larvae"]
    prob_of_survival_reef <- exp(-anemfish_params$predation_rate_reef_phase - anemfish_params$competition_reef_phase)
    surviving_recruits <- round(recruits * prob_of_survival_reef)
    
    # Allocate recruits to anemones based on oral disc area
    recruit_dispersal_prob <- time_step_3$anemone_oral_disc_area / sum(time_step_3$anemone_oral_disc_area)
    
    # Randomly allocate the recruits based on the probabilities:
    allocated_recruits <- rbinom(nrow(time_step_3), size = surviving_recruits, prob = recruit_dispersal_prob)
    
    # Check if the sum of allocated recruits, juveniles, and adults exceeds the carrying capacity:
    current_size <- rowSums(cbind(allocated_recruits, time_step_3$juveniles, time_step_3$adults))
    spots_left <- pmax(current_capacity - current_size, 0)
    
    if (sum(allocated_recruits[i] + time_step_3$juveniles[i] + time_step_3$adults[i]) <= current_capacity[i]) {
      allocated_recruits[i] <- allocated_recruits[i]
    } else {
      allocated_recruits[i] <- spots_left[i]
    }
  }
  
  # Create new dataframe to add the new values for the number of eggs 
  time_step_4 <- data.frame(time_step = rep(4, nrow(time_step_3)), 
                            anemone_oral_disc_area = time_step_3$anemone_oral_disc_area,
                            carrying_capacity = time_step_3$carrying_capacity,
                            eggs = rep(0, nrow(time_step_3)),
                            larvae = rep(0, nrow(time_step_3)),
                            recruits = allocated_recruits,
                            juveniles = time_step_3$juveniles,
                            adults = time_step_3$adults,
                            males = time_step_3$males,
                            females = time_step_3$females)
  
  # Bind the new subset to the existing data frame
  new_pop_recruits <- rbind(new_pop_larvae, time_step_4)
  
  # Return new dataframe 
  return(new_pop_recruits)
}



# Function 5: Model the juvenile life stage  ----------------------
anemonefish_lifestage_juvenile <- function(new_pop_recruits, anemfish_params, anem_params) {
  
  # Assign previous timestep to new object variable
  time_step_4 <- new_pop_recruits[new_pop_recruits$time_step == 4, ]
  
  # Initiate empty columns
  surviving_juveniles <- numeric(nrow(time_step_4))
  current_size <- numeric(nrow(time_step_4))
  spots_left <- numeric(nrow(time_step_4))
  
  # Assign recruits from time to new variable 
  juveniles <- time_step_4$recruits # recruits from previous time step now become juveniles
  
  # Determine the number of anemonefish recruits that successfully survived to become juveniles:
  prob_of_survival_reef <- exp(-anemfish_params$predation_rate_reef_phase - anemfish_params$competition_reef_phase)
  
  # Assign current capacity to the previous capacity 
  current_capacity <- time_step_4$carrying_capacity
  
  for (i in 1:nrow(time_step_4)) {
    
    # Update the number of juveniles in the new data frame using the probability of survival 
    surviving_juveniles[i] <- round(juveniles[i] * prob_of_survival_reef)

    # Check if the sum of recruits, juveniles, and adults exceeds the carrying capacity:
    current_size[i] <- surviving_juveniles[i] + time_step_4$adults[i]
    spots_left[i] <- max(0, current_capacity[i] - current_size[i])
    juveniles[i] <- ifelse(current_size[i] <= current_capacity[i], 
                           surviving_juveniles[i], 
                           spots_left[i])
  }
  
  # Create new dataframe to add the new values for the number of eggs 
  time_step_5 <- data.frame(time_step = rep(5, nrow(time_step_4)), 
                            anemone_oral_disc_area = time_step_4$anemone_oral_disc_area,
                            carrying_capacity = time_step_4$carrying_capacity,
                            eggs = rep(0, nrow(time_step_4)),
                            larvae = rep(0, nrow(time_step_4)),
                            recruits = rep(0, nrow(time_step_4)),
                            juveniles = juveniles,
                            adults = time_step_4$adults,
                            males = time_step_4$males,
                            females = time_step_4$females)
  
  # Bind the new subset to the existing data frame
  new_pop_juveniles <- rbind(new_pop_recruits, time_step_5)
  
  # Return the new dataframe 
  return(new_pop_juveniles)
}

# Function 6: Model the adult life stage ----------------------
anemonefish_lifestage_adult <- function(new_pop_juveniles, anemfish_params, anem_params) {
  # Assign the previous time step to a new object variable 
  time_step_5 <- new_pop_juveniles[new_pop_juveniles$time_step == 5, ]
    
  # Initialize empty columns
  males_in_group <- numeric(nrow(time_step_5))
  surviving_adults <- numeric(nrow(time_step_5))
  current_size <- numeric(nrow(time_step_5))
  spots_left <- numeric(nrow(time_step_5))
  adults <- numeric(nrow(time_step_5))
  
  # Specify current capacity
  current_capacity <- time_step_5$carrying_capacity
  
  # Assign juveniles to adults 
  new_adults <- time_step_5$juveniles
  
  # Determine the number of anemonefish juveniles that successfully survived to become adults:
  for (i in 1:nrow(time_step_5)) {
    prob_of_survival_reef <- exp(-anemfish_params$predation_rate_reef_phase - anemfish_params$competition_reef_phase)
    surviving_adults[i] <- round(new_adults[i] * prob_of_survival_reef) + time_step_5$adults[i]

    # Check if the sum of recruits, juveniles, and adults exceeds the carrying capacity:
    current_size[i] <- surviving_adults[i]
    spots_left[i] <- max(0, current_capacity[i] - current_size[i])
    
    if (current_size[i] <= current_capacity[i]) {
      adults[i] <- surviving_adults[i]
    } else {
      adults[i] <- spots_left[i]
    }
    time_step_5$adults[i] <- adults[i]
    
    # Update number of males in each anemone patch
    males_in_group[time_step_5$adults == 0] <- 0  # Set number of males to 0 if there are no adults
    males_in_group[i] <- time_step_5[i, "adults"] - time_step_5[i, "females"] 
    
  } 
  
  # Create new dataframe to add the new values for the number of eggs 
  time_step_6 <- data.frame(time_step = rep(6, nrow(time_step_5)), 
                            anemone_oral_disc_area = time_step_5$anemone_oral_disc_area,
                            carrying_capacity = time_step_5$carrying_capacity,
                            eggs = rep(0, nrow(time_step_5)),
                            larvae = rep(0, nrow(time_step_5)),
                            recruits = rep(0, nrow(time_step_5)),
                            juveniles = rep(0, nrow(time_step_5)),
                            adults = time_step_5$adults,
                            males = males_in_group,
                            females = time_step_5$females)
  
  # Bind the new subset to the existing data frame
  new_pop_adults <- rbind(new_pop_juveniles, time_step_6)
  
  # Return new dataframe 
  return(new_pop_adults)
}



# Master Function: Loops through each life stage function to determine the final population size ------------------
anemonefish_population_simulation <- function(time_steps, anemfish_params, anem_params) {
  # Generate initial population
  pop <- anemonefish_initial_population(anemfish_params, anem_params)
  
  # Loop through each time step
  for (t in 1:time_steps) {
    # Update egg production
    pop <- anemonefish_lifestage_egg(pop, anemfish_params, anem_params)
    
    # Update larval life stage
    pop <- anemonefish_lifestage_larval(pop, anemfish_params, anem_params)
    
    # Update recruit life stage
    pop <- anemonefish_lifestage_recruit(pop, anemfish_params, anem_params)
    
    # Update juvenile life stage 
    pop <- anemonefish_lifestage_juvenile(pop, anemfish_params, anem_params)
    
    # Update adult life stage (includes egg production)
    pop <- anemonefish_lifestage_adult(pop, anemfish_params, anem_params)
  }
  return(pop)
}

# Run the master function:
pop <- anemonefish_population_simulation(1, anemfish_params, anem_params)



# Function to simulate bleaching event  -----------------------------------

# Develop second function to simulate a widespread bleaching event which either results in anemone mortality or reduction in size and health which reduces the carrying capacity of anemonefish. As a result, anemonefish either die or make use of other anemone host species (i.e, relocate). 

# Potentially change the bleaching status to fixed 

# Make a list to combine all the parameters for bleaching events
bleaching_event_params <- list(prob_of_bleaching_event = 0.5,
                               anemone_mortality_threshold = 0.7)

# Function 7: Model the effects of bleaching events on anemonefish population size ---------
simulate_bleaching_event <- function(pop, bleaching_event_params, anem_params) {
  
  # Create new vectors for each of the columns 
  anemone_oral_disc_area <- pop$anemone_oral_disc_area[pop$time_step == 6]
  carrying_capacity <- numeric(anem_params$anemone_pop_size)
  adults <- pop$adults[pop$time_step == 6]
  males <- pop$males[pop$time_step == 6]
  females <- pop$females[pop$time_step == 6]
  
    # Determine severity of bleaching for each anemone using the probability of a bleaching event occurring
    bleaching_severity_anemone = rnorm(anem_params$anemone_pop_size, mean = bleaching_event_params$prob_of_bleaching_event, sd = 0.1)
  
  # Reduce size based on severity of bleaching 
  for (i in 1:anem_params$anemone_pop_size) {
    # Determine the reduction in size if the extent of bleaching is less than the mortality threshold:
    if (bleaching_severity_anemone[i] < bleaching_event_params$anemone_mortality_threshold) {
      anemone_oral_disc_area[i] <- (anemone_oral_disc_area[i] * (1 - bleaching_severity_anemone[i]))
    }
    # If the extent of bleaching is greater than the mortality threshold, the anemone dies:
    else {
      anemone_oral_disc_area[i] <- 0
    }
  
    # Calculate new carrying capacity for each anemone based on size: 
    if (anemone_oral_disc_area[i] <= 500) {
      carrying_capacity[i] <- sample(0:5, 1)
    } else if (anemone_oral_disc_area[i] <= 2000) {
      carrying_capacity[i] <- sample(5:20, 1)
    } else {
      carrying_capacity[i] <- sample(20:25, 1)
    }
    
    # Determine how many fish remain on the anemone after bleaching:
    # Determine the loss of fish inhabitants if the extent of bleaching is less than the mortality threshold:
    if (bleaching_severity_anemone[i] <= bleaching_event_params$anemone_mortality_threshold) {
      adults[i] <- round(adults[i] * (1 - bleaching_severity_anemone[i]))
      # Decrease fecundity based on severity 
      anemfish_params$prob_of_mating <- (anemfish_params$prob_of_mating / 2)
    } else { # If the extent of bleaching is greater than the mortality threshold, all fish die/ relocate:
      adults[i] <- 0
    }
    
    # Update the number of males and females in each anemone:
    if (adults[i] > 0) {
      # Assign new fish to females first, then males if necessary:
      if (pop$females[i] >= adults[i]) {
        females[i] <- adults[i] 
        males[i] <- adults[i] - females[i]
      } else {
        females[i] <- pop$females[i]
        males[i] <- adults[i] - females[i]
      }
    } else {
      # If there are no new fish, set males and females to zero:
      males[i] <- 0
      females[i] <- 0
    }
  }
  
  # Create new dataframe with updated life stage counts
  new_pop <- data.frame(time_step = rep(1, anemone_pop_size),
                        anemone_oral_disc_area = anemone_oral_disc_area,
                        carrying_capacity = carrying_capacity,
                        eggs = rep(0, anemone_pop_size),
                        larvae = rep(0, anemone_pop_size),
                        recruits = rep(0, anemone_pop_size),
                        juveniles = rep(0, anemone_pop_size),
                        adults = adults,
                        females = females,
                        males = males)
  # Return the new dataframe
  return(new_pop)
}

# Run the simulate bleaching event function
new_pop <- simulate_bleaching_event(pop, bleaching_event_params, anem_params)



# Master function - Post-Bleaching --------------------
# Simulate a generation after a bleaching event to determine the effects of reduced fecundity and decreased habitat quality:

simulate_after_bleaching <- function(new_pop, anemfish_params, anem_params) {
  # Specify the initial population 
  init_df <- new_pop
  
  # Loop through each time step
  for (t in 1:time_in_years) {
    # Simulate new generations following bleaching event: 
    # Update egg production
    updated_pop <- anemonefish_lifestage_egg(init_df, anemfish_params, anem_params)
    
    # Update larval life stage
    updated_pop <- anemonefish_lifestage_larval(updated_pop, anemfish_params, anem_params)

    # Update recruit life stage
    updated_pop <- anemonefish_lifestage_recruit(updated_pop, anemfish_params, anem_params)
  
    # Update juvenile life stage 
    updated_pop <- anemonefish_lifestage_juvenile(updated_pop, anemfish_params, anem_params)
    
    # Update adult life stage (includes egg production)
    updated_pop <- anemonefish_lifestage_adult(updated_pop, anemfish_params, anem_params)
  } 
  return(updated_pop)
}

# Run the simulation for after bleaching events function
updated_pop <- simulate_after_bleaching(new_pop, anemfish_params, anem_params)


# Determine the overall population size 
population_size <- data.frame(sum(updated_pop$larvae), sum(updated_pop$recruits), sum(updated_pop$recruits), sum(updated_pop$juveniles), sum(updated_pop$adults))


# Animation and Graphs ------------------------------------------------

# Change the time steps for the bleaching data frame to continue on from 6 
updated_pop <- updated_pop |> 
  mutate(time_step = time_step + 6)

# Bind the pre- and post-bleaching dataframes
df <- rbind(pop, updated_pop)

# Add a column for anemone id 
df$anemone_id <- rep(NA, nrow(df)) # add anemone_id column

for (t in unique(df$time_step)) {
  # Subset data for current time step
  df_subset <- df[df$time_step == t,]
  
  # Assign anemone IDs for current time step
  df_subset$anemone_id <- 1:nrow(df_subset)
  
  # Replace anemone IDs in original data frame
  df[df$time_step == t, "anemone_id"] <- df_subset$anemone_id
}

# Calculate average population size at each time step
avg_pop <- round(aggregate(cbind(eggs, larvae, recruits, juveniles, adults) ~ time_step, data = df, mean))


# Remove the columns that are no longer needed
df <- df[, c("time_step", "anemone_oral_disc_area", "eggs", "larvae", "recruits", "juveniles", "adults")]


# Calculate population size as a sum of each row
avg_pop$population_size <- rowSums(avg_pop[,2:6])

# Graph 1: 
# Plot population size over time using ggplot
ggplot(avg_pop, aes(x = time_step, y = population_size)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:length(avg_pop$time_step)) +
  geom_vline(xintercept = 6.5, linetype = "dashed", color = "red") +
  xlab("Time step") +
  ylab("Anemonefish population size") +
  ggtitle("Population size before and after a bleaching event") + 
  theme_bw() +
  geom_label(aes(label = population_size), vjust = -0.2)


# Animation 1:
# Produce an animation for the change in population size and anemone size 
# Assign the colours for each lifestage 
egg_colour <- "yellow"
larvae_colour <- "orange"
recruit_colour <- "red"
juvenile_colour <- "lightblue"
adult_colour <- "green"

# Loop through time steps
for (t in unique(df$time_step)) {
  set.seed(1) # Set a random seed
  
  # Subset data for current time step
  df_subset <- df[df$time_step == t, ]
  jitter_x <- runif(nrow(df_subset), min = 0, max = 600)
  jitter_y <- runif(nrow(df_subset), min = 0, max = 600)
  
  
  plot(0, xlim=c(0, 700), ylim=c(0, 700), xlab="X", ylab="Y", main = paste("Time Step", t), type="n")
  
  # Set colour for circles based on oral disc area
  circle_colours <- ifelse(df_subset$anemone_oral_disc_area > 10, "purple", "white")
  
  # Add smaller dots for each life stage
  symbols(jitter_x, jitter_y, 
          circles = df_subset$eggs, 
          inches = 0.2, add = TRUE, 
          bg = egg_colour, 
          fg = "black",
          xlim = c(0, 700), ylim = c(0, 700))
  
  symbols(jitter_x, jitter_y, 
          circles = df_subset$larvae, 
          inches = 0.2, add = TRUE, 
          bg = larvae_colour, 
          fg = "black",
          xlim = c(0, 700), ylim = c(0, 700))
  symbols(jitter_x, jitter_y, 
          circles = df_subset$recruits, 
          inches = 0.2, add = TRUE, 
          bg = recruit_colour, 
          fg = "black",
          xlim = c(0, 700), ylim = c(0, 700))
  symbols(jitter_x, jitter_y, 
          circles = df_subset$juveniles, 
          inches = 0.2, add = TRUE, 
          bg = juvenile_colour, 
          fg = "black",
          xlim = c(0, 700), ylim = c(0, 700))
  
  # Only plot adults for time steps 1, 6, 7 and 12 
  if (t == 1 | t == 6 | t == 7 | t == 12) {
    symbols(jitter_x, jitter_y, 
            circles = df_subset$adults, 
            inches = 0.2, add = TRUE, 
            bg = adult_colour, 
            fg = "black",
            xlim = c(0, 700), ylim = c(0, 700))
  }
  
  # Plot anemones with circles and rings for each life stage
  symbols(jitter_x, jitter_y, 
          circles = df_subset$anemone_oral_disc_area, 
          inches = 0.1, add = TRUE, 
          bg = circle_colours, 
          fg = "black",
          xlim = c(0, 700), ylim = c(0, 700))
  
  if (t == 7 | t == 8 | t == 9 | t == 10 | t == 11 | t == 12) {
    box(lwd = 3, col = "red")
  }
  legend("right",
         legend=c("Anemone", "Eggs", "Larvae", "Recruits", "Juveniles", "Adults"), 
         pch=21, cex = 0.7, pt.bg=c("purple", egg_colour, larvae_colour, recruit_colour, juvenile_colour, adult_colour), bty="n")
  
  Sys.sleep(1.0)
}
        

# Parameter screening ---------------------------------------------

# Define the number of simulations
set.seed(123)
n <- 100

# Create an empty list to store the population data frames
total_populations <- list()

# Loop through the range of n simulations
for (i in 1:n) {
  # Access the current value of prob_of_bleaching_event
  bleaching_event_prob <- runif(1)
  
  # Run through the function with different parameter values 
  lhs <- simulate_bleaching_event(pop, 
                                  bleaching_event_params = list(
                                    prob_of_bleaching_event = bleaching_event_prob,
                                    anemone_mortality_threshold = 0.8),
                                  anem_params = list(
                                    anemone_pop_size = 16))
  # Calculate sum for the population for each simulation
  population <- data.frame(param = bleaching_event_prob, 
                           sum(lhs$anemone_oral_disc_area), 
                           sum(lhs$eggs), 
                           sum(lhs$larvae), 
                           sum(lhs$recruits), 
                           sum(lhs$juveniles), 
                           sum(lhs$adults))
  
  names(population) <- c("param", "ODA", "eggs", "larvae", "recruits", "juveniles", "adults")
  
  # Store current population dataframe in a list
  total_populations[[i]] <- population
}

# Print the last population dataframe in the list
print(total_populations)

# Convert list to a dataframe
screen_results <- do.call(rbind, total_populations)

# Find lowest number of adults
which.min(screen_results$adults)

# Graph 2: 
# Plot using ggplot 
ggplot(screen_results, aes(x = param)) +
  geom_point(aes(y = adults), color = "black") +
  xlab("Probability of thermal bleaching for each anemone") +
  ylab("Number of adult inhabitants") +
  theme_bw()

