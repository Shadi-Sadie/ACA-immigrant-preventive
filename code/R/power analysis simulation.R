simulate_data <- function(expansion_df, # data frame with columns (state, year, expansion)
                          state_sizes,   # data frame with columns (state, year, N) for sample sizes
                          beta1, beta2,  # main effect & additional effect for foreign-born
                          sigma = 9      # residual SD
) {
    # For each (state, year), generate state_sizes$N individuals
    big_data <- purrr::pmap_dfr(
        list(state_sizes$state, state_sizes$year, state_sizes$N),
        function(st, yr, n_indiv) {
            # Subset or match the expansion var
            exp_val <- expansion_df %>%
                filter(state == st, year == yr) %>%
                pull(expansion)
            
            # Generate individuals
            df_indiv <- tibble(
                state = st,
                year = yr,
                expansion = exp_val,
                # Covariates
                ForeignBorn = rbinom(n_indiv, 1, 0.15), 
                AGE = rnorm(n_indiv, mean=40, sd=12),
                SEX = rbinom(n_indiv, 1, 0.5),
                # ... other covariates
            )
            return(df_indiv)
        }
    )
    
    # Random intercepts for states and years
    all_states <- unique(big_data$state)
    alpha_state <- rnorm(length(all_states), mean=0, sd=2)
    names(alpha_state) <- all_states
    
    all_years <- unique(big_data$year)
    alpha_year <- rnorm(length(all_years), mean=0, sd=2)
    names(alpha_year) <- all_years
    
    # Add outcome
    big_data <- big_data %>%
        mutate(
            # We can add a simple linear predictor first
            linpred = beta1 * expansion +
                beta2 * (expansion * ForeignBorn) +
                0.1 * AGE +  # small effect
                0.5 * SEX +
                alpha_state[as.character(state)] +
                alpha_year[as.character(year)],
            
            # Then add noise
            HINS1 = linpred + rnorm(nrow(.), 0, sigma)
        )
    
    return(big_data)
}


library(fixest)

est_model <- function(...) {
    d <- simulate_data(...)  # create a simulated dataset
    
    # Fit the real model:
    # HINS1 ~ expansion * ForeignBorn + controls | state + year
    m <- feols(
        HINS1 ~ expansion * ForeignBorn + AGE + SEX |
        state + year, 
        data = d
    )
    
    # Extract p-value for the interaction:
    coefs <- summary(m)$coeftable
    pval <- coefs["expansion:ForeignBorn", "Pr(>|t|)"]  # check the exact row name
    
    return(pval < 0.05)
    
    expansion_df <- Researcher_Dataset %>% select (State, Year, Expansion) %>% rename (state=State, year=Year, expansion=Expansion)
    
    state_sizes <- MEPSFAKE %>%
            group_by(state, YEAR) %>%
             summarise(N = n(), .groups = "drop") %>%
             rename(state = state, year = YEAR) %>%
        mutate(state = as.integer(state), year = as.integer(year))
    
    
library(dplyr)
library(fixest)
library(purrr)
    
    expansion_df <- Researcher_Dataset %>% select (State, Year, Expansion) %>% rename (state=State, year=Year, expansion=Expansion)
    state_sizes <- MEPSFAKE %>%
        group_by(state, YEAR) %>%
        summarise(N = n(), .groups = "drop") %>%
        rename(state = state, year = YEAR)   %>%
        mutate(state = as.integer(state), year = as.integer(year))
    
    

simulate_data <- function(expansion_df, state_sizes, beta1, beta2, sigma = 9) {
    big_data <- purrr::pmap_dfr(
        list(state_sizes$state, state_sizes$year, state_sizes$N),
        function(st, yr, n_indiv) {
            exp_val <- expansion_df %>% 
                filter(state == st, year == yr) %>%
                pull(expansion)
            
            df_indiv <- tibble(
                state = state,
                year = year,
                expansion = exp_val,
                ForeignBorn = rbinom(n_indiv, 1, 0.15),
                AGE = rnorm(n_indiv, 40, 12),
                SEX = rbinom(n_indiv, 1, 0.5)
            )
            df_indiv
        }
    )
    
    all_states <- unique(big_data$state)
    alpha_state <- rnorm(length(all_states), 0, 2)
    names(alpha_state) <- all_states
    
    all_years <- unique(big_data$year)
    alpha_year <- rnorm(length(all_years), 0, 2)
    names(alpha_year) <- all_years
    
    big_data <- big_data %>%
        mutate(
            linpred = beta1 * expansion +
                beta2 * (expansion * ForeignBorn) +
                0.1 * AGE +
                0.5 * SEX +
                alpha_state[state] +
                alpha_year[as.character(year)],
            
            HINS1 = linpred + rnorm(nrow(.), 0, sigma)
        )
    
    return(big_data)
}

est_model <- function(...) {
    d <- simulate_data(...)
    m <- feols(HINS1 ~ expansion * ForeignBorn + AGE + SEX | state + year, data = d)
    coefs <- summary(m)$coeftable
    pval <- coefs["expansion:ForeignBorn", "Pr(>|t|)"]
    return(pval < 0.05)
}

iterate_power <- function(iter, ...) {
    mean(replicate(iter, est_model(...)))
}



# Try a quick test:
power_res <- iterate_power(
    iter = 10,
    expansion_df = expansion_df,
    state_sizes  = state_sizes,
    beta1 = 0.05,
    beta2 = 0.03,
    sigma = 9
)
power_res
