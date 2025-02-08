# Libraries
library(tidyverse)
library(purrr)
library(broom)  # to tidy model output

set.seed(1000)

#--------------------------------------------------------------
# 1) Data creation function
#--------------------------------------------------------------
#
# We'll rename variables to reflect your context:
#   - 'foreign_born' in place of 'girl'
#   - 'expansion' in place of 'Training'
#   - 'HINS1' in place of 'Test'
# 
# We'll also demonstrate adding simple fixed effects for
# state and year by including factor(state) and factor(year)
# in the model. (In a real scenario, you might do it with
# fixest::feols and the | state + year notation.)

create_data <- function(N, main_effect, diff_effect) {
    # N = total number of individuals
    # main_effect = the baseline effect of expansion for US-born
    # diff_effect = how much bigger (or smaller) the effect is for foreign-born
    
    # Generate some covariates
    d <- tibble(
        # A continuous covariate (like 'W' before). 
        # Let's rename it 'age' for demonstration:
        age = rnorm(N, mean = 40, sd = 10),
        
        # A binary covariate (like sex)
        sex = sample(0:1, N, replace = TRUE),
        
        # Foreign-born (like 'girl' before, but renamed)
        foreign_born = sample(0:1, N, replace = TRUE, prob = c(0.85, 0.15)),
        
        # Let's say we have 50 states
        state = sample(1:50, N, replace = TRUE),
        
        # Let's say we have data from 2014 to 2018
        year = sample(2011:2019, N, replace = TRUE)
    )
    
    # Define expansion. In a real setting, you'd tie this to state/year.
    # For a simple example: expansion = 1 if year >= 2016, else 0.
    d <- d %>%
        mutate(expansion = if_else(year >= 2016, 1, 0))
    
    # Now define the outcome, HINS1:
    #   HINS1 = main_effect*expansion
    #           + diff_effect*(foreign_born*expansion)
    #           + some effect of age & sex
    #           + unobserved error
    #           + (optionally) state/year shifts
    # For simplicity, we add random noise with sd=9, like your previous code.
    # If you want state & year FE built-in, you can add random intercepts
    # per state, per year. For demonstration:
    alpha_state <- rnorm(50, mean = 0, sd = 2)  # random intercept by state
    names(alpha_state) <- as.character(1:50)
    
    alpha_year <- rnorm(length(unique(d$year)), mean = 0, sd = 2)
    names(alpha_year) <- as.character(unique(d$year))
    
    d <- d %>%
        mutate(
            HINS1 = main_effect*expansion + 
                diff_effect*foreign_born*expansion + 
                0.2*age +        # small effect of age
                1.0*sex +        # effect of sex
                alpha_state[as.character(state)] + 
                alpha_year[as.character(year)] + 
                rnorm(N, sd = 9) # random noise
        )
    
    return(d)
}

#--------------------------------------------------------------
# 2) Estimation function
#--------------------------------------------------------------
#
# This function:
#   1. Creates a dataset (size = N)
#   2. Fits a model
#   3. Returns a logical: TRUE if the interaction is significant, FALSE otherwise

est_model <- function(N, main_effect, diff_effect) {
    
    d <- create_data(N, main_effect, diff_effect)
    
    # Fit a simple OLS with interaction + controls + fixed effects
    # For demonstration, let's do factor(state) + factor(year).
    # If you use fixest::feols in your real code, you'd do:
    #   feols(HINS1 ~ expansion*foreign_born + sex + age | state + year, data = d)
    #
    # We'll keep it in "lm" to remain close to your example. 
    # But we have to manually include factor(state), factor(year) in the regression:
    
    m <- lm(
        HINS1 ~ expansion*foreign_born + sex + age + factor(state) + factor(year),
        data = d
    )
    
    # Tidy the model
    tidied <- tidy(m)
    
    # Find the row for the interaction term. Usually it's "expansion:foreign_born".
    # (Be mindful of naming differences: if expansion or foreign_born are factors,
    # R might create different labels. If you're uncertain, run once and see the row name.)
    #
    # We'll assume it's "expansion:foreign_born".
    
    interaction_row <- tidied %>% 
        filter(term == "expansion:foreign_born") 
    
    # Check p-value if that row exists
    if (nrow(interaction_row) == 0) {
        # If we can't find the row (naming mismatch), you might do a partial match:
        # But for simplicity, let's return NA
        return(FALSE)
    }
    
    pval <- interaction_row$p.value
    sig <- pval < 0.05
    
    return(sig)
}

#--------------------------------------------------------------
# 3) Iteration function to estimate power
#--------------------------------------------------------------
iterate <- function(N, main_effect, diff_effect, iters = 500) {
    
    results <- 1:iters %>%
        map_dbl(function(x) {
            # Just to track progress every 100 reps
            if (x %% 100 == 0) cat("Iteration:", x, "\n")
            
            # Run our model and return whether interaction was significant
            return(est_model(N, main_effect, diff_effect))
        })
    
    # Proportion of significant results = power
    mean(results)
}

#--------------------------------------------------------------
# 4) Example: Minimum Sample Size
#--------------------------------------------------------------
#
# Let's see how power changes with sample size for:
#   main_effect = 2
#   diff_effect = 0.8
#
# We'll pick a few N's and see which crosses, say, 90% power.

possible_N <- c(10000, 15000, 20000, 25000)
mss <- tibble(N = possible_N)

mss$power <- map_dbl(possible_N, ~ iterate(N = .x, 
                                           main_effect = 2, 
                                           diff_effect = 0.8, 
                                           iters = 500))

mss
# Look for the first N with power >= 0.90

#--------------------------------------------------------------
# 5) Example: Minimum Detectable Effect
#--------------------------------------------------------------
#
# Now fix N = 2000, vary 'diff_effect' and see power.
# We'll keep main_effect = 2 (the effect of expansion for US-born).
# diff_effect is the additional effect for foreign-born.

possible_diff <- c(0.8, 1.6, 2.4, 3.2)
mde <- tibble(diff_effect = possible_diff)

mde$power <- map_dbl(possible_diff, ~ iterate(N = 2000, 
                                              main_effect = 2, 
                                              diff_effect = .x, 
                                              iters = 500))

mde
# See which diff_effect first gets you >= 0.90 power

#--------------------------------------------------------------
# Optional: install pwr package
#--------------------------------------------------------------
# install.packages("pwr", repos="http://cran.r-project.org")
# library(pwr)
