library(tidyverse); library(purrr); library(broom)
set.seed(1000)

# Follow the description in the text for data creation. Since we want 
# to get minimum sample size and minimum detectable effect, allow both
# sample size and effect to vary.
# diff is the difference in effects between boys and girls
create_data <- function(N, effect, diff) {
    d <- tibble(W = rnorm(N),
                girl = sample(0:1, N, replace = TRUE)) %>%
        # A one-SD change in W makes treatment 10% less likely
        mutate(Training = runif(N) + .1*W < .5) %>%
        mutate(Test = effect*Training + diff*girl*Training + 
                   4*W + rnorm(N, sd = 9))
    
    return(d)
}

# Our estimation function
est_model <- function(N, effect, diff) {
    d <- create_data(N, effect, diff)
    
    # Our model
    m <- lm(Test~girl*Training + W, data = d)
    tidied <- tidy(m)
    
    # By looking we can spot that the interaction
    # term is in the 5th row
    sig <- tidied$p.value[5] < .05
    
    return(sig)
}

# Iteration function!
iterate <- function(N, effect, diff, iters) {
    results <-  1:iters %>%
        map_dbl(function(x) {
            # To keep track of progress
            if (x %% 100 == 0) {print(x)}
            
            # Run our model and return the result
            return(est_model(N, effect, diff))
        })
    
    # We want to know statistical power, 
    # i.e., the proportion of significant results
    return(mean(results))
}

# Let's find the minimum sample size
mss <- tibble(N = c(10000, 15000, 20000, 25000))
mss$power <- c(10000, 15000, 20000, 25000) %>%
    # Before we had to do function(x) here, but now the argument we're 
    # passing is the first argument of iterate() so we don't need it
    map_dbl(iterate, effect = 2, diff = .8, iter = 500)
# Look for the first N with power above 90%
mss

# Now for the minimum detectable effect
mde <- tibble(effect = c(.8, 1.6, 2.4, 3.2))
mde$power <-  c(.8, 1.6, 2.4, 3.2) %>%
    map_dbl(function(x) iterate(N = 2000, effect = 2,
                                diff = x, iter = 500))
# Look for the first effect wth power above 90%
mde




install.packages("pwr", repos="http://cran.r-project.org")
library(pwr)

