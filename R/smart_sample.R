# Wrapper function for sample
# If the sample size is larger than the population, returns the input vector
# Otherwise returns the result of sampling
# x: a vector to sample from
# sized: the size of the sample
# ...: additional arguments to pass to sample

# Value: a vector sampled from x
smart_sample <- function(x, size, ...) {
    if(size > length(x)) {
        return(x)
    }
    else {
        return(sample(x, size, ...))
    }
}