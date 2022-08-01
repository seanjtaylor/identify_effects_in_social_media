outcome.levels <- c('Up-vote', 'Down-vote', 'Reply')
condition.levels <- c('Identified', 'Anonymous')
covariates <- c('Production', 'Status', 'Reciprocity')

nlmh.labels <- c('None', 'Low', 'Medium', 'High')
mlh.labels <- c('Medium', 'Low', 'High')
prod.labels <- paste(' =', mlh.labels)
status.labels <- paste(' =', mlh.labels)
recip.labels <- paste(' =', nlmh.labels)

the_lag <- 3
