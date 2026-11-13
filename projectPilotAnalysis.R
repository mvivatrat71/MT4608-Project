data = read.csv('pilot_data_combined.csv')
data
# Total seats
seats.entrance <- 257
seats.top <- 367
seats.combined <- seats.entrance + seats.top

data$p <- data$belongings/seats.combined
strata <- c('morning', 'lunch', 'afternoon')
n <- 6
N <- 80

ybar <- c(0,0,0)
nj <- c(2,2,2)
Nj <- c(30,20,30)
Wj <- Nj/N
fj <- nj/Nj
s2 <- c(0,0,0)

# Get stratum sample means and sample variances
for (j in 1:3) {
  stratum_p <- data[data$time_of_day == strata[j], 'p']
  
  ybar[j] <- mean(stratum_p)
  
  s2[j] <- sum((stratum_p - ybar[j])^2)/(nj[j]-1)
}

# Stratified estimator point estimate
mu.st <- sum(ybar*Wj)

# Stratified estimator variance
var.mu.st <- sum(((1-fj)/nj) * Wj^2 * s2)
for (j in 1:3) {
  print(strata[j])
  print(((1-fj[j])/nj[j]) * Wj[j]^2 * s2[j])
}

# SE and Goal SE
se.mu.st <- sqrt(var.mu.st)
goal.se <- se.mu.st/2

writeLines(paste('Point Estimate: ', mu.st))
writeLines(paste('Variance: ', var.mu.st))
writeLines(paste('SE: ', se.mu.st))
writeLines(paste('Goal SE: ', goal.se))

# Required sample size
V0 <- goal.se^2
n.req <- (N*sum(Wj*s2))/(N*V0 + sum(Wj*s2))
writeLines(paste('Required n: ', n.req))

# Proportional allocation
for (j in 1:3) {
  writeLines(paste(strata[j], ':', Wj[j]*n.req))
}
