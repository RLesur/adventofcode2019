delta_vel <- function(moons) {
  vapply(1:4, function(i) {
    sum(moons$pos > moons$pos[i]) - sum(moons$pos < moons$pos[i])
  }, double(1))
}

apply_turn <- function(moons) {
  moons$vel <- moons$vel + delta_vel(moons)
  moons$pos <- moons$pos + moons$vel
  moons
}

find_period <- function(moons) {
  n <- 1
  next_pos <- apply_turn(moons)
  while (!identical(next_pos, moons) && n <= 1000000) {
    n <- n + 1
    next_pos <- apply_turn(next_pos)
  }
  stopifnot(n <= 1000000)
  return(n)
}

# game 
x <- list(pos = c(-7, -12, 6, 4), vel = c(0, 0, 0, 0)) 
y <- list(pos = c(-8, -3, -17, -10), vel = c(0, 0, 0, 0))
z <- list(pos = c(9, -4, -9, -6), vel = c(0, 0, 0, 0))
x_period <- find_period(x) # 186028
y_period <- find_period(y) # 28482
z_period <- find_period(z) # 231614

library(gmp)
lcm.bigz(lcm.bigz(x_period, y_period), z_period)
