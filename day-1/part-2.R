required_fuel <- function(mass) {
  fuel <- floor(mass / 3) - 2
  fuel <- ifelse(fuel >= 0, fuel, 0)
  if (all(fuel == 0)) return(fuel)
  fuel + required_fuel(fuel)
}

# test
required_fuel(c(14, 1969, 100756))

input <- read.csv("day-1/input_part-2.txt", header = FALSE)[[1]]

sum(required_fuel(input))
