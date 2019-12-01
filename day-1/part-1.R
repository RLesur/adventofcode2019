required_fuel <- function(mass) {
  sum(floor(mass / 3) - 2)
}

input <- read.csv("day-1/input_part-1.txt", header = FALSE)

required_fuel(input)
