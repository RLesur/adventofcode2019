# Part 1 ------------------------------------------------------------------
unit_vector <- function(direction) { # direction is R, U, L, D
  switch (direction,
          "R" = complex(real = 1, imaginary = 0),
          "U" = complex(real = 0, imaginary = 1),
          "L" = complex(real = -1, imaginary = 0),
          "D" = complex(real = 0, imaginary = -1)
  )
}

unidirectional_path <- function(instruction) { # instruction is "R15" or "U18"
  direction <- substr(instruction, 1, 1)
  path_length <- as.integer(substr(instruction, 2, nchar(instruction)))
  rep(unit_vector(direction), path_length)
}

to_unit_vectors <- function(instructions) { # instructions is a vector, ex: c("R15", "U18")
  unlist(lapply(instructions, unidirectional_path))
}

unit_moves_list <- function(raw_instructions) { # raw_instructions is a vector like c("U5,R2", "D1,L3")
  lapply(strsplit(raw_instructions, ","), to_unit_vectors)
}

draw_paths <- function(raw_instructions) {
  origin <- complex(real = 0, imaginary = 0)
  list_of_unit_moves <- unit_moves_list(raw_instructions)
  get_path <- function(x) Reduce("+", x, origin, accumulate = TRUE)
  paths <- lapply(list_of_unit_moves, get_path)
  remove_origin <- function(x) x[-1]
  lapply(paths, remove_origin)
}

get_intersections <- function(raw_instructions) {
  paths <- draw_paths(raw_instructions)
  intersect(paths[[1]], paths[[2]])
}

get_distances <- function(complex_vector) {
  abs(Re(complex_vector)) + abs(Im(complex_vector))
}

minimal_distance <- function(raw_instructions) {
  intersections <- get_intersections(raw_instructions)
  distances <- get_distances(intersections)
  min(distances)
}

# examples
example1 <- c(
  "R75,D30,R83,U83,L12,D49,R71,U7,L72",
  "U62,R66,U55,R34,D71,R55,D58,R83"
)
minimal_distance(example1)

example2 <- c(
  "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
  "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
)
minimal_distance(example2)

raw_instructions <- readLines("day-3/input_part-1.txt")
minimal_distance(raw_instructions)

# Part 2 ------------------------------------------------------------------
get_min_steps <- function(raw_instructions) {
  paths <- draw_paths(raw_instructions)
  intersections <- intersect(paths[[1]], paths[[2]])
  steps1 <- match(intersections, paths[[1]])
  steps2 <- match(intersections, paths[[2]])
  total_steps <- steps1 + steps2
  min(total_steps)  
}
