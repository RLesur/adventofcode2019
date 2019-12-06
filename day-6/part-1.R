parse_input <- function(input) {
  parsed <- strsplit(input, ")")
  res <- vapply(parsed, function(x) x[1], FUN.VALUE = character(1), USE.NAMES = FALSE)
  names(res) <- vapply(parsed, function(x) x[2], FUN.VALUE = character(1), USE.NAMES = FALSE)
  res
}

get_orbits <- function(object, map) {
  around <- map[object]
  if (is.na(around)) return(c())
  if (around == "COM") 
    return(c(around))
  else
    return(c(around, get_orbits(around,map)))
}

count_orbits <- function(object, map) {
  length(get_orbits(object, map))
}

total_orbits <- function(map) {
  counts <- vapply(names(map), count_orbits, FUN.VALUE = integer(1), map = map, USE.NAMES = FALSE)
  sum(counts)
}

test <- c(
  "COM)B",
  "B)C",
  "C)D",
  "D)E",
  "E)F",
  "B)G",
  "G)H",
  "D)I",
  "E)J",
  "J)K",
  "K)L"
)

map <- parse_input(test)
get_orbits("D", map)
get_orbits("L", map)
get_orbits("COM", map)
total_orbits(map)

input <- readLines("day-6/input_part-1.txt")
map <- parse_input(input)
total_orbits(map)
