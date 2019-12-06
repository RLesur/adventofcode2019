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

find_transfers <- function(from = "YOU", to = "SAN", map) {
  from_orbits <- get_orbits(from, map)
  to_orbits <- get_orbits(to, map)
  common_object <- intersect(from_orbits, to_orbits)[1]
  match(common_object, from_orbits) + match(common_object, to_orbits) -2
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
  "K)L",
  "K)YOU",
  "I)SAN"
)

map <- parse_input(test)
get_orbits("YOU", map)
get_orbits("SAN", map)

find_transfers(map = map)

input <- readLines("day-6/input_part-1.txt")
map <- parse_input(input)
find_transfers(map = map)
