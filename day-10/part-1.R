lines_to_df <- function(lines) {
  parsed <- gregexpr("#", lines, fixed = TRUE)
  
  x_coords <- function(v) {
    out <- as.vector(v)
    if (identical(out, -1L)) return(integer(0))
    out - 1L
  }
  
  data <- lapply(parsed, x_coords)
  
  dfs <- lapply(seq.int(0, length.out = length(data)), function(i) {
    x <- data[[i + 1]]
    y <- rep(i, length(x))
    data.frame(x = x, y = y)
  })
  
  do.call(rbind, dfs)
}

max_detected <- function(lines) {
  df <- lines_to_df(lines)
  
  sightable_from <- function(i, df) {
    remaining_asteroids <- df[-i,]
    asteroid <- unlist(df[i,])
    cplx <- complex(real = remaining_asteroids$x - asteroid["x"], imaginary = remaining_asteroids$y - asteroid["y"])
    length(unique(Arg(cplx)))
  } 
  
  sights <- vapply(seq.int(1, nrow(df)), sightable_from, FUN.VALUE = integer(1), df = df)
  max(sights)
}

lines <- readLines("day-10/input.txt")
max_detected(lines)

# tests
read_example <- function(ex) {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  cat(ex, "\n", file = f)
  readLines(f)
}

ex1 <- 
".#..#
.....
#####
....#
...##"

lines <- read_example(ex1)
lines_to_df(lines)

