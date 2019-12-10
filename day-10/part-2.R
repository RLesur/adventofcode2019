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
  cat("Maximum asteroids detected: ", max(sights), "\n")
  index <- which.max(sights)
  unlist(df[index,])
}

relative_map <- function(lines, laser_position) {
  df <- lines_to_df(lines)
  if (missing(laser_position)) {
    laser_position <- max_detected(lines)  
  }
  df$x_rel <- df$x - laser_position["x"]
  df$y_rel <- df$y - laser_position["y"]
  df$c <- complex(real = df$x_rel, imaginary = df$y_rel)
  df$arg <- Arg(df$c)
  df$mod <- Mod(df$c)
  df$angle <- ifelse(df$arg < 0 & df$x_rel < 0, 5*pi/2 + df$arg, pi/2 + df$arg)
  
  df <- df[order(df$angle, df$mod), ]
  turn <- unlist(lapply(unique(df$angle), function(x) {
    cumsum(rep(1, nrow(df[df$angle == x, ])))
  }))
  df$turn <- turn
  
  df[order(df$turn, df$angle),]
}

# test 
read_example <- function(ex) {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  cat(ex, "\n", file = f)
  readLines(f)
}

ex2 <- 
".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"

lines <- read_example(ex2)
relative_map(lines)[200,]

# Game
lines <- readLines("day-10/input.txt")
relative_map(lines)[200,]
