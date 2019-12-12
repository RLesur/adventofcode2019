Moon <- R6::R6Class(
  "Moon",
  private = list(
    pos = c(),
    vel = c(x = 0, y = 0, z = 0)
  ),
  public = list(
    initialize = function(x, y ,z) {
      private$pos <- c(x = x, y = y, z = z)
    },
    print = function() {
      cat("pos=<", paste(c("x", "y", "z"), private$pos, sep = "=", collapse = ", "), ">, ",
          "vel=<", paste(c("x", "y", "z"), private$vel, sep = "=", collapse = ", "), ">\n",
          sep = "")
    },
    get_position = function() {
      private$pos
    },
    get_velocity = function() {
      private$vel
    },
    set_velocity = function(vel) {
      stopifnot(identical(names(vel), c("x", "y", "z")))
      private$vel <- vel
    },
    apply_velocity = function() {
      private$pos <- private$pos + private$vel
    }
  ),
  active = list(
    potential_energy = function() {
      sum(abs(private$pos))
    },
    kinetic_energy = function() {
      sum(abs(private$vel))
    },
    total_energy = function() {
      self$potential_energy * self$kinetic_energy
    }
  )
)

System <- R6::R6Class(
  "System",
  private = list(
    moons = list(),
    time = 0
  ),
  public = list(
    initialize = function(...) {
      moons <- list(...)
      lapply(moons, function(x) stopifnot("Moon" %in% class(x)))
      private$moons <- moons
    },
    print = function() {
      lapply(private$moons, function(x) x$print())
    },
    get_moons = function() {
      private$moons
    },
    apply_gravity = function() {
      combs <- gtools::combinations(length(private$moons), 2)
      lapply(seq.int(1, length.out = nrow(combs)), function(i) {
        indexes <- combs[i, ]
        moonA <- private$moons[[indexes[1]]]
        moonB <- private$moons[[indexes[2]]]
        posA <- moonA$get_position()
        posB <- moonB$get_position()
        delta_velA <- ifelse(posA > posB, -1, ifelse(posA < posB, 1, 0))
        moonA$set_velocity(moonA$get_velocity() + delta_velA)
        moonB$set_velocity(moonB$get_velocity() - delta_velA)
      })
      self
    },
    apply_velocity = function() {
      lapply(private$moons, function(x) x$apply_velocity())
      self
    },
    increase_time = function() {
      private$time <- private$time + 1
      self
    },
    get_time = function() {
      private$time
    },
    apply_single_step = function() {
      self$apply_gravity()
      self$apply_velocity()
      self$increase_time()
    },
    apply_turns = function(turns = 1) {
      for (i in seq.int(1, length.out = turns)) {
        self$apply_single_step()
      }
      self
    }
  ),
  active = list(
    total_energy = function() {
      sum(vapply(private$moons, function(x) x$total_energy, double(1)))
    }
  )
)

# example 1
moon1 <- Moon$new(x=-1, y=0, z=2)
moon2 <- Moon$new(x=2, y=-10, z=-7)
moon3 <- Moon$new(x=4, y=-8, z=8)
moon4 <- Moon$new(x=3, y=5, z=-1)
system <- System$new(moon1, moon2, moon3, moon4)
system$apply_turns(10)
system$total_energy

# example 2
moon1 <- Moon$new(x=-8, y=-10, z=0)
moon2 <- Moon$new(x=5, y=5, z=10)
moon3 <- Moon$new(x=2, y=-7, z=3)
moon4 <- Moon$new(x=9, y=-8, z=-3)
system <- System$new(moon1, moon2, moon3, moon4)
system$apply_turns(100)
system$total_energy

# game
moon1 <- Moon$new(x=-7, y=-8, z=9)
moon2 <- Moon$new(x=-12, y=-3, z=-4)
moon3 <- Moon$new(x=6, y=-17, z=-9)
moon4 <- Moon$new(x=4, y=-10, z=-6)
system <- System$new(moon1, moon2, moon3, moon4)
system$apply_turns(1000)
system$total_energy
