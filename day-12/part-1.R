Moon <- R6::R6Class(
  "Moon",
  private = list(
    pos = c(),
    vel = c(x = 0, y = 0, z = 0)
  ),
  public = list(
    initialize = function(x, y ,z) {
      private$pos <- c(x = x, y = y, z = z)
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
      self$potential_energy + self$kinetic_energy
    }
  )
)

System <- R6::R6Class(
  "System",
  private = list(
    moons = list()
  ),
  public = list(
    initialize = function(...) {
      moons <- list(...)
      lapply(moons, function(x) stopifnot("Moon" %in% class(x)))
      private$moons <- moons
    }
  )
)

# test
moon1 <- Moon$new(x=-1, y=0, z=2)
moon2 <- Moon$new(x=2, y=-10, z=-7)
moon3 <- Moon$new(x=4, y=-8, z=8)
moon4 <- Moon$new(x=3, y=5, z=-1)
system <- System$new(moon1, moon2, moon3, moon4)
