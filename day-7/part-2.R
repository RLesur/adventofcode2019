Amplifier <- R6::R6Class(
  'Amplifier', 
  private = list(
    program = NULL,
    input = NULL, # a vector
    instruction_pointer = 0L,
    running = TRUE,
    paused = FALSE,
    waiting_for_execution = TRUE,
    output = NULL, # an integer
    .last_output = NULL # an integer
  ),
  public = list(
    initialize = function(program, input) {
      program <- eval(parse(text = paste0("c(", program, ")")))
      private$input <- input
      private$program <- as.integer(program)
    },
    is_running = function() {
      private$running
    },
    pause = function() {
      private$paused <- TRUE
    },
    is_paused = function() {
      private$paused
    },
    get_program = function() {
      private$program
    },
    get_instruction_pointer = function() {
      private$instruction_pointer
    },
    get_opcode = function() { 
      code <- private$program[self$get_instruction_pointer() + 1]
      as.integer(code %% 100)
    },
    get_instruction_length = function() {
      opcode <- self$get_opcode()
      if (identical(opcode, 1L)) return(4L)
      if (identical(opcode, 2L)) return(4L)
      if (identical(opcode, 3L)) return(2L)
      if (identical(opcode, 4L)) return(2L)
      if (identical(opcode, 5L)) return(3L)
      if (identical(opcode, 6L)) return(3L)
      if (identical(opcode, 7L)) return(4L)
      if (identical(opcode, 8L)) return(4L)
      if (identical(opcode, 99L)) return(1L)
      stop("Wrong Opcode")
    },
    get_instruction = function() {
      instruction_length <- self$get_instruction_length()
      instruction_pointer <- self$get_instruction_pointer()
      program <- self$get_program()
      program[seq.int(instruction_pointer + 1, instruction_pointer + instruction_length)]
    },
    move_pointer = function(address) {
      if (!self$is_running()) stop("Program halted.")
      if (self$is_paused()) stop("Program paused.")
      if (missing(address)) {
        instruction_length <- self$get_instruction_length()
        private$instruction_pointer <- private$instruction_pointer + instruction_length
      } else {
        private$instruction_pointer <- address
      }
      opcode <- self$get_opcode()
      if (identical(opcode, 99L)) {
        private$running <- FALSE
        private$waiting_for_execution <- FALSE
      } else {
        private$running <- TRUE
        private$waiting_for_execution <- TRUE
      }
    },
    get_parameters_mode = function() {
      modes <- floor(private$program[self$get_instruction_pointer() + 1] / 100)
      n_parameters <- self$get_instruction_length() - 1L
      if (n_parameters == 0) return(integer(0))
      vapply(
        seq.int(1, n_parameters), 
        function(i) {
          modes <- floor(modes / 10^(i-1))
          res <- as.integer(modes %% 10)
          if (res != 0 & res != 1) {
            stop("Found value ", res)
          }
          res
        }, 
        FUN.VALUE = integer(1), USE.NAMES = FALSE
      )
    },
    get_parameters = function() {
      self$get_instruction()[-1]
    },
    get_operands = function() {
      parameters <- self$get_parameters()
      parameters_mode <- self$get_parameters_mode()
      if (length(parameters) >= 3) {
        parameters <- head(parameters, -1)
      }
      get_value <- function(i) {
        if (parameters_mode[i] == 1) return(parameters[i]) 
        if (parameters_mode[i] == 0) return(self$read(parameters[i]))
        stop("got param mode ", parameters_mode[i])
      }
      vapply(seq.int(1, length(parameters)), get_value, FUN.VALUE = integer(1), USE.NAMES = FALSE)
    },
    get_operation = function() {
      opcode <- self$get_opcode()
      if (!(opcode %in% c(1:2, 5:8))) stop("Opcode must be 1, 2, 5, 6, 7, 8")
      if (opcode == 1) return(sum)
      if (opcode == 2) return(prod)
      if (opcode == 5) return(function(x) x[1] != 0)
      if (opcode == 6) return(function(x) x[1] == 0)
      if (opcode == 7) return(function(x) x[1] < x[2])
      if (opcode == 8) return(function(x) x[1] == x[2])
    },
    read = function(address) {
      private$program[address + 1]
    },
    write = function(value, address) {
      program <- private$program
      program[address + 1] <- as.integer(value)
      private$program <- program
    },
    write_output = function(value) {
      output_string <- paste0(c(private$output, value), collapse = "")
      private$output <- as.integer(output_string)
    },
    get_output = function() {
      private$.last_output <- private$output
      private$output <- NULL
      private$.last_output
    },
    get_last_output = function() {
      private$.last_output
    },
    read_input = function() {
      input <- private$input
      if (identical(length(input), 0L)) {
        self$pause()
        return()
      }
      private$input <- tail(input, -1)
      as.integer(input[1])
    },
    execute_instruction = function() {
      stopifnot(private$waiting_for_execution)
      opcode <- self$get_opcode()
      if (opcode == 4) {
        operands <- self$get_operands()
        self$write_output(operands)
        self$move_pointer()
      }
      if (opcode == 3) {
        input <- self$read_input()
        if (self$is_paused()) return()
        self$write(input, self$get_parameters())
        self$move_pointer()
      }
      if (opcode %in% c(1:2, 7:8)) {
        operands <- self$get_operands()
        operation <- self$get_operation()
        output_value <- operation(operands)
        parameters <- self$get_parameters()
        output_address <- parameters[3]
        self$write(output_value, output_address)
        self$move_pointer()
      }
      if (opcode %in% 5:6) {
        operands <- self$get_operands()
        operation <- self$get_operation()
        test <- operation(operands)
        pointer_address <- operands[2]
        if (isTRUE(test))
          self$move_pointer(pointer_address)
        else
          self$move_pointer()
      }
    },
    run = function() {
      if (!self$is_running()) stop("Cannot run: program halted.")
      while (self$is_running() && !self$is_paused()) {
        self$execute_instruction()
      }
      self$get_output()
    },
    resume = function(input) {
      if (!self$is_running()) stop("Cannot resume: not running.")
      if (!self$is_paused()) stop("Cannot resume: not paused.")
      private$input <- c(private$input, input)
      private$paused <- FALSE
      self$run()
    }
  )
)

FeedbackLoop <- R6::R6Class(
  private = list(
    signal = NULL,
    amplifier_to_feed = NULL
  ),
  public = list(
    initialize = function(program, phase_settings) {
      force(phase_settings)
      private$signal <- 0L
      private$amplifier_to_feed <- 1
      names(phase_settings) <- paste("amplifier", LETTERS[seq.int(1, length(phase_settings))], sep = "_")
      self$amplifiers <- lapply(phase_settings, function(x) {
        amplifier <- Amplifier$new(program, x)
        cat(amplifier$run())
        amplifier
      })
    },
    amplifiers = list(),
    can_resume = function() {
      self$amplifiers[[private$amplifier_to_feed]]$is_running()
    },
    resume_next_amplifier = function() {
      amplifier <- self$amplifiers[[private$amplifier_to_feed]]
      private$amplifier_to_feed <- private$amplifier_to_feed %% 5 + 1
      private$signal <- amplifier$resume(private$signal)
    },
    get_output = function() {
      self$amplifiers[[5]]$get_last_output()
    },
    run = function() {
      while (self$can_resume()) {
        self$resume_next_amplifier()
      }
      self$get_output()
    }
  )
)

thruster_signal <- function(phase_settings, program) {
  loop <- FeedbackLoop$new(program, phase_settings)
  loop$run()
}

# tests
program <- "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
phase_settings <- 9:5
thruster_signal(phase_settings, program)

program <- "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
phase_settings <- c(9L, 7L, 8L, 5L, 6L)
thruster_signal(phase_settings, program)

max_thruster_signal <- function(program) {
  permutations <- gtools::permutations(5, 5, 5:9)
  n_permutations <- nrow(permutations)
  get_thruster_signal <- function(index, program) {
    phase_settings <- permutations[index,]
    thruster_signal(phase_settings, program)
  }
  res <- vapply(seq.int(1, n_permutations), get_thruster_signal, FUN.VALUE = integer(1), program = program)
  max(res)
}

# test
max_thruster_signal("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
max_thruster_signal("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")

# game
program <- readLines("day-7/input.txt")
max_thruster_signal(program)
