# day 9
IntcodeComputer <- R6::R6Class(
  'IntcodeComputer', 
  private = list(
    memory = character(0), # a named character vector (names: addresses, ints are stored as character)
    input = c(), # a vector of integers stored as character
    instruction_pointer = gmp::as.bigz(0L),
    running = TRUE,
    paused = FALSE,
    waiting_for_execution = TRUE,
    relative_base = 0L,
    output = NULL, # a string (comma separated integers)
    .last_output = NULL # a string (comma separated integers)
  ),
  public = list(
    initialize = function(program, input) {
      if (!missing(input)) {
        private$input <- as.character(input)
      }
      memory <- strsplit(program, ",")[[1]]
      names(memory) <- seq.int(0, length(memory) - 1)
      private$memory <- memory
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
    get_memory = function() {
      private$memory
    },
    get_instruction_pointer = function() {
      private$instruction_pointer
    },
    get_opcode = function() {
      code <- self$read_memory(self$get_instruction_pointer())
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
      if (identical(opcode, 9L)) return(2L)
      if (identical(opcode, 99L)) return(1L)
      stop("Wrong Opcode")
    },
    get_instruction = function() {
      instruction_length <- self$get_instruction_length()
      instruction_pointer <- self$get_instruction_pointer()
      self$read_block_memory(address = instruction_pointer, length = instruction_length)
    },
    move_pointer = function(address) {
      if (!self$is_running()) stop("Computer halted.")
      if (self$is_paused()) stop("Computer paused.")
      if (missing(address)) {
        instruction_length <- self$get_instruction_length()
        private$instruction_pointer <- gmp::as.bigz(private$instruction_pointer + instruction_length)
      } else {
        private$instruction_pointer <- gmp::as.bigz(address)
      }
      opcode <- self$get_opcode()
      if (opcode == 99) {
        private$running <- FALSE
        private$waiting_for_execution <- FALSE
      } else {
        private$running <- TRUE
        private$waiting_for_execution <- TRUE
      }
    },
    get_parameters_mode = function() {
      modes <- floor(as.integer(self$read_memory(self$get_instruction_pointer())) / 100)
      n_parameters <- self$get_instruction_length() - 1L
      if (n_parameters == 0) return(integer(0))
      vapply(
        seq.int(1, n_parameters), 
        function(i) {
          modes <- floor(modes / 10^(i-1))
          res <- as.integer(modes %% 10)
          if (res != 0 & res != 1 & res != 2) {
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
        if (parameters_mode[i] == 0) return(self$read_memory(parameters[i]))
        if (parameters_mode[i] == 2) return(self$read_memory(parameters[i] + private$relative_base))
        stop("got param mode ", parameters_mode[i])
      }
      do.call(c, lapply(seq.int(1, length(parameters)), get_value))
    },
    get_write_address = function() {
      parameter <- tail(self$get_parameters(), 1)
      parameter_mode <- tail(self$get_parameters_mode(), 1)
      if (parameter_mode == 1) stop("Write instruction cannot be in immediate mode.")
      if (parameter_mode == 0) return(parameter)
      if (parameter_mode == 2) return(parameter + private$relative_base)
    },
    get_operation = function() {
      opcode <- self$get_opcode()
      if (!(opcode %in% c(1:2, 5:9))) stop("Opcode must be 1, 2, 5, 6, 7, 8, 9")
      if (opcode == 1) return(function(x) x[1] + x[2])
      if (opcode == 2) return(function(x) x[1] * x[2])
      if (opcode == 5) return(function(x) x[1] != 0)
      if (opcode == 6) return(function(x) x[1] == 0)
      if (opcode == 7) return(function(x) as.integer(x[1] < x[2]))
      if (opcode == 8) return(function(x) as.integer(x[1] == x[2]))
      if (opcode == 9) return(function(x) x[1] + private$relative_base)
    },
    read_memory = function(address) {
      if (length(address) > 1) {
        stop("Method read_memory cannot read a block, use read_block_memory instead.")
      }
      address <- gmp::as.bigz(address)
      if (address < 0) {
        stop("Wrong address memory: negative address.")
      }
      address <- as.character(address)
      value <- gmp::as.bigz(private$memory[address])
      if (is.na(value)) {
        value <- gmp::as.bigz(0L)
      }
      value
    },
    read_block_memory = function(address, length) {
      address <- gmp::as.bigz(address)
      addresses <- address + seq.int(0, length.out = as.integer(length))
      read <- self$read_memory
      do.call(c, lapply(addresses, read))
    },
    write_memory = function(value, address) {
      memory <- private$memory
      memory[as.character(address)] <- as.character(value)
      private$memory <- memory
    },
    write_output = function(value) {
      #cat("output: ", as.character(value), "\n")
      private$output <- paste(c(private$output, as.character(value)), collapse = ",")
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
      gmp::as.bigz(input[1])
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
        self$write_memory(input, self$get_write_address())
        self$move_pointer()
      }
      if (opcode %in% c(1:2, 7:8)) {
        operands <- self$get_operands()
        operation <- self$get_operation()
        output_value <- operation(operands)
        self$write_memory(output_value, self$get_write_address())
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
      if (opcode == 9) {
        operands <- self$get_operands()
        operation <- self$get_operation()
        private$relative_base <- operation(operands)
        self$move_pointer()
      }
    },
    run = function() {
      if (!self$is_running()) stop("Cannot run: computer halted.")
      while (self$is_running() && !self$is_paused()) {
        self$execute_instruction()
      }
      self$get_output()
    },
    resume = function(input) {
      if (!self$is_running()) stop("Cannot resume: not running.")
      if (!self$is_paused()) stop("Cannot resume: not paused.")
      private$input <- c(private$input, as.character(input))
      private$paused <- FALSE
      self$run()
    }
  )
)

program <- readLines("day-13/input.txt")
computer <- IntcodeComputer$new(program)
out <- computer$run()
writeLines(out, "day-13/out.txt")

data <- as.integer(strsplit(out, ",")[[1]])
data <- matrix(data = data, ncol = 3, byrow = TRUE)
colnames(data) <- c("x", "y", "tile_id")
data <- as.data.frame(data)
nrow(data[data$tile_id == 2, ])
