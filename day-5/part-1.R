# This is my source for day 5 part 1

Program <- R6::R6Class(
  'Program', 
  private = list(
    program = NULL,
    input = NULL,
    instruction_pointer = 0L,
    running = TRUE,
    waiting_for_execution = TRUE
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
      if (identical(opcode, 99L)) return(1L)
      stop("Wrong Opcode")
    },
    get_instruction = function() {
      instruction_length <- self$get_instruction_length()
      instruction_pointer <- self$get_instruction_pointer()
      program <- self$get_program()
      program[seq.int(instruction_pointer + 1, instruction_pointer + instruction_length)]
    },
    move_pointer = function() {
      if (!self$is_running()) stop("Program halted.")
      instruction_length <- self$get_instruction_length()
      private$instruction_pointer <- private$instruction_pointer + instruction_length
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
      if (length(parameters) == 3) {
        parameters <- parameters[1:2]
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
      if (opcode > 2) stop("Opcode must be 1 or 2")
      if (opcode == 1) return(sum)
      if (opcode == 2) return(prod)
    },
    read = function(address) {
      private$program[address + 1]
    },
    write = function(value, address) {
      program <- private$program
      program[address + 1] <- as.integer(value)
      private$program <- program
    },
    execute_instruction = function() {
      stopifnot(private$waiting_for_execution)
      opcode <- self$get_opcode()
      if (opcode == 4) {
        operands <- self$get_operands()
        cat(operands)
      }
      if (opcode == 3) self$write(private$input, self$get_parameters())
      if (opcode >= 1 && opcode <= 2) {
        operands <- self$get_operands()
        operation <- self$get_operation()
        parameters <- self$get_parameters()
        output_address <- parameters[3]
        output_value <- operation(operands)
        self$write(output_value, output_address)
      }
      private$waiting_for_execution <- FALSE
    },
    run = function() {
      while (self$is_running()) {
        self$execute_instruction()
        self$move_pointer()
      }
    }
  )
)

program <- Program$new("4,3,3,4,33", 1)
program$get_instruction()
program$get_instruction_length()
program$get_instruction_pointer()
program$get_opcode()
program$get_parameters()
program$get_parameters_mode()
program$get_program()
program$is_running()
program$execute_instruction()
program$get_program()
program$move_pointer()
program$get_opcode()

code <- readLines("day-5/input.txt")
program <- Program$new(code, 1)
program$run()

