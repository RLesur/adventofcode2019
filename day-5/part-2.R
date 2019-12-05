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
    execute_instruction = function() {
      stopifnot(private$waiting_for_execution)
      opcode <- self$get_opcode()
      if (opcode == 4) {
        operands <- self$get_operands()
        cat(operands)
        self$move_pointer()
      }
      if (opcode == 3) {
        self$write(private$input, self$get_parameters())
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
      while (self$is_running()) {
        self$execute_instruction()
      }
    }
  )
)

program <- Program$new("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", 9)
program$run()

code <- readLines("day-5/input.txt")
program <- Program$new(code, 5)
program$run()
