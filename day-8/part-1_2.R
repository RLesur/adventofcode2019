input <- readLines("day-8/input.txt")

parse_image <- function(input, width = 25L, height = 6L) {
  input <- eval(parse(text = paste0("c(", gsub("(.(?<!$))", "\\1,", input, perl = TRUE), ")")))
  layers <- as.integer(length(input) / (width * height))
  data <- array(input, dim = c(width, height, layers))
}

img <- parse_image(input)

# Part 1
counts_by_layer <- apply(img, 3, table)
target_layer <- which.min(counts_by_layer["0", ])
counts_by_layer["1", target_layer] * counts_by_layer["2", target_layer]

# Part 2
visible_pixel <- function(pixels) {
  head(pixels[pixels != 2], 1)
}

decode_image <- function(image) {
  apply(image, 1:2, visible_pixel)
}

decoded_img <- decode_image(img)

image(decoded_img, col = c("white", "black"), ylim = 1:0, axes = FALSE)
