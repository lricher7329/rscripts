# Function to put quotes around everything in list
q <- function(...) {
  s <- sys.call()[-1]
  w <- as.character(s)
  n <- names(s)
  if(length(n)) names(w) <- n
  w
}

# Function to convert yes no factor variable
yn <- function(x) {
  factor(x, 0:1, c('yes', 'no'))
  }
