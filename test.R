# Function to put quotes around everything in list
q <- function(...) {
  s <- sys.call()[-1]
  w <- as.character(s)
  n <- names(s)
  if(length(n)) names(w) <- n
  w
}
