el <- function() {
  x <- matrix(1:12, byrow = TRUE, ncol = 4)
  d <- edgelist(x)
  d <- subset(d, !is.na(to))
  expected <- data.frame(
    from = c(
      rep(1, 3), rep(2, 5), rep(3, 5), rep(4, 3),
      rep(5, 5), rep(6, 8),
      rep(7, 8), rep(8, 5),
      rep(9, 3), rep(10, 5), rep(11, 5),
      rep(12, 3)
    ),
    to = c(
      2, 5, 6, 1, 3, 5, 6, 7, 2, 4, 6, 7, 8, 3, 7, 8,
      1, 2, 6, 9, 10, 1, 2, 3, 5, 7, 9, 10, 11,
      2, 3, 4, 6, 8, 10, 11, 12, 3, 4, 7, 11, 12,
      5, 6, 10, 5, 6, 7, 9, 11, 6, 7, 8, 10, 12,
      7, 8, 11
    )
  )
  
  return(list(
    x = x,
    d = d,
    exp = expected
  ))
}