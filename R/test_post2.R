post2 <- function() {
  ######################################################
  ## Additional processing for coverage rate and RMSE ##
  ######################################################

  ccov11 <- mat.or.vec(10, 80)

  ce11 <- mat.or.vec(10, 80)

  for (i in 1:10) {

    # is true within the coverage interval?
    ccov11[i, ] <- as.numeric((c1[i, ] >= cl11[i, ]) & (c1[i, ] <= cu11[i, ]))

    # comparing medians to true for RMSE
    ce11[i, ] <- (cm11[i, ] - c1[i, ])^2

  }

  # applying over all 10 simulations
  ccov11 <- apply(ccov11, 2, sum) / 10

  # finishing the RSME calcuatlion
  ce11 <- sqrt(apply(ce11, 2, mean))

  # 80? #^^ length of the above
  # 24? #^^ scenarios: 6 * 2 * 2?
  date <- rep(1:80, times = 2)
  result <- c(
    ccov11,
    ce11
  )

  # 160?
  # 2?
  scenario <- rep(1, each = 160)
  model <- rep(1, each = 160)

  type <- rep(c("coverage", "RMSE"), each = 960)

  cout1 <- data.frame(cbind(date, result, scenario, model, type))

}
