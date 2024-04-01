post5 <- function() {
######################################################
## Additional processing for coverage rate and RMSE ##
######################################################
size <- 47
rcov11 <- mat.or.vec(10, size)

re11 <- mat.or.vec(10, size)


for (i in 1:10) {
  rcov11[i, ] <- as.numeric((r1[i, ] >= rl11[i, ]) & (r1[i, ] <= ru11[i, ]))

  re11[i, ] <- (rm11[i, ] - r1[i, ])^2

}

rcov11 <- apply(rcov11, 2, sum) / 10

re11 <- sqrt(apply(re11, 2, mean))


date <- rep(34:80, times = 24)
result <- c(
  rcov11,
  re11
)

### 94??? 47 * 2

scenario <- rep(1, each = 47, times = 2)
model <- rep(1, each = 47, times = 2)
type <- rep(c("coverage", "RMSE"), each = 47)
rout1 <- data.frame(cbind(date, result, scenario, model, type))
eva1 <- list(c = cout1, r = rout1)
saveRDS(eva1, "eva1.rds")
}
