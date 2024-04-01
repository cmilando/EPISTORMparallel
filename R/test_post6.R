post6 <- function() {
################################
## Prepare output for figures ##
################################

rl11 <- apply(rl11, 2, mean)

rm11 <- apply(rm11, 2, mean)

ru11 <- apply(ru11, 2, mean)


r1 <- apply(r1, 2, mean)


rr1 <- c(rep(NA, 3), apply(rr1, 2, mean))

scenarios <- c(
  "Single Delay Dist. & Correct Maximum Delay")

type <- c("1 Dispersion")

low <- c(rl11)
med <- c(rm11)
upp <- c(ru11)
epir <- c(r1, r1)
repr <- c(rr1, rr1)
outr <- data.frame(rep(34:80, times = 1),
                   low, med, upp, epir, repr,
                   rep(type, each = 47, times = 1),
                   rep(scenarios, each = 94),
                   rep(1, each = 94))

colnames(outr) <- c("date", "lower", "median", "upper", "epir",
                    "repr", "model", "scenario", "sn")

saveRDS(outr, "rest.rds")

## Finish Preprocessing: section 1
}
