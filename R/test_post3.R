post3 <- function() {
################################
## Prepare output for figures ##
################################

cl11 <- apply(cl11, 2, mean)

cm11 <- apply(cm11, 2, mean)

cu11 <- apply(cu11, 2, mean)


c1 <- apply(c1, 2, mean)


rc1 <- apply(rc1, 2, mean)


scenarios <- c(
  "Single Delay Dist. & Correct Maximum Delay")

type <- c("1 Dispersion")

low <- c(cl11)
med <- c(cm11)
upp <- c(cu11)
true <- c(c1)
report <- c(rc1)

outc <- data.frame(rep(1:80, times = 1),
                   low, med, upp, true, report,
                   rep(type, each = 80, times = 1),
                   rep(scenarios, each = 80),
                   rep(1, each = 80))
colnames(outc) <- c("date", "lower", "median", "upper", "epic", "repc", "model", "scenario", "sn")
#saveRDS(outc, "count.rds")
}
