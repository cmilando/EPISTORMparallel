post6 <- function() {
################################
## Prepare output for figures ##
################################

rl11 <- apply(rl11, 2, mean)
rl12 <- apply(rl12, 2, mean)
rm11 <- apply(rm11, 2, mean)
rm12 <- apply(rm12, 2, mean)
ru11 <- apply(ru11, 2, mean)
ru12 <- apply(ru12, 2, mean)
rl21 <- apply(rl21, 2, mean)
rl22 <- apply(rl22, 2, mean)
rm21 <- apply(rm21, 2, mean)
rm22 <- apply(rm22, 2, mean)
ru21 <- apply(ru21, 2, mean)
ru22 <- apply(ru22, 2, mean)
rl31 <- apply(rl31, 2, mean)
rl32 <- apply(rl32, 2, mean)
rm31 <- apply(rm31, 2, mean)
rm32 <- apply(rm32, 2, mean)
ru31 <- apply(ru31, 2, mean)
ru32 <- apply(ru32, 2, mean)
rl41 <- apply(rl41, 2, mean)
rl42 <- apply(rl42, 2, mean)
rm41 <- apply(rm41, 2, mean)
rm42 <- apply(rm42, 2, mean)
ru41 <- apply(ru41, 2, mean)
ru42 <- apply(ru42, 2, mean)
rl51 <- apply(rl51, 2, mean)
rl52 <- apply(rl52, 2, mean)
rm51 <- apply(rm51, 2, mean)
rm52 <- apply(rm52, 2, mean)
ru51 <- apply(ru51, 2, mean)
ru52 <- apply(ru52, 2, mean)
rl61 <- apply(rl61, 2, mean)
rl62 <- apply(rl62, 2, mean)
rm61 <- apply(rm61, 2, mean)
rm62 <- apply(rm62, 2, mean)
ru61 <- apply(ru61, 2, mean)
ru62 <- apply(ru62, 2, mean)

r1 <- apply(r1, 2, mean)
r2 <- apply(r2, 2, mean)
r3 <- apply(r3, 2, mean)
r4 <- apply(r4, 2, mean)
r5 <- apply(r5, 2, mean)
r6 <- apply(r6, 2, mean)

rr1 <- c(rep(NA, 3), apply(rr1, 2, mean))
rr2 <- c(rep(NA, 3), apply(rr2, 2, mean))
rr3 <- c(rep(NA, 3), apply(rr3, 2, mean))
rr4 <- c(rep(NA, 3), apply(rr4, 2, mean))
rr5 <- c(rep(NA, 3), apply(rr5, 2, mean))
rr6 <- c(rep(NA, 3), apply(rr6, 2, mean))

scenarios <- c(
  "Single Delay Dist. & Correct Maximum Delay", "Single Delay Dist. & Incorrect Maximum Delay",
  "Two Delay Dist. & Correct Maximum Delay", "Two Delay Dist. & Incorrect Maximum Delay",
  "Mutiple Delay Dist. & Correct Maximum Delay", "Mutiple Delay Dist. & Incorrect Maximum Delay"
)

type <- c("1 Dispersion", "2 Dispersions")

low <- c(rl11, rl12, rl21, rl22, rl31, rl32, rl41, rl42, rl51, rl52, rl61, rl62)
med <- c(rm11, rm12, rm21, rm22, rm31, rm32, rm41, rm42, rm51, rm52, rm61, rm62)
upp <- c(ru11, ru12, ru21, ru22, ru31, ru32, ru41, ru42, ru51, ru52, ru61, ru62)
epir <- c(r1, r1, r2, r2, r3, r3, r4, r4, r5, r5, r6, r6)
repr <- c(rr1, rr1, rr2, rr2, rr3, rr3, rr4, rr4, rr5, rr5, rr6, rr6)
outr <- data.frame(rep(34:80, times = 12), low, med, upp, epir, repr, rep(type, each = 47, times = 6), rep(scenarios, each = 94), rep(1:6, each = 94))

colnames(outr) <- c("date", "lower", "median", "upper", "epir", "repr", "model", "scenario", "sn")

saveRDS(outr, "rest.rds")

## Finish Preprocessing: section 1
}