post3 <- function() {
################################
## Prepare output for figures ##
################################

cl11 <- apply(cl11, 2, mean)
cl12 <- apply(cl12, 2, mean)
cm11 <- apply(cm11, 2, mean)
cm12 <- apply(cm12, 2, mean)
cu11 <- apply(cu11, 2, mean)
cu12 <- apply(cu12, 2, mean)
cl21 <- apply(cl21, 2, mean)
cl22 <- apply(cl22, 2, mean)
cm21 <- apply(cm21, 2, mean)
cm22 <- apply(cm22, 2, mean)
cu21 <- apply(cu21, 2, mean)
cu22 <- apply(cu22, 2, mean)
cl31 <- apply(cl31, 2, mean)
cl32 <- apply(cl32, 2, mean)
cm31 <- apply(cm31, 2, mean)
cm32 <- apply(cm32, 2, mean)
cu31 <- apply(cu31, 2, mean)
cu32 <- apply(cu32, 2, mean)
cl41 <- apply(cl41, 2, mean)
cl42 <- apply(cl42, 2, mean)
cm41 <- apply(cm41, 2, mean)
cm42 <- apply(cm42, 2, mean)
cu41 <- apply(cu41, 2, mean)
cu42 <- apply(cu42, 2, mean)
cl51 <- apply(cl51, 2, mean)
cl52 <- apply(cl52, 2, mean)
cm51 <- apply(cm51, 2, mean)
cm52 <- apply(cm52, 2, mean)
cu51 <- apply(cu51, 2, mean)
cu52 <- apply(cu52, 2, mean)
cl61 <- apply(cl61, 2, mean)
cl62 <- apply(cl62, 2, mean)
cm61 <- apply(cm61, 2, mean)
cm62 <- apply(cm62, 2, mean)
cu61 <- apply(cu61, 2, mean)
cu62 <- apply(cu62, 2, mean)

c1 <- apply(c1, 2, mean)
c2 <- apply(c2, 2, mean)
c3 <- apply(c3, 2, mean)
c4 <- apply(c4, 2, mean)
c5 <- apply(c5, 2, mean)
c6 <- apply(c6, 2, mean)

rc1 <- apply(rc1, 2, mean)
rc2 <- apply(rc2, 2, mean)
rc3 <- apply(rc3, 2, mean)
rc4 <- apply(rc4, 2, mean)
rc5 <- apply(rc5, 2, mean)
rc6 <- apply(rc6, 2, mean)

scenarios <- c(
  "Single Delay Dist. & Correct Maximum Delay", "Single Delay Dist. & Incorrect Maximum Delay",
  "Two Delay Dist. & Correct Maximum Delay", "Two Delay Dist. & Incorrect Maximum Delay",
  "Mutiple Delay Dist. & Correct Maximum Delay", "Mutiple Delay Dist. & Incorrect Maximum Delay"
)

type <- c("1 Dispersion", "2 Dispersions")

low <- c(cl11, cl12, cl21, cl22, cl31, cl32, cl41, cl42, cl51, cl52, cl61, cl62)
med <- c(cm11, cm12, cm21, cm22, cm31, cm32, cm41, cm42, cm51, cm52, cm61, cm62)
upp <- c(cu11, cu12, cu21, cu22, cu31, cu32, cu41, cu42, cu51, cu52, cu61, cu62)
true <- c(c1, c1, c2, c2, c3, c3, c4, c4, c5, c5, c6, c6)
report <- c(rc1, rc1, rc2, rc2, rc3, rc3, rc4, rc4, rc5, rc5, rc6, rc6)

outc <- data.frame(rep(1:80, times = 12), low, med, upp, true, report, rep(type, each = 80, times = 6), rep(scenarios, each = 160), rep(1:6, each = 160))
colnames(outc) <- c("date", "lower", "median", "upper", "epic", "repc", "model", "scenario", "sn")
saveRDS(outc, "count.rds")
}