post4 <- function() {
## Summarizing reproductive number estimates
size <- numeric(1000)
for (i in 1:1000) {
  size[i] <- length(sce4[[i]]$epicr)
}
## Choose the last 47 Rt estimates so as to have consistent comparison (47 is min(size) for all scenarios)
size <- 47
## Choose the last 44 Rt estimates based on the reported curve

## Collect the lower, median and upper bounds
rl11 <- mat.or.vec(1000, size)
rl12 <- mat.or.vec(1000, size)
rm11 <- mat.or.vec(1000, size)
rm12 <- mat.or.vec(1000, size)
ru11 <- mat.or.vec(1000, size)
ru12 <- mat.or.vec(1000, size)
rl21 <- mat.or.vec(1000, size)
rl22 <- mat.or.vec(1000, size)
rm21 <- mat.or.vec(1000, size)
rm22 <- mat.or.vec(1000, size)
ru21 <- mat.or.vec(1000, size)
ru22 <- mat.or.vec(1000, size)
rl31 <- mat.or.vec(1000, size)
rl32 <- mat.or.vec(1000, size)
rm31 <- mat.or.vec(1000, size)
rm32 <- mat.or.vec(1000, size)
ru31 <- mat.or.vec(1000, size)
ru32 <- mat.or.vec(1000, size)
rl41 <- mat.or.vec(1000, size)
rl42 <- mat.or.vec(1000, size)
rm41 <- mat.or.vec(1000, size)
rm42 <- mat.or.vec(1000, size)
ru41 <- mat.or.vec(1000, size)
ru42 <- mat.or.vec(1000, size)
rl51 <- mat.or.vec(1000, size)
rl52 <- mat.or.vec(1000, size)
rm51 <- mat.or.vec(1000, size)
rm52 <- mat.or.vec(1000, size)
ru51 <- mat.or.vec(1000, size)
ru52 <- mat.or.vec(1000, size)
rl61 <- mat.or.vec(1000, size)
rl62 <- mat.or.vec(1000, size)
rm61 <- mat.or.vec(1000, size)
rm62 <- mat.or.vec(1000, size)
ru61 <- mat.or.vec(1000, size)
ru62 <- mat.or.vec(1000, size)


## Extract known epidemic curve
r1 <- mat.or.vec(1000, size)
r2 <- mat.or.vec(1000, size)
r3 <- mat.or.vec(1000, size)
r4 <- mat.or.vec(1000, size)
r5 <- mat.or.vec(1000, size)
r6 <- mat.or.vec(1000, size)
## Extract the report curve
rr1 <- mat.or.vec(1000, 44)
rr2 <- mat.or.vec(1000, 44)
rr3 <- mat.or.vec(1000, 44)
rr4 <- mat.or.vec(1000, 44)
rr5 <- mat.or.vec(1000, 44)
rr6 <- mat.or.vec(1000, 44)

for (i in 1:1000) {
  n <- 46
  lr1 <- ncol(sce1[[i]]$estr1)
  lr2 <- ncol(sce2[[i]]$estr1)
  lr3 <- ncol(sce3[[i]]$estr1)
  lr4 <- ncol(sce4[[i]]$estr1)
  lr5 <- ncol(sce5[[i]]$estr1)
  lr6 <- ncol(sce6[[i]]$estr1)
  rl11[i, ] <- sce1[[i]]$estr1[1, (lr1 - n):lr1]
  rl12[i, ] <- sce1[[i]]$estr2[1, (lr1 - n):lr1]
  rm11[i, ] <- sce1[[i]]$estr1[2, (lr1 - n):lr1]
  rm12[i, ] <- sce1[[i]]$estr2[2, (lr1 - n):lr1]
  ru11[i, ] <- sce1[[i]]$estr1[3, (lr1 - n):lr1]
  ru12[i, ] <- sce1[[i]]$estr2[3, (lr1 - n):lr1]
  rl21[i, ] <- sce2[[i]]$estr1[1, (lr2 - n):lr2]
  rl22[i, ] <- sce2[[i]]$estr2[1, (lr2 - n):lr2]
  rm21[i, ] <- sce2[[i]]$estr1[2, (lr2 - n):lr2]
  rm22[i, ] <- sce2[[i]]$estr2[2, (lr2 - n):lr2]
  ru21[i, ] <- sce2[[i]]$estr1[3, (lr2 - n):lr2]
  ru22[i, ] <- sce2[[i]]$estr2[3, (lr2 - n):lr2]
  rl31[i, ] <- sce3[[i]]$estr1[1, (lr3 - n):lr3]
  rl32[i, ] <- sce3[[i]]$estr2[1, (lr3 - n):lr3]
  rm31[i, ] <- sce3[[i]]$estr1[2, (lr3 - n):lr3]
  rm32[i, ] <- sce3[[i]]$estr2[2, (lr3 - n):lr3]
  ru31[i, ] <- sce3[[i]]$estr1[3, (lr3 - n):lr3]
  ru32[i, ] <- sce3[[i]]$estr2[3, (lr3 - n):lr3]
  rl41[i, ] <- sce4[[i]]$estr1[1, (lr4 - n):lr4]
  rl42[i, ] <- sce4[[i]]$estr2[1, (lr4 - n):lr4]
  rm41[i, ] <- sce4[[i]]$estr1[2, (lr4 - n):lr4]
  rm42[i, ] <- sce4[[i]]$estr2[2, (lr4 - n):lr4]
  ru41[i, ] <- sce4[[i]]$estr1[3, (lr4 - n):lr4]
  ru42[i, ] <- sce4[[i]]$estr2[3, (lr4 - n):lr4]
  rl51[i, ] <- sce5[[i]]$estr1[1, (lr5 - n):lr5]
  rl52[i, ] <- sce5[[i]]$estr2[1, (lr5 - n):lr5]
  rm51[i, ] <- sce5[[i]]$estr1[2, (lr5 - n):lr5]
  rm52[i, ] <- sce5[[i]]$estr2[2, (lr5 - n):lr5]
  ru51[i, ] <- sce5[[i]]$estr1[3, (lr5 - n):lr5]
  ru52[i, ] <- sce5[[i]]$estr2[3, (lr5 - n):lr5]
  rl61[i, ] <- sce6[[i]]$estr1[1, (lr6 - n):lr6]
  rl62[i, ] <- sce6[[i]]$estr2[1, (lr6 - n):lr6]
  rm61[i, ] <- sce6[[i]]$estr1[2, (lr6 - n):lr6]
  rm62[i, ] <- sce6[[i]]$estr2[2, (lr6 - n):lr6]
  ru61[i, ] <- sce6[[i]]$estr1[3, (lr6 - n):lr6]
  ru62[i, ] <- sce6[[i]]$estr2[3, (lr6 - n):lr6]
  s1 <- length(sce1[[i]]$epicr)
  r1[i, ] <- sce1[[i]]$epicr[(s1 - n):s1]
  s2 <- length(sce2[[i]]$epicr)
  r2[i, ] <- sce2[[i]]$epicr[(s2 - n):s2]
  s3 <- length(sce3[[i]]$epicr)
  r3[i, ] <- sce3[[i]]$epicr[(s3 - n):s3]
  s4 <- length(sce4[[i]]$epicr)
  r4[i, ] <- sce4[[i]]$epicr[(s4 - n):s4]
  s5 <- length(sce5[[i]]$epicr)
  r5[i, ] <- sce5[[i]]$epicr[(s5 - n):s5]
  s6 <- length(sce6[[i]]$epicr)
  r6[i, ] <- sce6[[i]]$epicr[(s6 - n):s6]
  s1 <- length(sce1[[i]]$repcr)
  rr1[i, ] <- sce1[[i]]$repcr[(s1 - 43):s1]
  s2 <- length(sce2[[i]]$repcr)
  rr2[i, ] <- sce2[[i]]$repcr[(s2 - 43):s2]
  s3 <- length(sce3[[i]]$repcr)
  rr3[i, ] <- sce3[[i]]$repcr[(s3 - 43):s3]
  s4 <- length(sce4[[i]]$repcr)
  rr4[i, ] <- sce4[[i]]$repcr[(s4 - 43):s4]
  s5 <- length(sce5[[i]]$repcr)
  rr5[i, ] <- sce5[[i]]$repcr[(s5 - 43):s5]
  s6 <- length(sce6[[i]]$repcr)
  rr6[i, ] <- sce6[[i]]$repcr[(s6 - 43):s6]
}
}