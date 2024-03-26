post5 <- function() {
######################################################
## Additional processing for coverage rate and RMSE ##
######################################################
size <- 47
rcov11 <- mat.or.vec(1000, size)
rcov21 <- mat.or.vec(1000, size)
rcov31 <- mat.or.vec(1000, size)
rcov41 <- mat.or.vec(1000, size)
rcov51 <- mat.or.vec(1000, size)
rcov61 <- mat.or.vec(1000, size)
rcov12 <- mat.or.vec(1000, size)
rcov22 <- mat.or.vec(1000, size)
rcov32 <- mat.or.vec(1000, size)
rcov42 <- mat.or.vec(1000, size)
rcov52 <- mat.or.vec(1000, size)
rcov62 <- mat.or.vec(1000, size)
re11 <- mat.or.vec(1000, size)
re12 <- mat.or.vec(1000, size)
re21 <- mat.or.vec(1000, size)
re22 <- mat.or.vec(1000, size)
re31 <- mat.or.vec(1000, size)
re32 <- mat.or.vec(1000, size)
re41 <- mat.or.vec(1000, size)
re42 <- mat.or.vec(1000, size)
re51 <- mat.or.vec(1000, size)
re52 <- mat.or.vec(1000, size)
re61 <- mat.or.vec(1000, size)
re62 <- mat.or.vec(1000, size)

for (i in 1:1000) {
  rcov11[i, ] <- as.numeric((r1[i, ] >= rl11[i, ]) & (r1[i, ] <= ru11[i, ]))
  rcov12[i, ] <- as.numeric((r1[i, ] >= rl12[i, ]) & (r1[i, ] <= ru12[i, ]))
  rcov21[i, ] <- as.numeric((r2[i, ] >= rl21[i, ]) & (r2[i, ] <= ru21[i, ]))
  rcov22[i, ] <- as.numeric((r2[i, ] >= rl22[i, ]) & (r2[i, ] <= ru22[i, ]))
  rcov31[i, ] <- as.numeric((r3[i, ] >= rl31[i, ]) & (r3[i, ] <= ru31[i, ]))
  rcov32[i, ] <- as.numeric((r3[i, ] >= rl32[i, ]) & (r3[i, ] <= ru32[i, ]))
  rcov41[i, ] <- as.numeric((r4[i, ] >= rl41[i, ]) & (r4[i, ] <= ru41[i, ]))
  rcov42[i, ] <- as.numeric((r4[i, ] >= rl42[i, ]) & (r4[i, ] <= ru42[i, ]))
  rcov51[i, ] <- as.numeric((r5[i, ] >= rl51[i, ]) & (r5[i, ] <= ru51[i, ]))
  rcov52[i, ] <- as.numeric((r5[i, ] >= rl52[i, ]) & (r5[i, ] <= ru52[i, ]))
  rcov61[i, ] <- as.numeric((r6[i, ] >= rl61[i, ]) & (r6[i, ] <= ru61[i, ]))
  rcov62[i, ] <- as.numeric((r6[i, ] >= rl62[i, ]) & (r6[i, ] <= ru62[i, ]))
  re11[i, ] <- (rm11[i, ] - r1[i, ])^2
  re12[i, ] <- (rm12[i, ] - r1[i, ])^2
  re21[i, ] <- (rm21[i, ] - r2[i, ])^2
  re22[i, ] <- (rm22[i, ] - r2[i, ])^2
  re31[i, ] <- (rm31[i, ] - r3[i, ])^2
  re32[i, ] <- (rm32[i, ] - r3[i, ])^2
  re41[i, ] <- (rm41[i, ] - r4[i, ])^2
  re42[i, ] <- (rm42[i, ] - r4[i, ])^2
  re51[i, ] <- (rm51[i, ] - r5[i, ])^2
  re52[i, ] <- (rm52[i, ] - r5[i, ])^2
  re61[i, ] <- (rm61[i, ] - r6[i, ])^2
  re62[i, ] <- (rm62[i, ] - r6[i, ])^2
}

rcov11 <- apply(rcov11, 2, sum) / 1000
rcov12 <- apply(rcov12, 2, sum) / 1000
rcov21 <- apply(rcov21, 2, sum) / 1000
rcov22 <- apply(rcov22, 2, sum) / 1000
rcov31 <- apply(rcov31, 2, sum) / 1000
rcov32 <- apply(rcov32, 2, sum) / 1000
rcov41 <- apply(rcov41, 2, sum) / 1000
rcov42 <- apply(rcov42, 2, sum) / 1000
rcov51 <- apply(rcov51, 2, sum) / 1000
rcov52 <- apply(rcov52, 2, sum) / 1000
rcov61 <- apply(rcov61, 2, sum) / 1000
rcov62 <- apply(rcov62, 2, sum) / 1000
re11 <- sqrt(apply(re11, 2, mean))
re12 <- sqrt(apply(re12, 2, mean))
re21 <- sqrt(apply(re21, 2, mean))
re22 <- sqrt(apply(re22, 2, mean))
re31 <- sqrt(apply(re31, 2, mean))
re32 <- sqrt(apply(re32, 2, mean))
re41 <- sqrt(apply(re41, 2, mean))
re42 <- sqrt(apply(re42, 2, mean))
re51 <- sqrt(apply(re51, 2, mean))
re52 <- sqrt(apply(re52, 2, mean))
re61 <- sqrt(apply(re61, 2, mean))
re62 <- sqrt(apply(re62, 2, mean))

date <- rep(34:80, times = 24)
result <- c(
  rcov11, rcov12, rcov21, rcov22, rcov31, rcov32, rcov41, rcov42, rcov51, rcov52, rcov61, rcov62,
  re11, re12, re21, re22, re31, re32, re41, re42, re51, re52, re61, re62
)
scenario <- rep(1:6, each = 94, times = 2)
model <- rep(1:2, each = 47, times = 12)
type <- rep(c("coverage", "RMSE"), each = 564)
rout1 <- data.frame(cbind(date, result, scenario, model, type))
eva1 <- list(c = cout1, r = rout1)
saveRDS(eva1, "eva1.rds")
}