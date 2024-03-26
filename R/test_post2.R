post2 <- function() {
  ######################################################
  ## Additional processing for coverage rate and RMSE ##
  ######################################################
  
  ccov11 <- mat.or.vec(1000, 80)
  ccov21 <- mat.or.vec(1000, 80)
  ccov31 <- mat.or.vec(1000, 80)
  ccov41 <- mat.or.vec(1000, 80)
  ccov51 <- mat.or.vec(1000, 80)
  ccov61 <- mat.or.vec(1000, 80)
  ccov12 <- mat.or.vec(1000, 80)
  ccov22 <- mat.or.vec(1000, 80)
  ccov32 <- mat.or.vec(1000, 80)
  ccov42 <- mat.or.vec(1000, 80)
  ccov52 <- mat.or.vec(1000, 80)
  ccov62 <- mat.or.vec(1000, 80)
  
  ce11 <- mat.or.vec(1000, 80)
  ce12 <- mat.or.vec(1000, 80)
  ce21 <- mat.or.vec(1000, 80)
  ce22 <- mat.or.vec(1000, 80)
  ce31 <- mat.or.vec(1000, 80)
  ce32 <- mat.or.vec(1000, 80)
  ce41 <- mat.or.vec(1000, 80)
  ce42 <- mat.or.vec(1000, 80)
  ce51 <- mat.or.vec(1000, 80)
  ce52 <- mat.or.vec(1000, 80)
  ce61 <- mat.or.vec(1000, 80)
  ce62 <- mat.or.vec(1000, 80)
  
  for (i in 1:1000) {
    
    # is true within the coverage interval?
    ccov11[i, ] <- as.numeric((c1[i, ] >= cl11[i, ]) & (c1[i, ] <= cu11[i, ]))
    ccov12[i, ] <- as.numeric((c1[i, ] >= cl12[i, ]) & (c1[i, ] <= cu12[i, ]))
    ccov21[i, ] <- as.numeric((c2[i, ] >= cl21[i, ]) & (c2[i, ] <= cu21[i, ]))
    ccov22[i, ] <- as.numeric((c2[i, ] >= cl22[i, ]) & (c2[i, ] <= cu22[i, ]))
    ccov31[i, ] <- as.numeric((c3[i, ] >= cl31[i, ]) & (c3[i, ] <= cu31[i, ]))
    ccov32[i, ] <- as.numeric((c3[i, ] >= cl32[i, ]) & (c3[i, ] <= cu32[i, ]))
    ccov41[i, ] <- as.numeric((c4[i, ] >= cl41[i, ]) & (c4[i, ] <= cu41[i, ]))
    ccov42[i, ] <- as.numeric((c4[i, ] >= cl42[i, ]) & (c4[i, ] <= cu42[i, ]))
    ccov51[i, ] <- as.numeric((c5[i, ] >= cl51[i, ]) & (c5[i, ] <= cu51[i, ]))
    ccov52[i, ] <- as.numeric((c5[i, ] >= cl52[i, ]) & (c5[i, ] <= cu52[i, ]))
    ccov61[i, ] <- as.numeric((c6[i, ] >= cl61[i, ]) & (c6[i, ] <= cu61[i, ]))
    ccov62[i, ] <- as.numeric((c6[i, ] >= cl62[i, ]) & (c6[i, ] <= cu62[i, ]))
    
    # comparing medians to true for RMSE
    ce11[i, ] <- (cm11[i, ] - c1[i, ])^2 
    ce12[i, ] <- (cm12[i, ] - c1[i, ])^2
    ce21[i, ] <- (cm21[i, ] - c2[i, ])^2
    ce22[i, ] <- (cm22[i, ] - c2[i, ])^2
    ce31[i, ] <- (cm31[i, ] - c3[i, ])^2
    ce32[i, ] <- (cm32[i, ] - c3[i, ])^2
    ce41[i, ] <- (cm41[i, ] - c4[i, ])^2
    ce42[i, ] <- (cm42[i, ] - c4[i, ])^2
    ce51[i, ] <- (cm51[i, ] - c5[i, ])^2
    ce52[i, ] <- (cm52[i, ] - c5[i, ])^2
    ce61[i, ] <- (cm61[i, ] - c6[i, ])^2
    ce62[i, ] <- (cm62[i, ] - c6[i, ])^2
  }
  
  # applying over all 1000 simulations
  ccov11 <- apply(ccov11, 2, sum) / 1000
  ccov12 <- apply(ccov12, 2, sum) / 1000
  ccov21 <- apply(ccov21, 2, sum) / 1000
  ccov22 <- apply(ccov22, 2, sum) / 1000
  ccov31 <- apply(ccov31, 2, sum) / 1000
  ccov32 <- apply(ccov32, 2, sum) / 1000
  ccov41 <- apply(ccov41, 2, sum) / 1000
  ccov42 <- apply(ccov42, 2, sum) / 1000
  ccov51 <- apply(ccov51, 2, sum) / 1000
  ccov52 <- apply(ccov52, 2, sum) / 1000
  ccov61 <- apply(ccov61, 2, sum) / 1000
  ccov62 <- apply(ccov62, 2, sum) / 1000
  
  # finishing the RSME calcuatlion
  ce11 <- sqrt(apply(ce11, 2, mean))
  ce12 <- sqrt(apply(ce12, 2, mean))
  ce21 <- sqrt(apply(ce21, 2, mean))
  ce22 <- sqrt(apply(ce22, 2, mean))
  ce31 <- sqrt(apply(ce31, 2, mean))
  ce32 <- sqrt(apply(ce32, 2, mean))
  ce41 <- sqrt(apply(ce41, 2, mean))
  ce42 <- sqrt(apply(ce42, 2, mean))
  ce51 <- sqrt(apply(ce51, 2, mean))
  ce52 <- sqrt(apply(ce52, 2, mean))
  ce61 <- sqrt(apply(ce61, 2, mean))
  ce62 <- sqrt(apply(ce62, 2, mean))
  
  # 80?
  # 24? 
  date <- rep(1:80, times = 24)
  result <- c(
    ccov11, ccov12, ccov21, ccov22, ccov31, ccov32, ccov41, ccov42, ccov51, ccov52, ccov61, ccov62,
    ce11, ce12, ce21, ce22, ce31, ce32, ce41, ce42, ce51, ce52, ce61, ce62
  )
  
  # 160? 
  # 2?
  scenario <- rep(1:6, each = 160, times = 2)
  model <- rep(1:2, each = 80, times = 12)
  
  type <- rep(c("coverage", "RMSE"), each = 960)
  
  cout1 <- data.frame(cbind(date, result, scenario, model, type))

}