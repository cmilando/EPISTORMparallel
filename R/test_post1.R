post1 <- function() {
  #########################################
  ## Preprocessing the output: Section 1 ##
  #########################################
  
  ## Read the outputccc
  sce1 <- readRDS("sce1.rds")
  sce2 <- readRDS("sce2.rds")
  sce3 <- readRDS("sce3.rds")
  sce4 <- readRDS("sce4.rds")
  sce5 <- readRDS("sce5.rds")
  sce6 <- readRDS("sce6.rds")
  
  ## Read the Geweke statistics
  g1 <- mat.or.vec(1000, 4)
  g2 <- mat.or.vec(1000, 4)
  g3 <- mat.or.vec(1000, 4)
  g4 <- mat.or.vec(1000, 4)
  g5 <- mat.or.vec(1000, 4)
  g6 <- mat.or.vec(1000, 4)
  
  for (i in 1:1000) {
    g1[i, ] <- sce1[[i]]$geweke # geweke <- c(gb1, gr1, gb2, gr2)
    g2[i, ] <- sce2[[i]]$geweke
    g3[i, ] <- sce3[[i]]$geweke
    g4[i, ] <- sce4[[i]]$geweke
    g5[i, ] <- sce5[[i]]$geweke
    g6[i, ] <- sce6[[i]]$geweke
  }
  
  geweke <- list(g1 = g1, g2 = g2, g3 = g3, g4 = g4, g5 = g5, g6 = g6)
  saveRDS(geweke, "geweke.rds")
  
  
  ## Summarizing curve estimates
  ## Collect the lower, median and upper bounds
  cl11 <- mat.or.vec(1000, 80)
  cl12 <- mat.or.vec(1000, 80)
  cm11 <- mat.or.vec(1000, 80)
  cm12 <- mat.or.vec(1000, 80)
  cu11 <- mat.or.vec(1000, 80)
  cu12 <- mat.or.vec(1000, 80)
  
  cl21 <- mat.or.vec(1000, 80)
  cl22 <- mat.or.vec(1000, 80)
  cm21 <- mat.or.vec(1000, 80)
  cm22 <- mat.or.vec(1000, 80)
  cu21 <- mat.or.vec(1000, 80)
  cu22 <- mat.or.vec(1000, 80)
  
  cl31 <- mat.or.vec(1000, 80)
  cl32 <- mat.or.vec(1000, 80)
  cm31 <- mat.or.vec(1000, 80)
  cm32 <- mat.or.vec(1000, 80)
  cu31 <- mat.or.vec(1000, 80)
  cu32 <- mat.or.vec(1000, 80)
  
  cl41 <- mat.or.vec(1000, 80)
  cl42 <- mat.or.vec(1000, 80)
  cm41 <- mat.or.vec(1000, 80)
  cm42 <- mat.or.vec(1000, 80)
  cu41 <- mat.or.vec(1000, 80)
  cu42 <- mat.or.vec(1000, 80)
  
  cl51 <- mat.or.vec(1000, 80)
  cl52 <- mat.or.vec(1000, 80)
  cm51 <- mat.or.vec(1000, 80)
  cm52 <- mat.or.vec(1000, 80)
  cu51 <- mat.or.vec(1000, 80)
  cu52 <- mat.or.vec(1000, 80)
  
  cl61 <- mat.or.vec(1000, 80)
  cl62 <- mat.or.vec(1000, 80)
  cm61 <- mat.or.vec(1000, 80)
  cm62 <- mat.or.vec(1000, 80)
  cu61 <- mat.or.vec(1000, 80)
  cu62 <- mat.or.vec(1000, 80)
  
  ## Extract known epidemic curve
  c1 <- mat.or.vec(1000, 80)
  c2 <- mat.or.vec(1000, 80)
  c3 <- mat.or.vec(1000, 80)
  c4 <- mat.or.vec(1000, 80)
  c5 <- mat.or.vec(1000, 80)
  c6 <- mat.or.vec(1000, 80)
  
  ## Extract the report curve
  rc1 <- mat.or.vec(1000, 80)
  rc2 <- mat.or.vec(1000, 80)
  rc3 <- mat.or.vec(1000, 80)
  rc4 <- mat.or.vec(1000, 80)
  rc5 <- mat.or.vec(1000, 80)
  rc6 <- mat.or.vec(1000, 80)
  
  
  for (i in 1:1000) {
    
    ##
    md1 <- sce1[[i]]$minday
    md2 <- sce2[[i]]$minday
    md3 <- sce3[[i]]$minday
    md4 <- sce4[[i]]$minday
    md5 <- sce5[[i]]$minday
    md6 <- sce6[[i]]$minday
    
    ###
    cl11[i, md1:80] <- sce1[[i]]$est1[1, ]
    cl12[i, md1:80] <- sce1[[i]]$est2[1, ]
    cm11[i, md1:80] <- sce1[[i]]$est1[2, ]
    cm12[i, md1:80] <- sce1[[i]]$est2[2, ]
    cu11[i, md1:80] <- sce1[[i]]$est1[3, ]
    cu12[i, md1:80] <- sce1[[i]]$est2[3, ]
    cl21[i, md2:80] <- sce2[[i]]$est1[1, ]
    cl22[i, md2:80] <- sce2[[i]]$est2[1, ]
    cm21[i, md2:80] <- sce2[[i]]$est1[2, ]
    cm22[i, md2:80] <- sce2[[i]]$est2[2, ]
    cu21[i, md2:80] <- sce2[[i]]$est1[3, ]
    cu22[i, md2:80] <- sce2[[i]]$est2[3, ]
    cl31[i, md3:80] <- sce3[[i]]$est1[1, ]
    cl32[i, md3:80] <- sce3[[i]]$est2[1, ]
    cm31[i, md3:80] <- sce3[[i]]$est1[2, ]
    cm32[i, md3:80] <- sce3[[i]]$est2[2, ]
    cu31[i, md3:80] <- sce3[[i]]$est1[3, ]
    cu32[i, md3:80] <- sce3[[i]]$est2[3, ]
    cl41[i, md4:80] <- sce4[[i]]$est1[1, ]
    cl42[i, md4:80] <- sce4[[i]]$est2[1, ]
    cm41[i, md4:80] <- sce4[[i]]$est1[2, ]
    cm42[i, md4:80] <- sce4[[i]]$est2[2, ]
    cu41[i, md4:80] <- sce4[[i]]$est1[3, ]
    cu42[i, md4:80] <- sce4[[i]]$est2[3, ]
    cl51[i, md5:80] <- sce5[[i]]$est1[1, ]
    cl52[i, md5:80] <- sce5[[i]]$est2[1, ]
    cm51[i, md5:80] <- sce5[[i]]$est1[2, ]
    cm52[i, md5:80] <- sce5[[i]]$est2[2, ]
    cu51[i, md5:80] <- sce5[[i]]$est1[3, ]
    cu52[i, md5:80] <- sce5[[i]]$est2[3, ]
    cl61[i, md6:80] <- sce6[[i]]$est1[1, ]
    cl62[i, md6:80] <- sce6[[i]]$est2[1, ]
    cm61[i, md6:80] <- sce6[[i]]$est1[2, ]
    cm62[i, md6:80] <- sce6[[i]]$est2[2, ]
    cu61[i, md6:80] <- sce6[[i]]$est1[3, ]
    cu62[i, md6:80] <- sce6[[i]]$est2[3, ]
    
    ###
    days1 <- as.numeric(names(sce1[[i]]$epic)) + 20 # 20 is max reporting delay
    c1[i, days1] <- sce1[[i]]$epic
    
    days2 <- as.numeric(names(sce2[[i]]$epic)) + 20
    c2[i, days2] <- sce2[[i]]$epic
    
    days3 <- as.numeric(names(sce3[[i]]$epic)) + 20
    c3[i, days3] <- sce3[[i]]$epic
    
    days4 <- as.numeric(names(sce4[[i]]$epic)) + 20
    c4[i, days4] <- sce4[[i]]$epic
    
    days5 <- as.numeric(names(sce5[[i]]$epic)) + 20
    c5[i, days5] <- sce5[[i]]$epic
    
    days6 <- as.numeric(names(sce6[[i]]$epic)) + 20
    c6[i, days6] <- sce6[[i]]$epic
    
    ###
    days1 <- as.numeric(names(sce1[[i]]$repc)) + 20
    rc1[i, days1] <- sce1[[i]]$repc
    
    days2 <- as.numeric(names(sce2[[i]]$repc)) + 20
    rc2[i, days2] <- sce2[[i]]$repc
    days3 <- as.numeric(names(sce3[[i]]$repc)) + 20
    rc3[i, days3] <- sce3[[i]]$repc
    days4 <- as.numeric(names(sce4[[i]]$repc)) + 20
    rc4[i, days4] <- sce4[[i]]$repc
    days5 <- as.numeric(names(sce5[[i]]$repc)) + 20
    rc5[i, days5] <- sce5[[i]]$repc
    days6 <- as.numeric(names(sce6[[i]]$repc)) + 20
    rc6[i, days6] <- sce6[[i]]$repc
  }

}