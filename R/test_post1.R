post1 <- function() {
  #########################################
  ## Preprocessing the output: Section 1 ##
  #########################################

  ## Read the outputccc
  sce1 <- readRDS("../../../OneDrive - Boston University/Documents/55 - EPISTORM/backandnow/backandnow-master/simulation/sce1.rds")

  ## Read the Geweke statistics
  g1 <- mat.or.vec(10, 4)

  for (i in 1:10) {
    g1[i, ] <- sce1[[i]]$geweke # geweke <- c(gb1, gr1, gb2, gr2)
  }

  geweke <- list(g1 = g1)
  #saveRDS(geweke, "geweke.rds")


  ## Summarizing curve estimates
  ## Collect the lower, median and upper bounds
  cl11 <- mat.or.vec(10, 80)
  cm11 <- mat.or.vec(10, 80)
  cu11 <- mat.or.vec(10, 80)


  ## Extract known epidemic curve
  c1 <- mat.or.vec(10, 80)

  ## Extract the report curve
  rc1 <- mat.or.vec(10, 80)


  for (i in 1:10) {

    ## #### OFFSET BY THE MINDAY EACH TIME!!!!!!!
    md1 <- sce1[[i]]$minday

    ###
    cl11[i, md1:80] <- sce1[[i]]$est1[1, ]
    cm11[i, md1:80] <- sce1[[i]]$est1[2, ]
    cu11[i, md1:80] <- sce1[[i]]$est1[3, ]

    ###
    ### STARTS at 4, which is the result of the getr() function ....
    ### SO SHOULD I USE THAT TOO?
    days1 <- as.numeric(names(sce1[[i]]$epic)) + 20 # 20 is max reporting delay
    c1[i, days1] <- sce1[[i]]$epic

    ###
    days1 <- as.numeric(names(sce1[[i]]$repc)) + 20
    rc1[i, days1] <- sce1[[i]]$repc


  }

}
