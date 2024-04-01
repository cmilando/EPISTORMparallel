post4 <- function() {

  ## Summarizing reproductive number estimates
  size <- numeric(10)
  for (i in 1:10) {
    size[i] <- length(sce1[[i]]$epicr)
  }

  ## Choose the last 47 Rt estimates so as to have consistent comparison
  ## (47 is min(size) for all scenarios)
  size <- 47

  ## Choose the last 44 Rt estimates based on the reported curve

  ## Collect the lower, median and upper bounds
  rl11 <- mat.or.vec(10, size)
  rm11 <- mat.or.vec(10, size)
  ru11 <- mat.or.vec(10, size)

  ## Extract known epidemic curve
  r1 <- mat.or.vec(10, size)

  ## Extract the report curve
  rr1 <- mat.or.vec(10, 44) #### 44 !! so .... this .... is .... offset??

  for (i in 1:10) {

    n <- 46
    lr1 <- ncol(sce1[[i]]$estr1)


    rl11[i, ] <- sce1[[i]]$estr1[1, (lr1 - n):lr1]

    rm11[i, ] <- sce1[[i]]$estr1[2, (lr1 - n):lr1]

    ru11[i, ] <- sce1[[i]]$estr1[3, (lr1 - n):lr1]

    #####
    s1 <- length(sce1[[i]]$epicr)
    r1[i, ] <- sce1[[i]]$epicr[(s1 - n):s1]

    ####
    s1 <- length(sce1[[i]]$repcr)
    rr1[i, ] <- sce1[[i]]$repcr[(s1 - 43):s1] #### ????

  }
}
