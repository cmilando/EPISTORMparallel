run_backnow <- function(input, MAX_ITER = 2000, n_chains = 3,
                        norm_sigma = NULL, sip = NULL,
                        NB_maxdelay = NULL, NB_size = NULL,
                        printProgress = 0, ...) {

  # ---------------------------------------------------------
  # input checking
  if(all(c("caseCounts", "lineList") %in% class(input)))    stop()
  if(all(!(c("caseCounts", "lineList") %in% class(input)))) stop()

  input_type <- 'NA'
  if("caseCounts" %in% class(input)) {
    timestamp(suffix = "> inputType = caseCounts")
    input_type = 'caseCounts'
  }
  if("lineList" %in% class(input)) {
    timestamp(suffix = "> inputType = lineList")
    input_type = 'lineList'
  }
  stopifnot(!is.na(input_type))

  # nchains
  stopifnot(is.integer(n_chains))
  stopifnot(n_chains >= 1)
  if(n_chains >= 10) warning('`n_chains` >= 10 will lead to long run times')

  # maxiter
  stopifnot(is.integer(MAX_ITER))
  stopifnot(MAX_ITER >= 2000)
  if(MAX_ITER >= 20000) warning('`MAX_ITER` >= 20,000 will lead to long run times')

  # norm sigma for bayes parameters
  stopifnot(is.numeric(norm_sigma))
  stopifnot(norm_sigma > 0)

  # NB_maxdelay, the maximum of the right truncated distribution
  stopifnot(is.integer(NB_maxdelay))
  stopifnot(NB_maxdelay >= 0)
  stopifnot(NB_maxdelay < 50)

  # vector for SIP
  stopifnot(is.vector(sip))
  stopifnot(all(is.numeric(sip)))
  stopifnot(all(sip >= 0))

  # size of the NB distribut
  stopifnot(is.integer(NB_size))
  stopifnot(NB_size >= 0)

  # ---------------------------------------------------------
  if(printProgress == 1) {

    dirName = 'tmp'

    # Check if the directory exists
    if (dir.exists(dirName)) {
      # Ask the user if they want to clear the directory
      userResponse <- readline(prompt = "The directory 'tmp' exists. Do you want to clear it? (yes/no): ")

      if (tolower(userResponse) == "yes") {
        # Clear the directory (be cautious, as this will delete files!)
        unlink(dirName, recursive = TRUE)
        cat("The directory has been cleared.\n")

        # Optionally, recreate the directory after clearing
        dir.create(dirName)
        cat("The directory has been recreated.\n")
      } else {
        cat("The directory has not been modified.\n")
      }
    } else {
      # Directory doesn't exist, so create it
      dir.create(dirName)
      cat("The directory has been created.\n")
    }

  }

  # ---------------------------------------------------------
  ## run these in parallel using future.lapply
  timestamp(suffix = sprintf(" > %i chains starting", n_chains))

  # if caseCounts
  if(input_type == 'caseCounts') {

    # get lineList, with random each time, from ...
    lineLists <- vector("list", length = n_chains)
    for(i in 1:n_chains) {
      lineLists[[i]] <- create_linelist(input, ...)
    }

    nCores <- detectCores() - 1

    cl <- makeCluster(nCores)

    out_list <- clusterApplyLB(cl, 1:n_chains, function(i) {

      caseCounts_line <- lineLists[[i]]
      Rcpp::sourceCpp("src/backnow_cm.cpp")
      # run and export backnow
      backnow_cm(outcome = caseCounts_line$delay_int,
                 days = caseCounts_line$report_in,
                 week = caseCounts_line$week_in,
                 weekend = caseCounts_line$is_weekend,
                 workerID = as.integer(i),
                 printProgress = as.integer(printProgress),
                 iter = MAX_ITER,
                 sigma = norm_sigma,
                 maxdelay = NB_maxdelay,
                 si = sip,
                 size = NB_size)
    })


    # then run future
    future_backnow <- function(n_chains) {

      chain_seq <- seq(n_chains)
      p <- progressor(along = chain_seq)
      lapply(chain_seq, function(i) {

        p()

      }#,
      #future.seed = T,
      #future.packages = c('lubridate', 'tidyverse'),
      #future.globals = c('create_linelist', 'input', 'MAX_ITER', 'sip',
      #                   'backnow', 'norm_sigma', 'NB_maxdelay', 'NB_size')

      )
    }

  }

  # if LineList
  if(input_type == 'lineList') {
    stop()
    # this is input.
    caseCounts_line <- input

    future_backnow <- function(n_chains) {
      chain_seq <- seq(n_chains)
      p <- progressor(along = chain_seq)
      lapply(chain_seq, function(i) {
        p()
        workerID = paste0("w", i)
        # run and export backnow
        backnow(outcome = caseCounts_line$delay_int,
                days = caseCounts_line$report_in,
                week = caseCounts_line$week_in,
                weekend = caseCounts_line$is_weekend,
                iter = MAX_ITER,
                sigma = norm_sigma,
                maxdelay = NB_maxdelay,
                si = sip,
                size = NB_size,
                workerID = workerID,
                printProgress = T)
      }#,
      #future.seed = T,
      #future.packages = c('lubridate', 'tidyverse'),
      #future.globals = c('create_linelist', 'input', 'MAX_ITER', 'sip',
      #                   'backnow', 'norm_sigma', 'NB_maxdelay', 'NB_size')

      )
    }

  }

  # run in parallel
  # out_list <- future_backnow(n_chains)
  timestamp(suffix = sprintf(" > %i chains complete", n_chains))

  # ---------------------------------------------------------
  # process back-calcution and r(t) across chains
  out_list2 <- lapply(out_list, function(out1) {

    # after 1000 burn-in
    # also every 2 ...
    back1  <- out1$Back[seq(1001, nrow(out1$Back), by = 2), ]

    # depending onthe onset distribution, you may have some NA cols at the tails
    # remove those, but throw an error if ANY NAS remain
    # which columns are ALL NA
    remove_cols <- c()
    for(j in 1:ncol(back1)) {
      if(all(is.na(back1[, j]))) remove_cols <- c(j, remove_cols)
    }
    if(length(remove_cols) > 0) {
      back1 <- back1[, -remove_cols]
    }

    for(j in 1:ncol(back1)) {
      if(any(is.na(back1[, j]))) stop("some NA values in `back`")
    }

    est_back <- apply(back1, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))

    # gewke diagnostics
    gback1 <- geweke.diag(back1)$z
    gback1[is.nan(gback1)] <- 0
    gb1  <- sum(abs(gback1) > 1.96) / length(gback1)

    # ---------------------------------------------------------
    # process the r(t)
    r1 <- out1$R[seq(1001, nrow(out1$R), by = 2), ]

    # which columns are ALL NA
    remove_cols <- c()
    for(j in 1:ncol(r1)) {
      if(all(is.na(r1[, j]))) remove_cols <- c(j, remove_cols)
    }
    if(length(remove_cols) > 0) {
      r1 <- r1[, -remove_cols]
    }

    for(j in 1:ncol(r1)) {
      if(any(is.na(r1[, j]))) stop("some NA remains in `r1`")
    }

    ##
    est_r <- apply(r1, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))

    # gewke diagnostics
    gr1 <- geweke.diag(r1)$z
    gr1[is.nan(gr1)] <- 0
    gr1 <- sum(abs(gr1) > 1.96) / length(gr1)

    # ---------------------------------------------------------
    list(est_back = est_back, est_r = est_r, gb1 = gb1, gr1 = gr1)
  })


  # ----------------------------------------------------------------------
  # now post-process across chains

  #
  ## Extract the geweke's diagnostics
  # g <- readRDS('geweke.rds')
  # g <- rbind(g$g1,g$g2,g$g3,g$g4,g$g5,g$g6)
  # apply(g,2,mean)
  ## On average, 92% of the daily count and reproduction number estimates passed the geweke's test for convergence



  return(out_list2)
}
