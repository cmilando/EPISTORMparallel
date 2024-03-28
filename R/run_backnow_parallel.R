#' Title
#'
#' @param input
#' @param MAX_ITER
#' @param n_chains
#' @param norm_sigma
#' @param sip
#' @param NB_maxdelay
#' @param NB_size
#' @param printProgress
#' @param ...
#'
#' @return
#' @export
#' @import parallel
#' @import linelistBayes
#' @examples
run_backnow_parallel <- function(input, MAX_ITER = 2000, n_chains = 3,
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
      # userResponse <- readline(prompt = "The directory 'tmp' exists. Do you want to clear it? (yes/no): ")
      userResponse <- 'yes'

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
  P <- detectCores(logical = FALSE) ## P = 4
  cl <- makeCluster(P)
  clusterEvalQ(cl, {library(linelistBayes)})

  # ---------------------------------------------------------
  ## run these in parallel
  timestamp(suffix = sprintf(" > %i chains starting", n_chains))

  # if caseCounts
  if(input_type == 'caseCounts') {

    # get lineList, with random each time, from ...
    lineLists <- vector("list", length = n_chains)

    for(i in 1:n_chains) {
      lineLists[[i]] <- create_linelist(input, ...)
    }

    out_list <- clusterApplyLB(cl, 1:n_chains, function(i) {

      caseCounts_line <- lineLists[[i]]

      # run and export backnow
      run_backnow(
        caseCounts_line, MAX_ITER = MAX_ITER,
        norm_sigma = norm_sigma, sip = sip,
        NB_maxdelay = NB_maxdelay, NB_size = NB_size,
        workerID = i,
        printProgress = printProgress)
    })

  }

  # if LineList
  if(input_type == 'lineList') {

    # this is input.
    caseCounts_line <- input

    out_list <- clusterApplyLB(cl, 1:n_chains, function(i) {

      # run and export backnow
      run_backnow(
        caseCounts_line, MAX_ITER = MAX_ITER,
        norm_sigma = norm_sigma, sip = sip,
        NB_maxdelay = NB_maxdelay, NB_size = NB_size,
        workerID = i,
        printProgress = printProgress)
    })

  }

  timestamp(suffix = sprintf(" > %i chains complete", n_chains))

  # -------------------------------------------------------------------------

  ## TODO: add convergence checks

  ## Now get the mean values across
  get_row_i <- function(out_i, i, obj) out_i[[obj]][i, ]

  ##
  est_back_mat_lb <- do.call(rbind, lapply(out_list, get_row_i, i = 1, obj = 'est_back'))
  est_back_lb <- apply(est_back_mat_lb, MARGIN = 2, mean)

  est_back_mat_med <- do.call(rbind, lapply(out_list, get_row_i, i = 2, obj = 'est_back'))
  est_back_med <- apply(est_back_mat_med, MARGIN = 2, mean)

  est_back_mat_ub <- do.call(rbind, lapply(out_list, get_row_i, i = 3, obj = 'est_back'))
  est_back_ub <- apply(est_back_mat_ub, MARGIN = 2, mean)

  est_back <- rbind(est_back_lb, est_back_med, est_back_ub)

  ##
  est_back_date <- out_list[[1]]$est_back_date

  ##
  est_rt_mat_lb <- do.call(rbind, lapply(out_list, get_row_i, i = 1, obj = 'est_rt'))
  est_rt_lb <- apply(est_rt_mat_lb, MARGIN = 2, mean)

  est_rt_mat_med <- do.call(rbind, lapply(out_list, get_row_i, i = 2, obj = 'est_rt'))
  est_rt_med <- apply(est_rt_mat_med, MARGIN = 2, mean)

  est_rt_mat_ub <- do.call(rbind, lapply(out_list, get_row_i, i = 3, obj = 'est_rt'))
  est_rt_ub <- apply(est_rt_mat_ub, MARGIN = 2, mean)

  est_rt <- rbind(est_rt_lb, est_rt_med, est_rt_ub)

  ##
  est_rt_date <- out_list[[1]]$est_rt_date

  ##
  geweke_back <- do.call(c, lapply(out_list, function(oi) oi$geweke_back))

  geweke_rt <- do.call(c, lapply(out_list, function(oi) oi$geweke_rt))

  return(structure(class = "backnow",
                   list(est_back      = est_back,
                        est_back_date = est_back_date,
                        est_rt        = est_rt,
                        est_rt_date   = est_rt_date,
                        geweke_back   = geweke_back,
                        geweke_rt     = geweke_rt,
                        report_date   = out_list[[1]]$report_date,
                        report_cases  = out_list[[1]]$report_cases,
                        MAX_ITER      = MAX_ITER,
                        norm_sigma    = norm_sigma,
                        NB_maxdelay   = NB_maxdelay,
                        si            = sip,
                        NB_size       = NB_size)))
}
