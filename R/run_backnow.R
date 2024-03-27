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
run_backnow_parallel <- function(cl, input, MAX_ITER = 2000, n_chains = 3,
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
  ## run these in parallel using future.lapply
  timestamp(suffix = sprintf(" > %i chains starting", n_chains))

  # if caseCounts
  if(input_type == 'caseCounts') {

    # get lineList, with random each time, from ...
    lineLists <- vector("list", length = n_chains)

    for(i in 1:n_chains) {
      lineLists[[i]] <- create_linelist(input, ...)
    }


    # xx <- future_lapply(1:n_chains, function(i) {
    #   run_backnow(
    #     lineLists[[i]], MAX_ITER = MAX_ITER,
    #     norm_sigma = norm_sigma, sip = sip,
    #     NB_maxdelay = NB_maxdelay, NB_size = NB_size,
    #     workerID = i,
    #     printProgress = printProgress)
    # })
    #
    #
    # stop("done")

    # clusterExport(cl,c("MAX_ITER",
    #                    "norm_sigma","sip","NB_maxdelay",
    #                    "NB_size","printProgress"))

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

  # # if LineList
  # if(input_type == 'lineList') {
  #   stop()
  #   # this is input.
  #   caseCounts_line <- input
  #
  #   future_backnow <- function(n_chains) {
  #     chain_seq <- seq(n_chains)
  #     p <- progressor(along = chain_seq)
  #     lapply(chain_seq, function(i) {
  #       p()
  #       workerID = paste0("w", i)
  #       # run and export backnow
  #       backnow(outcome = caseCounts_line$delay_int,
  #               days = caseCounts_line$report_in,
  #               week = caseCounts_line$week_in,
  #               weekend = caseCounts_line$is_weekend,
  #               iter = MAX_ITER,
  #               sigma = norm_sigma,
  #               maxdelay = NB_maxdelay,
  #               si = sip,
  #               size = NB_size,
  #               workerID = workerID,
  #               printProgress = T)
  #     }#,
  #     #future.seed = T,
  #     #future.packages = c('lubridate', 'tidyverse'),
  #     #future.globals = c('create_linelist', 'input', 'MAX_ITER', 'sip',
  #     #                   'backnow', 'norm_sigma', 'NB_maxdelay', 'NB_size')
  #
  #     )
  #   }
  #
  # }

  # run in parallel
  # out_list <- future_backnow(n_chains)
  timestamp(suffix = sprintf(" > %i chains complete", n_chains))

  return(out_list)
}
