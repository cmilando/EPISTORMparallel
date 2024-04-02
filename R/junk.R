funk4 <- function() {

  devtools::install("../linelistBayes/")
  devtools::install()

  library(linelistBayes)
  library(EPISTORMparallel)

  # load data
  data("sample_dates")
  data("sample_location")
  data("sample_cases")

  head(sample_dates)
  head(sample_cases)

  # creating case-counts
  caseCounts <- create_caseCounts(date_vec = sample_dates,
                                  location_vec = sample_location,
                                  cases_vec = sample_cases)

  # get the first wave only
  caseCounts <- caseCounts[1:80, ]
  head(caseCounts)

  # serial interval
  sip <- si(14, 4.29, 1.18)

  ####################
  ## run once
  out_list <- run_backnow(caseCounts,
                                   MAX_ITER = as.integer(6000),
                                   norm_sigma = 0.2,
                                   sip = sip,
                                   NB_maxdelay = as.integer(20),
                                   NB_size = as.integer(6),
                                   printProgress = 1,
                                   reportF_missP = 0.6)

  # plot
  plot(out_list)

  out_list$geweke_back
  out_list$geweke_rt

  ####################
  # running backnow in parallel
  out_list_parallel <- run_backnow_parallel(caseCounts,
                          MAX_ITER = as.integer(21000),
                          n_chains = as.integer(100),
                          norm_sigma = 0.2,
                          sip = sip,
                          NB_maxdelay = as.integer(20),
                          NB_size = as.integer(6),
                          printProgress = 1,
                          reportF_missP = 0.6)

  # plot
  plot(out_list_parallel)

  # geweke: dim is n_chains x 1
  1 - apply(out_list_parallel$geweke_back, 2, mean)
  1 - apply(out_list_parallel$geweke_rt, 2, mean)


}
