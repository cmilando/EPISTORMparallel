junk2 <- function() {

  # this gives you back the 2.5th, 50th, and 97.5th distribution values
  # for each chain

  #################################
  # post-processing happens now
  # see postprocessing.R,
  # there is something with offsets that is not entirely clear
  #################################

  ## Now get the mean values across
  get_row_i <- function(out_i, i) out_i$est_back[i, ]

  ## TODO: this could change if we allow for different percentiles
  est_back_mat_lb <- do.call(rbind, lapply(out_list, get_row_i, i = 1))
  est_back_lb <- apply(est_back_mat_lb, MARGIN = 2, mean)
  est_back_lb

  est_back_mat_med <- do.call(rbind, lapply(out_list, get_row_i, i = 2))
  est_back_med <- apply(est_back_mat_med, MARGIN = 2, mean)
  est_back_med

  est_back_mat_ub <- do.call(rbind, lapply(out_list, get_row_i, i = 3))
  est_back_ub <- apply(est_back_mat_ub, MARGIN = 2, mean)
  est_back_ub

  estimated_LD = 20

  ### the offset here is maxdelay + 1 ...?
  ### should this line up with the original r(t) ?? probably right ??
  ### starting vector is ld + 1
  outr <- data.frame(low = est_back_lb,
                     med = est_back_med,
                     upp = est_back_ub)[1:100, ]

  colnames(outr) <- c("lower", "median", "upper")
  outr$index <- 1:nrow(outr) - estimated_LD
  head(outr)
  tail(outr)

  ggplot(outr) +
    geom_ribbon(aes(x = index, ymin = lower, ymax = upper)) +
    geom_line(aes(x = index, y = median))

  caseCounts$report_int <- as.vector(caseCounts$date - min(caseCounts$date)) + 1

  ggplot(outr) + theme_bw() +
    geom_ribbon(aes(x = index, ymin = lower, ymax = upper), fill = 'grey') +
    geom_line(aes(x = index, y = median), color = 'blue') +
    geom_line(data = caseCounts, mapping = aes(x = report_int, y = cases))



  ### the offset here is maxdelay + 1 ...?
  outr <- data.frame(low = est_r[1, (estimated_LD + 1):ncol(est_r)],
                     med = est_r[2, (estimated_LD + 1):ncol(est_r)],
                     upp = est_r[3, (estimated_LD + 1):ncol(est_r)])

  colnames(outr) <- c("lower", "median", "upper")
  outr$index <- 1:nrow(outr)
  head(outr)
  tail(outr)

}
