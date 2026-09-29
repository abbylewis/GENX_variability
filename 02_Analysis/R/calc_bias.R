#' Calculate bias of manual sampling
#'
#' @param interval Time interval for sampling
#'
#' @returns Data frame with metrics describing the bias of manual sampling
#'
calc_bias <- function(interval, only_time = T, season = F) {
  
  # Parse interval name
  samples_per_bin <- case_when(
    grepl("twice", interval) ~ 2,
    .default = 1
  )
  
  bin_dur <- str_extract(interval, "daily|week|month|fortnight")
  bin_dur <- ifelse(bin_dur == "daily", "day", bin_dur)
  
  message(
    paste0(
      "Running ", interval,
      ": \nBin: ", bin_dur,
      "\nSamples per bin: ", samples_per_bin
    )
  )
  
  # Define grouping variables
  group_vars <- if (season) {
    c("season", bin_dur, "Chamber")
  } else {
    c(bin_dur, "Chamber")
  }
  
  # Run simulation for all data
  out_all <- df %>%
    group_by(across(all_of(group_vars))) %>%
    reframe(
      n = n(),
      rep = seq_len(reps),
      # For every replicate, choose a flux at random
      # and calculate the mean
      flux = replicate(
        reps,
        mean(sample(CH4_umol_m2_h, samples_per_bin))
      ),
      interval = interval
    ) %>%
    mutate(time = "all")
  
  if (!only_time) {
    
    # Daytime
    out_daytime <- df %>%
      filter(hour(DateTime_local) %in% 9:17) %>%
      group_by(across(all_of(group_vars))) %>%
      reframe(
        n = n(),
        rep = seq_len(reps),
        flux = replicate(
          reps,
          mean(sample(CH4_umol_m2_h, samples_per_bin))
        ),
        interval = interval
      ) %>%
      mutate(time = "daytime")
    
    # Water level
    out_wl <- df %>%
      filter(Depth_cm <= 1) %>%
      group_by(across(all_of(group_vars))) %>%
      reframe(
        n = n(),
        rep = seq_len(reps),
        flux = replicate(
          reps,
          mean(sample(CH4_umol_m2_h, samples_per_bin))
        ),
        interval = interval
      ) %>%
      mutate(time = "waterlevel")
    
    return(bind_rows(out_all, out_daytime, out_wl))
  }
  
  return(out_all)
}