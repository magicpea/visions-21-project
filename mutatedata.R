library(dplyr)
starting_df <- read.csv("combined.csv", stringsAsFactors = FALSE) 

starting_df <- starting_df[-1, ]

test_df <- starting_df

starting_df$sea_water_pressure_profiler_depth_enabled <- as.numeric(starting_df$sea_water_pressure_profiler_depth_enabled)

for(num in starting_df[4]) {
    test_df[ , 4] = (round(num) * -1)
}

for(i in 1:nrow(test_df)) {
  print(paste("pressue: ", test_df[i, 4], " z: ", test_df[i, 3]))
}

new_df$new_c <- new_c

for(n in test_df$sea_water_pressure_profiler_depth_enabled) {
  for(m in test_df$z) {
    if(n == m) {
        mutate(test_df, new_c = test_df[m, 2])
    }
  }
}