### TIME FROM MMSS TO SS ###

# table with only time
time_table = final_fishes[, 3:6]

# convert time in "minutes:seconds" to seconds

mmss_to_ss = function(time_str) {
  
  divide_mmss = strsplit(time_str, ":")[[1]]
  mm = as.numeric(divide_mmss[1])
  ss = as.numeric(divide_mmss[2])
  tot_ss = mm * 60 + ss
  return(tot_ss)
  
}

# apply function to every value of every coloumn and put them in dataframe

time_ss = data.frame(sapply(time_table, function(x) sapply(x, mmss_to_ss)))

# replace values 

final_fishes[,3:6]=time_ss