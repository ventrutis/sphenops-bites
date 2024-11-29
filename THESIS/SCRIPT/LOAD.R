### LOAD CSV ###

bites_table = read.csv("DATA/FISHES_BITES.csv", sep = ";")
final_table = read.csv("DATA/FINAL_TABLE.csv", sep = ",")

# change name of column "Individual" to "run" in final_table

names(final_table)[names(final_table) == "Individual"] = "run"

# change name coloumns
colnames(fishes_final)[colnames(fishes_final) == "Gender"]="sex"
colnames(fishes_final)[colnames(fishes_final) == "Temperature"]="temperature"

### JOIN DATASET BY "RUN" ###

#library(dplyr)

selected_columns = c("run", "sex", "temperature")

final_fishes = bites_table %>%
  left_join(final_table[selected_columns], by = "run")

### TIME FROM MMSS TO SS ###

source("SCRIPT/TIME.R")

#covert as.factor

col_factor=c("patch","sex","temperature")

fishes_final[col_factor]=lapply(fishes_final[col_factor],as.factor)

### REMOVE ENTRANCE=NA ###

final_fishes = final_fishes[!is.na(final_fishes$entrance), ]

#cut_entrance

fishes_final$cut_entrance=cut(fishes_final$entrance, breaks = 4, labels = c("5 min","10 min","15 min","20 min"))

#cut_entrance in 8 parts and numerical

fishes_final$time=cut(fishes_final$entrance, breaks = 8, labels = c("2.5","5","7.5","10","12.5","15","17.5","20"))
fishes_final$time=as.numeric(as.character(fishes_final$time))

#N.bites=NA
bites_NA=fishes_final[is.na(fishes_final$nbites),]

#N.bites!=NA
bites=fishes_final[!is.na(fishes_final$nbites),]

#NA=0
bites_0=fishes_final
bites_0[is.na(bites_0)]=0