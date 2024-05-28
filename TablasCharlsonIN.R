##########
# TTABLE #
##########
num_cols<-ncol(resultsTable)
colors_set3 <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F","#8DD3C7")

# INPUTS
inputsTable <- resultsTable[1:62, ]  

ITable <- kable(inputsTable, format = "html", escape = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c("INPUTS" = (ncol(resultsTable)+1))) %>%
  pack_rows("Epidemiological data (%|x, CI 95%)", 1, 5) %>%  
  pack_rows("Previous dease (%|x, CI 95%)", 6, 32) %>%
  pack_rows("ICU details (%|x, CI 95%)", 33, 34) %>%
  pack_rows("Support (%|x, CI 95%)", 35, 42) %>%
  pack_rows("Sedation (%|x, CI 95%)", 43,45 ) %>%
  pack_rows("Anti delirium measures & Neurological (%|x, CI 95%)", 46,49 ) %>%
  pack_rows("Race binarized (%|x, CI 95%)", 50,54 ) %>%
  pack_rows("Sedation_level_final_day1 binarized (%|x, CI 95%)", 55,59) %>%
  pack_rows("benzo_day1 binarized (%|x, CI 95%)", 60,62)

for (i in 2:num_cols) {
  ITable <- ITable %>%
    column_spec(i, background = colors_set3[i])  %>% column_spec(i + 1, background = "inherit")
}

# OUTPUTS
outputsTable <- resultsTable[63:nrow(resultsTable), ]  

OTable <- kable(outputsTable, format = "html", escape = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c("OUTPUTS" = (ncol(resultsTable)+1))) %>%
  pack_rows("Neurological status (%|x, CI 95%)", 1,13) %>%
  pack_rows("Ventilatory support (%|x, CI 95%)", 14,17) %>%
  pack_rows("Survival (%|x, CI 95%)", 18,23) %>%
  pack_rows("Length of stay (%|x, CI 95%)", 24,26) %>%
  pack_rows("Status_28_f binarized (%|x, CI 95%)", 27,32 )

for (i in 2:num_cols) {
  OTable <- OTable %>%
    column_spec(i, background = colors_set3[i])  %>% column_spec(i + 1, background = "inherit")
}

#TABLA ENTERA

tTable <- kable(resultsTable, format = "html", escape = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  pack_rows("Epidemiological data (%|x, CI 95%)", 1, 5) %>%  
  pack_rows("Previous dease (%|x, CI 95%)", 6, 32) %>%
  pack_rows("ICU details (%|x, CI 95%)", 33, 34) %>%
  pack_rows("Support (%|x, CI 95%)", 35, 42) %>%
  pack_rows("Sedation (%|x, CI 95%)", 43,45 ) %>%
  pack_rows("Anti delirium measures & Neurological (%|x, CI 95%)", 46,50 ) %>%
  pack_rows("Race binarized (%|x, CI 95%)", 51,55 ) %>%
  pack_rows("Sedation_level_final_day1 binarized (%|x, CI 95%)", 56,60) %>%
  pack_rows("benzo_day1 binarized (%|x, CI 95%)", 61,63) %>%
  pack_rows("Neurological status (%|x, CI 95%)", 64,76) %>%
  pack_rows("Ventilatory support (%|x, CI 95%)", 77,79) %>%
  pack_rows("Survival (%|x, CI 95%)", 80,85) %>%
  pack_rows("Length of stay (%|x, CI 95%)", 86,88 ) %>%
  pack_rows("Status_28_f binarized (%|x, CI 95%)", 89,94 )

for (i in 2:num_cols) {
  tTable <- tTable %>%
    column_spec(i, background = colors_set3[i])  %>% column_spec(i + 1, background = "inherit")
}

