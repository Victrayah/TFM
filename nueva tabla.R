##########
# TTABLE #
##########
num_cols<-ncol(resultsTable)
colors_set3 <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F","#8DD3C7")

# INPUTS
inputsTable <- resultsTable[1:38, ]  

ITable <- kable(inputsTable, format = "html", escape = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c("INPUTS" = (ncol(resultsTable)+1))) %>%
  pack_rows("Epidemiological data (%|x, CI 95%)", 1, 4) %>%  
  pack_rows("Previous dease (%|x, CI 95%)", 5, 12) %>%
  pack_rows("ICU details (%|x, CI 95%)", 13,13) %>%
  pack_rows("Support (%|x, CI 95%)", 14,21 ) %>%
  pack_rows("Sedation (%|x, CI 95%)", 22,24 ) %>%
  pack_rows("Anti delirium measures & Neurological (%|x, CI 95%)", 25,28) %>%
  pack_rows("Race binarized (%|x, CI 95%)", 29,33 ) %>%
  pack_rows("Sedation_level_final_day1 binarized (%|x, CI 95%)", 34,36) %>%
  pack_rows("benzo_day1 binarized (%|x, CI 95%)", 37,38)

for (i in 2:num_cols) {
  ITable <- ITable %>%
    column_spec(i, background = colors_set3[i])  %>% column_spec(i + 1, background = "inherit")
}

# OUTPUTS
outputsTable <- resultsTable[39:nrow(resultsTable), ]  

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
  add_header_above(c("INPUTS" = (ncol(resultsTable)+1))) %>%
  pack_rows("Epidemiological data (%|x, CI 95%)", 1, 4) %>%  
  pack_rows("Previous dease (%|x, CI 95%)", 5, 12) %>%
  pack_rows("ICU details (%|x, CI 95%)", 13,13) %>%
  pack_rows("Support (%|x, CI 95%)", 14,21 ) %>%
  pack_rows("Sedation (%|x, CI 95%)", 22,24 ) %>%
  pack_rows("Anti delirium measures & Neurological (%|x, CI 95%)", 25,28) %>%
  pack_rows("Sedation_level_final_day1 binarized (%|x, CI 95%)", 29,31) %>%
  pack_rows("benzo_day1 binarized (%|x, CI 95%)", 32,33) %>%
  pack_rows("Neurological status (%|x, CI 95%)", 34,46) %>%
  pack_rows("Ventilatory support (%|x, CI 95%)", 47,50) %>%
  pack_rows("Survival (%|x, CI 95%)", 51,57) %>%
  pack_rows("Length of stay (%|x, CI 95%)", 58,60 ) %>%
  pack_rows("Race binarized (%|x, CI 95%)", 61,66 ) %>%
  pack_rows("Status_28_f binarized (%|x, CI 95%)", 67,72 )



for (i in 2:num_cols) {
  tTable <- tTable %>%
    column_spec(i, background = colors_set3[i])  %>% column_spec(i + 1, background = "inherit")
}

tTable
