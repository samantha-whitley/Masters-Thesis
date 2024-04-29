## install packages from CRAN
p_needed <- c("tidyverse", "cshapes", "readxl", "spData", "purrr", "broom", "knitr", "stargazer", "sf", "spdep", "tmap", "AER",
"plm", "splm", "spatialreg", "gt", "modelsummary", "vctrs", "kableExtra", "units", "vtable", "flextable", "webshot2", "sandwich",
"summarytools", "officer"
              )

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)



