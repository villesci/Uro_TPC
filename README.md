# Urosalpinx thermal performance curve manuscript

*Urosalpinx* thermal performance manuscript for submission to ProcB, *Environment and phenology shape local adaptation in thermal performance*

There are three scripts in this repository:

1) "uro_growth_rate_bin_rtpc_glmm.RMD" - This rmarkdown file contains the main statistical analysis used in this paper. This is where we fit models to thermal perofrmance data, extracted topt and MTP, and performed model selection on these variables with a set of extracted environmental parameters

2) "test_env.Rmd" - This file is an overview of environmental sources we reviewed and selected from to calculate the environmental parameters in the above Rmarkdown file. For many sites, we show multiple data sources to support our selection of certain sources based on data quality, availability, etc.

3) "TPC_bootstrap_glm/Rmd" - This file contains the code used to bootstrap our TPC curves and produce confidence intervals about our data. This is also where we produced figure 3 in our manuscript.