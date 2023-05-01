require(tidyverse)
require(haven)
require(labelled)
require(rio)
require(broom)
require(officer)
require(magrittr)
require(flextable)
require(rvg)
require(writexl)
require(data.table)

mcf_data <- read_dta(paste("G:/Shared drives/Vanguard Drive",
                           "/2. Projects/MCF IPO/4. Baseline",
                           "/MCF Baseline 2.0/2. QUANT/2. PWD",
                           "/7. Sampling/Final samples for data collection",
                           "/MCF Baseline 2.0 Quantitative sample - PWDs",
                "/MCF_Baseline_Listing_clean_PWD_for quant sampling.dta",sep=""))
view(characterize(mcf_data))
mcf_sampled<-count(mcf_data,district,sector,pwd_quant_sample)%>%
  ungroup()%>%
  pivot_wider(names_from = "pwd_quant_sample",
              values_from = n)
write_excel_csv(mcf_sampled,paste("G:/Shared drives/Vanguard Drive",
                                  "/2. Projects/MCF IPO/4. Baseline",
                                  "/MCF Baseline 2.0/2. QUANT/2. PWD",
                                  "/7. Sampling/Final samples for data collection",
                                  "/MCF Baseline 2.0 Quantitative sample - PWDs",
                                  "/Excel/PWD_SAMPLED.csv",
                                  sep=""))

setDT(mcf_sampled)[,enumerators := 3, by = district]

write_excel_csv(mcf_sampled,paste("G:/Shared drives/Vanguard Drive",
                                  "/2. Projects/MCF IPO/4. Baseline",
                                  "/MCF Baseline 2.0/2. QUANT/2. PWD",
                                  "/7. Sampling/Final samples for data collection",
                                  "/MCF Baseline 2.0 Quantitative sample - PWDs",
                                  "/Excel/PWD_SAMPLED_enumerators.csv",
                                  sep=""))
