
Dataset <- 
  readXL("C:/Users/fearl/Documents/Projekt/zarzadzanie_projektami/Efekt3/Dane_Rcmd/Rcmd.xlsx",
   rownames=FALSE, header=TRUE, na="", sheet="Arkusz1", stringsAsFactors=TRUE)
local({
  .Table <- with(Dataset, table(joined_EU))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
cor(Dataset[,c("emp_deadline_pct","GDP_pc","high_tech_trade_pc","is_euro_currency","nuclear_electricity","phycisians_per_1000",
  "r.d_bud_pct","r.d_gdp_pct","sea_access","use_cloud_pct","weeknd_work_pct","working_pop_pct")], use="complete")
library(lattice, pos=17)
library(survival, pos=17)
library(Formula, pos=17)
library(ggplot2, pos=17)
library(Hmisc, pos=17)
rcorr.adjust(Dataset[,c("emp_deadline_pct","GDP_pc","high_tech_trade_pc","is_euro_currency","nuclear_electricity","phycisians_per_1000","r.d_bud_pct","r.d_gdp_pct",
  "sea_access","use_cloud_pct","weeknd_work_pct","working_pop_pct")], type="pearson", use="complete")
with(Dataset, Hist(GDP_pc, scale="frequency", breaks="Sturges", col="darkgray"))

