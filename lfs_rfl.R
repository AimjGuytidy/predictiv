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
# import data
lfs <- read_dta("data/Labour force survey_2022.dta")
lfs_finance <- lfs %>%
  filter(indd03 == 11)
nrow(lfs_finance)
count(lfs_finance,wt = weight2)

set.seed(42)
lfs_finance <- lfs_finance %>%
  mutate(key = runif(nrow(lfs_finance)))
view(lfs_finance%>%filter(C01 == 2 & C02 == 2)%>%characterize())
# lfs_finance_clean <- lfs_finance %>% 
#   mutate(key = round(key,5)) %>%
#   filter(key!=0.45774)
# nrow(lfs_finance_clean)
# count(lfs_finance_clean,wt = weight2)
table(characterize(lfs_finance)[,"A01"])
table(lfs_finance[,"A01"],lfs_finance[,"indd01"])
view(table(characterize(lfs_finance)[,c("A01","indd01")])%>%
       as.data.frame()%>%
       pivot_wider(values_from = Freq,names_from = A01))
temp1 <- count(characterize(lfs_finance),A01,indd01,wt = weight2)%>%
  pivot_wider(values_from = n,names_from = A01)%>%
  rename(setNames("indd01", var_label(lfs$indd01))[1])

ftab <- flextable(temp1)
ftab <- colformat_double(
  x = ftab,
  big.mark = ",", digits = 2, na_str = "N/A"
)

ftab <- autofit(ftab)
std_border <- fp_border(color="gray")
ftab <- border_remove(x = ftab)
ftab <- hline(ftab, part="all", border = std_border )
std_borderv <- fp_border(color="gray")
ftab <- vline(ftab, border = std_borderv )
ftab <- border(ftab, border = fp_border(color = "grey"))
ftab <- border(ftab, border = fp_border(color = "grey"),part = "header")
ftab <- fontsize(ftab, size = 7, part = "header")
ftab <- fontsize(ftab, size = 6.5, part = "body")

# write_xlsx(temp1,"data/temp.xlsx")

#### financial sector employment by occupation status

occup <- count(characterize(lfs_finance),indd01,wt = weight2)%>%
  rename(setNames("indd01", var_label(lfs$indd01))[1],Frequency=n)

ftab <- flextable(occup)
ftab <- colformat_double(
  x = ftab,
  big.mark = ",", digits = 2, na_str = "N/A"
)

ftab <- autofit(ftab)
std_border <- fp_border(color="gray")
ftab <- border_remove(x = ftab)
ftab <- hline(ftab, part="all", border = std_border )
std_borderv <- fp_border(color="gray")
ftab <- vline(ftab, border = std_borderv )
ftab <- border(ftab, border = fp_border(color = "grey"))
ftab <- border(ftab, border = fp_border(color = "grey"),part = "header")
ftab <- fontsize(ftab, size = 7, part = "header")
ftab <- fontsize(ftab, size = 6.5, part = "body")

# write_xlsx(occup,"data/occupation.xlsx")


#### financial sector employment by employment status

emp <- count(characterize(lfs_finance),D05,wt = weight2)%>%
  rename(setNames("D05", var_label(lfs$D05))[1],Frequency=n)

ftab <- flextable(emp)
ftab <- colformat_double(
  x = ftab,
  big.mark = ",", digits = 2, na_str = "N/A"
)

ftab <- autofit(ftab)
std_border <- fp_border(color="gray")
ftab <- border_remove(x = ftab)
ftab <- hline(ftab, part="all", border = std_border )
std_borderv <- fp_border(color="gray")
ftab <- vline(ftab, border = std_borderv )
ftab <- border(ftab, border = fp_border(color = "grey"))
ftab <- border(ftab, border = fp_border(color = "grey"),part = "header")
ftab <- fontsize(ftab, size = 7, part = "header")
ftab <- fontsize(ftab, size = 6.5, part = "body")

write_xlsx(emp,"data/employ_status.xlsx")

#### financial sector employment by education status

educ <- count(characterize(lfs_finance),B05C,wt = weight2)%>%
  rename(setNames("B05C", var_label(lfs$B05C))[1],Frequency=n)%>%
  mutate(Frequency_percentage = Frequency * 100/sum(Frequency),
         `ESCED name` = replace(`ESCED name`, which(`ESCED name`==""), "Not available"))

