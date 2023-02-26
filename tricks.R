require(tidyverse)
mpg_temp <- mpg%>%
  mutate_if(is.character,as.factor)
mpg_temp1<-mpg_temp %>%
  select_if(is.factor)
unique(mpg_temp1)
for (i in c(colnames(mpg_temp1))){
  #print(i)
  unique(mpg_temp1[i])
}
unlist(lapply(mpg_temp1, function(x) length(unique(x))))
lapply(mpg_temp1, function(x) unique(x))


