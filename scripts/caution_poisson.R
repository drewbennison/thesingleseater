cf <- c(4,1,2,0,2,3)
options(scipen=999)

cr <- tibble(n=-1, pr=-1)
for(i in c(0:10)){
 dpr <- dpois(i, mean(cf))
 cr <- cr %>% add_row(n=i, pr=dpr)
}

cr %>% filter(n!=-1) %>% 
  ggplot(aes(x=n, y=pr)) + geom_col()
