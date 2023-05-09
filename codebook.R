#Setup

library(tidyverse)
library(haven)
library(naniar)
library(magrittr)

wdi = read_csv('codebook original ALT.csv',na='..')

#Data cleaning
wdi2 = wdi [-(37:41),]

wdi2 = select(wdi2,cols=-4)

wdi3 = 
  wdi2 %>%
  pivot_longer(
    cols = 4:14, 
    names_to = 'Year', 
    names_transform = list(Year = as.integer),
    values_to = 'value'
  )

wdi4 =
  wdi3 %>%
  pivot_wider(
    names_from = 'Series Name',
    values_from = value
  )

wdi4 = 
  wdi4 %>% replace_na(list(`Current health expenditure (% of GDP)`=-99))

wdi5 = 
  wdi4 %>%
  mutate(
    ID = row_number(), 
    .before = 1
  ) %>%
  rename(
    CName = `Country Name`,
    CCode = `Country Code`,
    GDP = `GDP (constant 2015 US$)`,
    GDPPC = `GDP per capita (constant 2015 US$)`,
    MilExpPerc = `Military expenditure (% of GDP)`,
    IntUsePerc = `Individuals using the Internet (% of population)`,
    HealthExpPerc = `Current health expenditure (% of GDP)`,
    LifeExp = `Life expectancy at birth, total (years)`
  )

FctWhen = function(...) {
  args = rlang::list2(...)
  rhs = map(args, rlang::f_rhs)
  cases = case_when( !!!args )
  exec(fct_relevel, cases, !!!rhs)
}  

wdi5 %<>%
  mutate(
    MilExpFact = FctWhen(
      MilExpPerc <= 2 ~ 'Low',
      MilExpPerc >2 & MilExpPerc <3 ~ 'Mid',
      MilExpPerc >= 3 ~ 'High'
    )
  )

wdi5 %<>%
  mutate(
    IntUseFact = FctWhen(
      IntUsePerc <= 80 ~ 'Low',
      IntUsePerc >= 80 ~ 'High'
    )
  )

wdi6 =
  wdi5 %>%
  relocate(starts_with('MilExpFact'), .before = IntUsePerc)
           
wdi6 =
  wdi6 %>% 
  relocate(starts_with('IntUseFact'), .before = HealthExpPerc)

#Summarize the data


sumfunc = function(.data,x){
  x = enquo(x)
  summarize(.data, min(!!x),
            mean(!!x),
            median(!!x),
            max(!!x)) %>% 
    knitr::kable(format = 'pipe', digits = 1L)
}
  
sumfunc(wdi6,GDP)
sumfunc(wdi6,GDPPC)
sumfunc(wdi6,MilExpPerc) 
sumfunc(wdi6,IntUsePerc)
sumfunc(wdi6,HealthExpPerc)
sumfunc(wdi6,LifeExp)



count(wdi6, MilExpFact) %>%
  mutate(
    values = as.numeric(MilExpFact),
    labels = as_factor(MilExpFact),
    freq = n,
    perc = n/sum(n)*100,
    .keep = 'unused'
  ) %>%
  knitr::kable(format = 'pipe', digits = 1L)

count(wdi6, IntUseFact) %>%
  mutate(
    values = as.numeric(IntUseFact),
    labels = as_factor(IntUseFact),
    freq = n,
    perc = n/sum(n)*100,
    .keep = 'unused'
  ) %>%
  knitr::kable(format = 'pipe', digits = 1L)

#Export the files
write_csv(wdi6, 'codebook wdi.csv')
save(wdi6, file='codebook wdi.RData')
