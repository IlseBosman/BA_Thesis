##Loading packages
library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmice)
library(naniar)
library(lavaan)
library(tinytex)
library(tidyverse)
library(psych)
library(aod)
library(gridExtra)

#Loading youth datafiles
m_youth <- read_dta("m_youth.dta")
l_youth <- read_dta("l_youth.dta")
k_youth <- read_dta("k_youth.dta")

#Creating datafiles to only contain those aged 15+
m_youth15 <- filter(m_youth, m_age_dv >= 15)
l_youth15 <- filter(l_youth, l_age_dv >= 15)
k_youth15 <- filter(k_youth, k_age_dv >= 15)

#Outcome variable: checking which respondents are available in wave n
n_indall <- read_dta("THESIS/n_indall.dta")
data_n = n_indall %>% filter(pidp %in% m_youth15$pidp) %>% select(pidp, n_ivfio)
data_n %>% count(n_ivfio)
data_n$n_ivfio

m_indall <- read_dta("THESIS/m_indall.dta")
data_m = m_indall %>% filter(pidp %in% l_youth15$pidp) %>% select(pidp, m_ivfio)
data_m %>% count(m_ivfio)
data_m$m_ivfio

l_indall <- read_dta("THESIS/l_indall.dta")
data_l = l_indall %>% filter(pidp %in% k_youth15$pidp) %>% select(pidp, l_ivfio)
data_l %>% count(l_ivfio)
data_l$l_ivfio

m_youth15_1 <- data_n %>% inner_join(n_indall %>% select(pidp,n_ivfio)) %>%
  filter(!n_ivfio %in% c(21,25,84))
l_youth15_1 <- data_m %>% inner_join(m_indall %>% select(pidp,m_ivfio)) %>%
  filter(!m_ivfio %in% c(21,25,22,84))
k_youth15_1 <- data_l %>% inner_join(l_indall %>% select(pidp,l_ivfio)) %>%
  filter(!l_ivfio %in% c(21,25,22,84))

m_youth15_1 = m_youth15 %>% inner_join(n_indall %>% select(pidp,n_ivfio)) %>%
  filter(!n_ivfio %in% c(21,25,84)) %>%
  mutate(response=ifelse(n_ivfio==1,1,0))
l_youth15_1 = l_youth15 %>% inner_join(m_indall %>% select(pidp,m_ivfio)) %>%
  filter(!m_ivfio %in% c(21,25,22,84)) %>%
  mutate(response=ifelse(m_ivfio==1,1,0))
k_youth15_1 = k_youth15 %>% inner_join(l_indall %>% select(pidp,l_ivfio)) %>%
  filter(!l_ivfio %in% c(21,25,22,84)) %>%
  mutate(response=ifelse(l_ivfio==1,1,0))

# L does not contain all relevant variables and needs to borrow from K (previous wave)
k_youth_l <- k_youth %>% filter(pidp %in% l_youth15_1$pidp)

borrow_from = k_youth %>% select(k_ypfamsup,k_ypargm,k_yptlkm,
                                 
                                 k_ypparsch,k_ypmisbsch,k_ypfrobully,
                                 
                                 k_ypfrpbully,k_ypsdqcp_dv,k_ypsdqes_dv,
                                 
                                 k_ypsdqha_dv,k_ypsdqpp_dv,k_ypsdqps_dv,
                                 
                                 k_ypsdqtd_dv,pidp) %>%
  
  filter(pidp %in% l_youth15_1$pidp)
l_youth15_1=l_youth15_1 %>% inner_join(borrow_from)

#Renaming relevant variables
m_youth15_1 <- m_youth15_1 %>%
  rename(
    sex = m_ypsex,
    ethnicity = m_ethn_dv,
    feeling_supported_by_family = m_ypfamsup,
    quarrel_with_mother = m_ypargm,
    important_talks_with_mother = m_yptlkm,
    happiness_about_family = m_yphfm,
    parents_interest_in_school = m_ypparsch,
    age = m_age_dv,
    importance_school_exams = m_ypacvwell,
    future_plans = m_yplvsc2do,
    misbehaviour_school = m_ypmisbsch,
    frequency_bullying_otherways = m_ypfrobully,
    frequency_physical_bullying = m_ypfrpbully,
    SDQconduct_problems = m_ypsdqcp_dv,
    SDQemotional_symptoms = m_ypsdqes_dv,
    SDQhyperactivity = m_ypsdqha_dv,
    SDQpeer_problems = m_ypsdqpp_dv,
    SDQprosocial = m_ypsdqps_dv)

l_youth15_1 <- l_youth15_1 %>%
  rename(
    sex = l_ypsex,
    ethnicity = l_ethn_dv,
    feeling_supported_by_family = k_ypfamsup,
    quarrel_with_mother = k_ypargm,
    important_talks_with_mother = k_yptlkm,
    happiness_about_family = l_yphfm,
    parents_interest_in_school = k_ypparsch,
    age = l_age_dv,
    importance_school_exams = l_ypacvwell,
    future_plans = l_yplvsc2do,
    misbehaviour_school = k_ypmisbsch,
    frequency_bullying_otherways = k_ypfrobully,
    frequency_physical_bullying = k_ypfrpbully,
    SDQconduct_problems = k_ypsdqcp_dv,
    SDQemotional_symptoms = k_ypsdqes_dv,
    SDQhyperactivity = k_ypsdqha_dv,
    SDQpeer_problems = k_ypsdqpp_dv,
    SDQprosocial = k_ypsdqps_dv)

k_youth15_1 <- k_youth15_1 %>%
  rename(
    sex = k_ypsex,
    ethnicity = k_ethn_dv,
    feeling_supported_by_family = k_ypfamsup,
    quarrel_with_mother = k_ypargm,
    important_talks_with_mother = k_yptlkm,
    happiness_about_family = k_yphfm,
    parents_interest_in_school = k_ypparsch,
    age = k_age_dv,
    importance_school_exams = k_ypacvwell,
    future_plans = k_yplvsc2do,
    misbehaviour_school = k_ypmisbsch,
    frequency_bullying_otherways = k_ypfrobully,
    frequency_physical_bullying = k_ypfrpbully,
    SDQconduct_problems = k_ypsdqcp_dv,
    SDQemotional_symptoms = k_ypsdqes_dv,
    SDQhyperactivity = k_ypsdqha_dv,
    SDQpeer_problems = k_ypsdqpp_dv,
    SDQprosocial = k_ypsdqps_dv)

#Retrieving Household size from other datafile
m_indresp <- read_dta("THESIS/m_indresp.dta")
l_indresp <- read_dta("THESIS/l_indresp.dta")
k_indresp <- read_dta("THESIS/k_indresp.dta")

hh_size_m <- m_indresp %>% filter(m_hidp %in% m_youth15_1$m_hidp) %>%
  select(m_hidp, m_hhsize) %>% unique()
m_youth15_2 <- m_youth15_1 %>% 
  inner_join(hh_size_m, by = c("m_hidp" = "m_hidp"))

hh_size_l <- l_indresp %>% filter(l_hidp %in% l_youth15_1$l_hidp) %>%
  select(l_hidp, l_hhsize) %>% unique()
hh_size_l <- hh_size_l %>% group_by(l_hidp) %>% summarise(l_hhsize = unique(l_hhsize)[1])
l_youth15_2 <- l_youth15_1 %>% 
  inner_join(hh_size_l, by = c("l_hidp" = "l_hidp"))

hh_size_k <- k_indresp %>% filter(k_hidp %in% k_youth15_1$k_hidp) %>%
  select(k_hidp, k_hhsize) %>% unique()
k_youth15_2 <- k_youth15_1 %>% 
  inner_join(hh_size_k, by = c("k_hidp" = "k_hidp"))

#Parenting style wave m
youthidentifiers_m = m_youth15_2 %>%
  select(pidp)

g_child <- read_dta("g_child.dta")
g_child_psdq_m = g_child %>%filter(pidp %in% youthidentifiers_m$pidp) %>% 
  select(pidp, g_psdqa1_dv,g_psdqa2_dv,g_psdqb1_dv,g_psdqb2_dv,g_psdqc1_dv,g_psdqc2_dv,g_psdq1pno,g_psdq2pno)
g_child_psdq_m %>% count(g_psdqa1_dv)

h_child <- read_dta("h_child.dta")
h_child_psdq_m = h_child %>%filter(pidp %in% youthidentifiers_m$pidp) %>% select(pidp, h_psdqa1_dv,h_psdqa2_dv,h_psdqb1_dv,h_psdqb2_dv,h_psdqc1_dv,h_psdqc2_dv,h_psdq1pno,h_psdq2pno)
h_child_psdq_m %>% count(h_psdqa1_dv)

i_child <- read_dta("i_child.dta")
i_child_psdq_m = i_child %>%filter(pidp %in% youthidentifiers_m$pidp) %>% select(pidp, i_psdqa1_dv,i_psdqa2_dv,i_psdqb1_dv,i_psdqb2_dv,i_psdqc1_dv,i_psdqc2_dv,i_psdq1pno,i_psdq2pno)
i_child_psdq_m %>% count(i_psdqa1_dv)

#Checking occurrences
g_child_psdq_m %>% count(g_psdq1pno)
g_child_psdq_m %>% count(g_psdqa1_dv) 
g_child_psdq_m %>% count(g_psdqb1_dv) 
g_child_psdq_m %>% count(g_psdqc1_dv) 
g_child_psdq_m %>% count(g_psdq2pno)
g_child_psdq_m %>% count(g_psdqa2_dv)
g_child_psdq_m %>% count(g_psdqb1_dv)
g_child_psdq_m %>% count(g_psdqc1_dv)

h_child_psdq_m %>% count(h_psdq1pno)
h_child_psdq_m %>% count(h_psdqa1_dv)
h_child_psdq_m %>% count(h_psdqb1_dv)
h_child_psdq_m %>% count(h_psdqc1_dv)
h_child_psdq_m %>% count(h_psdq2pno)
h_child_psdq_m %>% count(h_psdqa2_dv)
h_child_psdq_m %>% count(h_psdqb1_dv)
h_child_psdq_m %>% count(h_psdqc1_dv)

i_child_psdq_m %>% count(i_psdq1pno)
i_child_psdq_m %>% count(i_psdqa1_dv)
i_child_psdq_m %>% count(i_psdqb1_dv)
i_child_psdq_m %>% count(i_psdqc1_dv)
i_child_psdq_m %>% count(i_psdq2pno)
i_child_psdq_m %>% count(i_psdqa2_dv)
i_child_psdq_m %>% count(i_psdqb1_dv)
i_child_psdq_m %>% count(i_psdqc1_dv)

g_child_1_m <- g_child_psdq_m %>%
  filter(!g_psdqa1_dv %in% c(-8, -9) & !g_psdqa2_dv %in% c(-8, -9)) %>%
  filter(!g_psdqb1_dv %in% c(-8, -9) & !g_psdqb2_dv %in% c(-8, -9)) %>%
  filter(!g_psdqc1_dv %in% c(-8, -9) & !g_psdqc2_dv %in% c(-8, -9)) %>%
  mutate(mean_authoritative = rowMeans(data.frame(g_psdqa1_dv, g_psdqa2_dv), na.rm = TRUE),
         mean_authorarian = rowMeans(data.frame(g_psdqb1_dv, g_psdqb2_dv), na.rm = TRUE),
         mean_permissive = rowMeans(data.frame(g_psdqc1_dv, g_psdqc2_dv), na.rm = TRUE)) %>%
  select(pidp, mean_authoritative, mean_authorarian, mean_permissive)


h_child_1_m <- h_child_psdq_m %>%
  filter(!h_psdqa1_dv %in% c(-8, -9) & !h_psdqa2_dv %in% c(-8, -9)) %>%
  filter(!h_psdqb1_dv %in% c(-8, -9) & !h_psdqb2_dv %in% c(-8, -9)) %>%
  filter(!h_psdqc1_dv %in% c(-8, -9) & !h_psdqc2_dv %in% c(-8, -9)) %>%
  mutate(mean_authoritative = rowMeans(data.frame(h_psdqa1_dv, h_psdqa2_dv), na.rm = TRUE),
         mean_authorarian = rowMeans(data.frame(h_psdqb1_dv, h_psdqb2_dv), na.rm = TRUE),
         mean_permissive = rowMeans(data.frame(h_psdqc1_dv, h_psdqc2_dv), na.rm = TRUE)) %>%
  select(pidp, mean_authoritative, mean_authorarian, mean_permissive)

i_child_1_m <- i_child_psdq_m %>%
  filter(!i_psdqa1_dv %in% c(-8, -9) & !i_psdqa2_dv %in% c(-8, -9)) %>%
  filter(!i_psdqb1_dv %in% c(-8, -9) & !i_psdqb2_dv %in% c(-8, -9)) %>%
  filter(!i_psdqc1_dv %in% c(-8, -9) & !i_psdqc2_dv %in% c(-8, -9)) %>%
  mutate(mean_authoritative = rowMeans(data.frame(i_psdqa1_dv, i_psdqa2_dv), na.rm = TRUE),
         mean_authorarian = rowMeans(data.frame(i_psdqb1_dv, i_psdqb2_dv), na.rm = TRUE),
         mean_permissive = rowMeans(data.frame(i_psdqc1_dv, i_psdqc2_dv), na.rm = TRUE)) %>%
  select(pidp, mean_authoritative, mean_authorarian, mean_permissive)

PSDQall_m = h_child_1_m %>%
  bind_rows(i_child_1_m %>% filter(!pidp %in% h_child_1_m$pidp)) %>%
  bind_rows(g_child_1_m %>% filter(!pidp %in% h_child_1_m$pidp)) %>%
  filter(!is.na(mean_authoritative) & !is.na(mean_authorarian) & !is.na(mean_permissive))

m_youth15_3 <- m_youth15_2 %>%
  left_join(PSDQall_m, by = "pidp")

#Parenting style wave l
youthidentifiers_l = l_youth15_2 %>%
  select(pidp)

g_child_psdq_l = g_child %>%filter(pidp %in% youthidentifiers_l$pidp) %>% 
  select(pidp, g_psdqa1_dv,g_psdqa2_dv,g_psdqb1_dv,g_psdqb2_dv,g_psdqc1_dv,g_psdqc2_dv,g_psdq1pno,g_psdq2pno)
g_child_psdq_l %>% count(g_psdqa1_dv)

f_child <- read_dta("f_child.dta")
f_child_psdq_l = f_child %>%filter(pidp %in% youthidentifiers_l$pidp) %>% 
  select(pidp, f_psdqa1_dv,f_psdqa2_dv,f_psdqb1_dv,f_psdqb2_dv,f_psdqc1_dv,f_psdqc2_dv,f_psdq1pno,f_psdq2pno)
f_child_psdq_l %>% count(f_psdqa1_dv)

#Checking occurrences
f_child_psdq_l %>% count(f_psdq1pno)
f_child_psdq_l %>% count(f_psdqa1_dv) 
f_child_psdq_l %>% count(f_psdqb1_dv) 
f_child_psdq_l %>% count(f_psdqc1_dv) 
f_child_psdq_l %>% count(f_psdq2pno)
f_child_psdq_l %>% count(f_psdqa2_dv)
f_child_psdq_l %>% count(f_psdqb1_dv)
f_child_psdq_l %>% count(f_psdqc1_dv)

g_child_psdq_l %>% count(g_psdq1pno)
g_child_psdq_l %>% count(g_psdqa1_dv) 
g_child_psdq_l %>% count(g_psdqb1_dv) 
g_child_psdq_l %>% count(g_psdqc1_dv) 
g_child_psdq_l %>% count(g_psdq2pno)
g_child_psdq_l %>% count(g_psdqa2_dv)
g_child_psdq_l %>% count(g_psdqb1_dv)
g_child_psdq_l %>% count(g_psdqc1_dv)

g_child_1_l <- g_child_psdq_l %>%
  filter(!g_psdqa1_dv %in% c(-8, -9) & !g_psdqa2_dv %in% c(-8, -9)) %>%
  filter(!g_psdqb1_dv %in% c(-8, -9) & !g_psdqb2_dv %in% c(-8, -9)) %>%
  filter(!g_psdqc1_dv %in% c(-8, -9) & !g_psdqc2_dv %in% c(-8, -9)) %>%
  mutate(mean_authoritative = rowMeans(data.frame(g_psdqa1_dv, g_psdqa2_dv), na.rm = TRUE),
         mean_authorarian = rowMeans(data.frame(g_psdqb1_dv, g_psdqb2_dv), na.rm = TRUE),
         mean_permissive = rowMeans(data.frame(g_psdqc1_dv, g_psdqc2_dv), na.rm = TRUE)) %>%
  select(pidp, mean_authoritative, mean_authorarian, mean_permissive)

f_child_1_l <- f_child_psdq_l %>%
  filter(!f_psdqa1_dv %in% c(-8, -9) & !f_psdqa2_dv %in% c(-8, -9)) %>%
  filter(!f_psdqb1_dv %in% c(-8, -9) & !f_psdqb2_dv %in% c(-8, -9)) %>%
  filter(!f_psdqc1_dv %in% c(-8, -9) & !f_psdqc2_dv %in% c(-8, -9)) %>%
  mutate(mean_authoritative = rowMeans(data.frame(f_psdqa1_dv, f_psdqa2_dv), na.rm = TRUE),
         mean_authorarian = rowMeans(data.frame(f_psdqb1_dv, f_psdqb2_dv), na.rm = TRUE),
         mean_permissive = rowMeans(data.frame(f_psdqc1_dv, f_psdqc2_dv), na.rm = TRUE)) %>%
  select(pidp, mean_authoritative, mean_authorarian, mean_permissive)

PSDQall_l = f_child_1_l %>%
  bind_rows(g_child_1_l %>% filter(!pidp %in% f_child_1_l$pidp)) %>%
  filter(!is.na(mean_authoritative) & !is.na(mean_authorarian) & !is.na(mean_permissive))

#Adding Parenting style
l_youth15_3 <- l_youth15_2 %>%
  left_join(PSDQall_l, by = "pidp")

#Wave k
youthidentifiers_k = k_youth15_2 %>%
  select(pidp)

g_child <- read_dta("THESIS/g_child.dta")
g_child_psdq_k = g_child %>%filter(pidp %in% youthidentifiers_k$pidp) %>% 
  select(pidp, g_psdqa1_dv,g_psdqa2_dv,g_psdqb1_dv,g_psdqb2_dv,g_psdqc1_dv,g_psdqc2_dv,g_psdq1pno,g_psdq2pno)
g_child_psdq_k %>% count(g_psdqa1_dv)

f_child<- read_dta("f_child.dta")
f_child_psdq_k = f_child %>%filter(pidp %in% youthidentifiers_k$pidp) %>% 
  select(pidp, f_psdqa1_dv,f_psdqa2_dv,f_psdqb1_dv,f_psdqb2_dv,f_psdqc1_dv,f_psdqc2_dv,f_psdq1pno,f_psdq2pno)
f_child_psdq_k %>% count(f_psdqa1_dv)

e_child <- read_dta("e_child.dta")
e_child_psdq_k = e_child %>%filter(pidp %in% youthidentifiers_k$pidp) %>% 
  select(pidp, e_psdqa1_dv,e_psdqa2_dv,e_psdqb1_dv,e_psdqb2_dv,e_psdqc1_dv,e_psdqc2_dv,e_psdq1pno,e_psdq2pno)
e_child_psdq_k %>% count(e_psdqa1_dv)

#Checking occurrences
f_child_psdq_k %>% count(f_psdq1pno)
f_child_psdq_k %>% count(f_psdqa1_dv) 
f_child_psdq_k %>% count(f_psdqb1_dv) 
f_child_psdq_k %>% count(f_psdqc1_dv) 
f_child_psdq_k %>% count(f_psdq2pno)
f_child_psdq_k %>% count(f_psdqa2_dv)
f_child_psdq_k %>% count(f_psdqb1_dv)
f_child_psdq_k %>% count(f_psdqc1_dv)

e_child_psdq_k %>% count(e_psdq1pno)
e_child_psdq_k %>% count(e_psdqa1_dv)
e_child_psdq_k %>% count(e_psdqb1_dv)
e_child_psdq_k %>% count(e_psdqc1_dv)
e_child_psdq_k %>% count(e_psdq2pno)
e_child_psdq_k %>% count(e_psdqa2_dv)
e_child_psdq_k %>% count(e_psdqb1_dv)
e_child_psdq_k %>% count(e_psdqc1_dv)

g_child_psdq_k %>% count(g_psdq1pno)
g_child_psdq_k %>% count(g_psdqa1_dv) 
g_child_psdq_k %>% count(g_psdqb1_dv) 
g_child_psdq_k %>% count(g_psdqc1_dv) 
g_child_psdq_k %>% count(g_psdq2pno)
g_child_psdq_k %>% count(g_psdqa2_dv)
g_child_psdq_k %>% count(g_psdqb1_dv)
g_child_psdq_k %>% count(g_psdqc1_dv)

g_child_1_k <- g_child_psdq_k %>%
  filter(!g_psdqa1_dv %in% c(-8, -9) & !g_psdqa2_dv %in% c(-8, -9)) %>%
  filter(!g_psdqb1_dv %in% c(-8, -9) & !g_psdqb2_dv %in% c(-8, -9)) %>%
  filter(!g_psdqc1_dv %in% c(-8, -9) & !g_psdqc2_dv %in% c(-8, -9)) %>%
  mutate(mean_authoritative = rowMeans(data.frame(g_psdqa1_dv, g_psdqa2_dv), na.rm = TRUE),
         mean_authorarian = rowMeans(data.frame(g_psdqb1_dv, g_psdqb2_dv), na.rm = TRUE),
         mean_permissive = rowMeans(data.frame(g_psdqc1_dv, g_psdqc2_dv), na.rm = TRUE)) %>%
  select(pidp, mean_authoritative, mean_authorarian, mean_permissive)


e_child_1_k <- e_child_psdq_k %>%
  filter(!e_psdqa1_dv %in% c(-8, -9) & !e_psdqa2_dv %in% c(-8, -9)) %>%
  filter(!e_psdqb1_dv %in% c(-8, -9) & !e_psdqb2_dv %in% c(-8, -9)) %>%
  filter(!e_psdqc1_dv %in% c(-8, -9) & !e_psdqc2_dv %in% c(-8, -9)) %>%
  mutate(mean_authoritative = rowMeans(data.frame(e_psdqa1_dv, e_psdqa2_dv), na.rm = TRUE),
         mean_authorarian = rowMeans(data.frame(e_psdqb1_dv, e_psdqb2_dv), na.rm = TRUE),
         mean_permissive = rowMeans(data.frame(e_psdqc1_dv, e_psdqc2_dv), na.rm = TRUE)) %>%
  select(pidp, mean_authoritative, mean_authorarian, mean_permissive)


f_child_1_k <- f_child_psdq_k %>%
  filter(!f_psdqa1_dv %in% c(-8, -9) & !f_psdqa2_dv %in% c(-8, -9)) %>%
  filter(!f_psdqb1_dv %in% c(-8, -9) & !f_psdqb2_dv %in% c(-8, -9)) %>%
  filter(!f_psdqc1_dv %in% c(-8, -9) & !f_psdqc2_dv %in% c(-8, -9)) %>%
  mutate(mean_authoritative = rowMeans(data.frame(f_psdqa1_dv, f_psdqa2_dv), na.rm = TRUE),
         mean_authorarian = rowMeans(data.frame(f_psdqb1_dv, f_psdqb2_dv), na.rm = TRUE),
         mean_permissive = rowMeans(data.frame(f_psdqc1_dv, f_psdqc2_dv), na.rm = TRUE)) %>%
  select(pidp, mean_authoritative, mean_authorarian, mean_permissive)


PSDQall_k = f_child_1_k %>%
  bind_rows(e_child_1_k %>% filter(!pidp %in% f_child_1_k$pidp)) %>%
  bind_rows(g_child_1_k %>% filter(!pidp %in% f_child_1_k$pidp)) %>%
  filter(!is.na(mean_authoritative) & !is.na(mean_authorarian) & !is.na(mean_permissive))

k_youth15_3 <- k_youth15_2 %>%
  left_join(PSDQall_k, by = "pidp")

#Creating data file with all relevant variables
youth15mrelevant <- m_youth15_3[ ,c("pidp", "m_pno", "m_hidp", "sex", "age", "ethnicity", "feeling_supported_by_family", "quarrel_with_mother", "important_talks_with_mother", "happiness_about_family", "parents_interest_in_school", "future_plans", "importance_school_exams", "frequency_physical_bullying", "frequency_bullying_otherways", "misbehaviour_school", "SDQemotional_symptoms", "SDQconduct_problems", "SDQhyperactivity", "SDQpeer_problems", "SDQprosocial", "mean_authoritative", "mean_authorarian", "mean_permissive", "m_hhsize", "response")]  
youth15mrelevant <- youth15mrelevant %>%
  rename(personnumber = m_pno,
         household_size = m_hhsize,
         householdnumber = m_hidp)

youth15lrelevant <- l_youth15_3[ ,c("pidp", "l_pno", "l_hidp", "sex", "age", "ethnicity", "feeling_supported_by_family", "quarrel_with_mother", "important_talks_with_mother", "happiness_about_family", "parents_interest_in_school", "future_plans", "importance_school_exams", "frequency_physical_bullying", "frequency_bullying_otherways", "misbehaviour_school", "SDQemotional_symptoms", "SDQconduct_problems", "SDQhyperactivity", "SDQpeer_problems", "SDQprosocial", "mean_authoritative", "mean_authorarian", "mean_permissive", "l_hhsize", "response")]  
youth15lrelevant <- youth15lrelevant %>%
  rename(personnumber = l_pno,
         household_size = l_hhsize,
         householdnumber = l_hidp)

youth15krelevant <- k_youth15_3[ ,c("pidp", "k_pno", "k_hidp", "sex", "age", "ethnicity", "feeling_supported_by_family", "quarrel_with_mother", "important_talks_with_mother", "happiness_about_family", "parents_interest_in_school", "future_plans", "importance_school_exams", "frequency_physical_bullying", "frequency_bullying_otherways", "misbehaviour_school", "SDQemotional_symptoms", "SDQconduct_problems", "SDQhyperactivity", "SDQpeer_problems", "SDQprosocial", "mean_authoritative", "mean_authorarian", "mean_permissive", "k_hhsize", "response")]  
youth15krelevant <- youth15krelevant %>%
  rename(personnumber = k_pno,
         household_size = k_hhsize,
         householdnumber = k_hidp)

#Joining the datasets
youth15combined <- rbind(youth15mrelevant, youth15lrelevant, youth15krelevant)
youth15combined[youth15combined == -9] <- NA
youth15combined[youth15combined == -8] <- NA
youth15combined[youth15combined == -1] <- NA