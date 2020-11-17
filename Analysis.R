# to replace "anlaysis.Rmd"
source("libs.R") # load libraries
source("funs.R") # load functions

# data cleaning -----------------------------------------------------------

d <-data_read_john()
d <-data_clean_john(d) 
d <-data_churchdates_fix_john(d) # fix dates



# data exploration --------------------------------------------------------

show_correlations(d,"cooperative_reputation","religious_reputation")
show_correlations(d,"religious_reputation","church_donationN")
show_correlations(d,"cooperative_reputation","village_donationN")
show_correlations(d,"religious_reputation","church_attend")
show_correlations(d,"village_donationN","church_donationN")


No, to replicate the study John and Martin's study. Not that we really need to assess the ordering of the event within in date (whether first, second or third.)  I (JB) need to understand John's coding better before we can do that.


# model  observed church attendence by reported attendance ----------------

m1<-model_brm_church(d) # model 
show_coef_plot(m1) + ggtitle("")
sjPlot::tab_model(m1) # table


# predicton graph ---------------------------------------------------------

gp<-ggpredict(m1, terms =c("male","reported_attendanceF"))
p1<-plot(gp, facets = F, color="viridis") + scale_y_continuous(limits=c(0,1)) + theme_clean()
p1 + xlab("reported attendance (factor)") + ylab("probability of church attendance") + ggtitle("Predicted probabilities of church attendance for men and women")


```{r, cache=TRUE,dependson=js}
#now the main show, the mediation model, get data into shape

df1<-js%>%
 # dplyr::filter(church_at_riskF ==1)%>% # needs to be able to be at church
  dplyr::filter(!is.na(church_attend),!is.na(cooperative_reputation),!is.na(church_donationN))%>%
  dplyr::group_by(id, church_attend) %>%
  summarise(n = n()) %>%
  mutate(churchfreq = n / sum(n))%>%
  filter(church_attend !=0)%>%
  ungroup()%>%
  dplyr::select(id,churchfreq)
nrow(df1) ##44 
````


Ok, we merge this now.

```{r, echo = TRUE, cache=TRUE}
df2 <- js %>%
   dplyr::select(id, cooperative_reputation,church_donationN)%>%
  distinct(id,.keep_all = TRUE)

jj1 <-merge (df1, df2, by.y = 'id')
nrow(jj1)
# inspect histograms
hist(jj1$church_donationN, breaks=10) # zeven is modal
hist(log(jj1$cooperative_reputation+1)) # better 
```

Put reputation on the log scale. 

```{r,  echo=FALSE, cache=TRUE, dependson=jj1}
# try different approach
jj2 <- jj1 %>% 
dplyr::mutate(cooperativereputationLOG = (log(jj1$cooperative_reputation+1)),
churchdonationN = as.integer(church_donationN),
                churchfreq = as.numeric(churchfreq))
```

For th
```{r, echo=FALSE, cache=FALSE, dependson=jj2}
# BRMS mediation model # maybe priors here?
f1 <- bf(cooperativereputationLOG ~ churchfreq)
f2 <- bf(churchdonationN ~ churchfreq + cooperativereputationLOG,family = cumulative(link = "logit", threshold = "flexible"))
m2 <- brm(f1 + f2 + set_rescor(FALSE), data = jj2, cores = 4, iter=10000,
          file = here("m2"))
summary(m2)
```

Table for results
```{r}
#summary
tab_model(m2)
```
Code for a figure

```{r,  echo=TRUE, cache=FALSE}
library(bayestestR)
bbt1<-bayestestR::mediation(m2,centrality = "mean") 
library(ggpubr)
pt1<-bayestestR::describe_posterior(as.data.frame(m2),centrality = "mean", test="p_direction",ci=.8)%>%
  tibble::as_tibble(include.rownames = FALSE)%>%
  dplyr::slice(n=1:4)%>%
  dplyr::select(-CI)%>%
  mutate_if(is.numeric, ~round(., 3))%>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ttheme("mRed"))
pt1
pm1<-as.data.frame(bbt1)%>%
  dplyr::select(-proportion_mediated)%>%
  bayesplot::mcmc_intervals()%>%
  ggpar(main = "cooperative_reputationLOG")
dm1<-as.data.frame(bbt1)%>%
  dplyr::select(-proportion_mediated)%>%
  bayestestR::estimate_density()%>%
  plot(labels = FALSE)%>%
  ggpar(main = "")
r1<-brms::conditional_effects(m2, "churchfreq", resp ="cooperativereputationLOG")
r7<-brms::conditional_effects(m2, "churchfreq", resp ="churchdonationN", categorical =F)
r8<-brms::conditional_effects(m2, "cooperativereputationLOG", resp ="churchdonationN", categorical =F)
```

Plot 1
```{r}
r1
```

Plot 2
```{r}
r7
```

Plot 3
```{r}
r8
```


```{r,  mediation_plot, echo=FALSE, cache=TRUE}
med1<-ggarrange(ggarrange(pm1,dm1,labels=c("   i","ii")),ggarrange(pt1,r1, widths = c(1,1),labels=c("ii","iii")),nrow=2, heights=c(3,1))
med1
```

Needs stronger priors here (44 cases only). Looks like there is something here.  more to be done
