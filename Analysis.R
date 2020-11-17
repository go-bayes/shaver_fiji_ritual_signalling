# to replace "anlaysis.Rmd"
source("libs.R") # load libraries
source("funs.R") # load functions
devtools::install_github("ropensci/drake")
a# set theme
test<-read_johns_data()
test<-johns_clean_data(test)
head(test)

theme_set(theme_pubclean())

jsor%>%
  filter(is.na(church_date))%>%
  nrow()
# we observe that the dava dates differ from the church datas. 
jsor%>%
  filter(is.na(kava_date))%>%
  filter(is.na(church_date))%>%
  nrow()
# number of ids
length(unique(jsor$id)) # 50 people, 
# or another method of counting ideas.
jsor %>% 
  distinct(id) %>% 
  count()
# check date
print(jsor$church_date) # inconsistent data fomats format 
library(stringr)
# test a method for fixing (as I've not done this before)
fruits <- c("6/27/2010", "7/25/2010","7/25/10")
stringr::str_replace(fruits,"/10","/2010")
# method works 

# proceed to fix the dates
js1<-jsor %>% 
  dplyr::mutate(church_dateR = church_date)%>% # ccreate new column in case of SNAFU
  dplyr::mutate(church_dateR = stringr::str_replace_all(church_dateR, "/10","/2010")) # apply method
# checks slook good
head(js1$church_dateR)
head(js1$church_date)

# not sure what I did here so commenting out (consider deletin later)
# js1%>%
#   dplyr::mutate(church_dateR = as.Date(church_dateR,format = "%m/%d/%Y"))%>%
#   head()
```
```{r,  echo=TRUE, cache=FALSE}
# get data frame in read for analysis
js<-js1%>%
  dplyr::mutate(male =factor(ifelse(sex==1,"male","not_male"))) %>% # fix bad coding
  dplyr::mutate(id = factor(id) )%>% 
  dplyr::select(-church_at_risk) %>%  # we don't want this in our models -- if you can't attend it doesnt' count
  dplyr::mutate(church_attendF = factor(church_attend))%>% 
  dplyr::mutate(timeF = factor(time)) %>% 
  dplyr::mutate(church_event_noF = factor(church_event_no))%>%
  dplyr::mutate(church_dateF = factor(church_dateR),
                kava_dateF = factor(kava_date),
                reported_attendanceF = factor(reported_attendance),
                church_donationF = factor(church_donation),
                church_donationN = as.numeric(church_donationF),
                church_donationC = scale(church_donationN,center=T,scale=F),
                village_donationF = factor(village_donation),
                village_donationN = as.numeric(village_donationF),
                village_rankN = as.numeric(village_rank),
                village_rankS = scale(village_rankN))%>%
  arrange(id)
```


```{r,  echo=FALSE, include=FALSE}
#inspect data  commending this out because its not needed.
# js%>%
#   glimse()
# js %>% 
#   filter(is.na(church_dateR)) %>% 
#   nrow() #841
# js %>% 
#   filter(is.na(church_date)) %>% 
#   nrow()  # also 841  
# js%>%
#   filter(is.na(kava_date))%>%
#   nrow() # different analysis
# nrow(js) # this suggests kava dates and church dates differ.
# js%>%
#   dplyr::mutate(KavaAndChurch = factor(ifelse(!is.na(kava_dateF)&!is.na(church_dateF),1,0)))%>%
#   dplyr::filter(KavaAndChurch ==1)%>%
#   nrow()  # and indeed this is the case
# head(js)
# 1140/3241 # 35% NAs?
```

The purpose of our study is to investigate how religious attendance affects cooperation via religious reptuational pathways.

But before we get to that, we explore the data


```{r,  echo=TRUE, cache=TRUE}
# So, some basic correlations to investigate relationships. 
library(correlation)
library(see) # for plotting
library(ggraph) # needs to be loaded
js %>% 
  dplyr::select(cooperative_reputation,religious_reputation)%>%
  correlation(partial = TRUE) %>% 
  summary()
js %>% 
  dplyr::select(cooperative_reputation,church_donationN)%>%
  correlation(partial = TRUE) %>% 
  summary()
js %>% 
  dplyr::select(religious_reputation,church_donationN)%>%
  correlation(partial = TRUE) %>% 
  summary()
js %>% 
  dplyr::select(cooperative_reputation,village_donationN)%>%
  correlation(partial = TRUE) %>% 
  summary()
js %>% 
  dplyr::select(religious_reputation,village_donationN)%>%
  correlation(partial = TRUE) %>% 
  summary()
js %>% 
  dplyr::select(church_attend,church_donationN)%>%
  correlation(partial = TRUE) %>% 
  summary()
js %>% 
  dplyr::select(religious_reputation,church_donationN)%>%
  correlation(partial = TRUE) %>% 
  summary()
js %>% 
  dplyr::select(church_attend,church_donationN)%>%
  correlation(partial = TRUE) %>% 
  summary()
js %>% 
  dplyr::select(religious_reputation,village_donationN)%>%
  correlation(partial = TRUE) %>% 
  summary()
js %>% 
  dplyr::select(religious_reputation,reported_attendance)%>%
  correlation(partial = TRUE) %>% 
  summary()
# we could carry on with more correlations, but lets hold off on that
```


No, to replicate the study John and Martin's study. Not that we really need to assess the ordering of the event within in date (whether first, second or third.)  I (JB) need to understand John's coding better before we can do that.


```{r,  echo=TRUE, cache=TRUE,dependson="js"}
# Let's get on to some modelling. This is the model that John and Martin are running (I think)
library(brms)
# because I can't get the dates right, I'll assume need the date factor. 
# does reported attendance predict attendance bu wed 
# only the church data

m1 <-
  brm(data = js, family = bernoulli,
      church_attend  ~ male + reported_attendanceF  + (1|church_dateF/id),
      prior(normal(0, 10), class = Intercept),
      seed = 10,
      file = here("m1"))
#summary(m1) # ugly
```

```{r,  coefficient_plot_reported_attendance_predicts_attendance, echo=FALSE}
plot_model(m1,cache=TRUE)
```

```{r,  echo=FALSE, cache=TRUE,}
tab_model(m1)
```

Here is a figure showing the predicted probabilities for male/not males. I don't know the coding but I could guess. 


```{r echo=TRUE, cache =TRUE, dependson=m1}
gp<-ggpredict(m1, terms =c("male","reported_attendanceF"))
p1<-plot(gp, facets = F, color="viridis") + scale_y_continuous(limits=c(0,1)) + theme_clean() 
p1 + xlab("reported attendance (factor)") + ylab("probability of church attendance") + ggtitle("Predicted probabilities of church attendance for men and women")
```


Some data wrangling to get the proportions of church attendance. Note that we remove the "at risk" cases because you can only skip or attend church if you can skip or attend church. We removed those cases above. 


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
