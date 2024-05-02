library(corrplot)
library(dplyr)
library(emmeans)
library(fastDummies)
library(ggalluvial)
library(ggplot2)
library(ggstatsplot)
library(gtsummary)
library(haven)
library(Hmisc)
library(interactions)
library(jtools)
library(labelled)
library(lavaan)
library(lme4)
library(ltm)
library(margins)
library(misty)
library(prediction)
library(reshape2)
library(scaleAlign)
library(see)
library(SIMR)
library(sjPlot)
library(TAM)
library(tidyr)
library(tidyverse)

########################### All population ###############################
data.all <- read_sav("C:/Users/rqa2a/OneDrive - Middle Tennessee State University/Research - ReCCEE Projects/PCoRE - Everyone Project/Data/22 11 03 recoded in SPSS.sav")
table(data.all$course) #11995 in 72 course
#remove courses containing under 20 students
course_counts <- table(data.all$course) # Get the counts of each course
filtered_courses <- names(course_counts[course_counts >= 20]) # Filter out the courses with less than 20 students
df<- data.all[data.all$course %in% filtered_courses, ] # Subset the original data to only include the filtered courses

########################### Naming Variables###############################
#df$cc1 = Scientists and religious people cannot see eye to eye about evolution.
# 5 strongly disagree
# reversed to cc1fixed where 5 is strongly agree = pconf3

#chose variables and rename
df<-df %>% select(cc1, cc1fixed, cc2, cc2fixed, cc3, cc3fixed, cc4, cc4fixed, cc5, cc5fixed, cc6, cc6fixed,
                  ResponseId, religion3, course, biomajor, institution, gender, semester, raceoriginal) %>%
  rename(pconf1 = cc3fixed,
         pconf2 = cc5fixed,
         pconf3 = cc1fixed,
         pcomp1 = cc4,
         pcomp2 = cc6,
         pcomp3 = cc2,) %>% 
  mutate(
    pconf = (pconf1 + pconf2 + pconf3) / 3,
    pcomp = (pcomp1 + pcomp2 + pcomp3) / 3) %>%
  drop_na(religion3, pcomp, pconf) #now 11,428

#Rename variable
df$religion3 <- as.factor(df$religion3)
df$religion3 <- relevel (df$religion3, ref =  "1")
df$religion3 <- recode(df$religion3, '1' = "Atheist", '2'= "Agnostic", '3'= "Christian - Protestant", '4'="Christian - CJCLDS",
                       '5' = "Christian - nondenominational", '6'= "Christian - Catholic", '7'= "Jewish",
                       '8' = "Muslim", '9'= "Hindu", '10'= "Buddhist", '11'="Other religion")

df$biomajor<-to_character(df$biomajor)
df$biomajor <-as.factor(df$biomajor)
df$gender<-to_character(df$gender)
df$gender <-as.factor(df$gender)
df$raceoriginal<-to_character(df$raceoriginal)
df$raceoriginal <-as.factor(df$raceoriginal)
df$semester<-to_character(df$semester, levels=c("Fall 2018", "Spring 2020", "Fall 2020", "Spring 2021"))
df$semester <-as.factor(df$semester)

#demographic information #TABLE 2
dem.acc <- df %>% select(semester, gender, raceoriginal, biomajor, religion3)
dem.acc %>% tbl_summary(label = list(semester ~ "Semester",gender ~ "Gender",
                                     raceoriginal ~ "Race/ethnicity", biomajor ~ "Bio Major", religion3 ~ "Religion"))

cor.matrix<-cor(df[c("pcomp1", "pcomp2", "pcomp3", "pconf1", "pconf2", "pconf3")], use= "na.or.complete")
corrplot(cor.matrix, method = "number") #correlation matrix between all RAW items on the perceived conflict scale


########################### Validity Evidence ###############################
com<-df[c("pcomp1", "pcomp2", "pcomp3", "pconf1", "pconf2", "pconf3")]
# Convert all variables in 'com' to numeric
com[] <- lapply(com, as.numeric)
cronbach.alpha(com[c("pcomp1", "pcomp2", "pcomp3")], na.rm = TRUE) #alpha: 0.73
cronbach.alpha(com[c("pconf1", "pconf2", "pconf3")], na.rm = TRUE) #alpha: 0.655

#EFA
library(psych)
efa_result.k <- fa(com, nfactors = 2, rotate = "oblimin", scores = "regression", fm = "ml")
print(efa_result.k)

#CFA
#two factor solution
two<- ' compatibility =~ pcomp1 + pcomp2 + pcomp3
conflict =~ pconf1 + pconf2 + pconf3'
two.fit <- cfa(two, data=com, std.lv=TRUE, estimator = "DWLS")
summary(two.fit, standardized=TRUE, fit.measures=TRUE)
fitMeasures(two.fit, c("cfi", "rmsea", "srmr"))
#cfi rmsea  srmr 
#0.950 0.087 0.067 

########################### Design Effect ###############################

round(deff(df$pconf, df$course), 2)
#n clusters      rho     deff 
#11428.00    63.00     0.06    19.98 

#the deff is 19.98, which means that the variance of our estimate is 19.98 times 
#larger than what it would be if I had used a simple random sample.
#This suggests that there is a considerable amount of clustering in our data, 
#and that the variable class may indeed be a significant source of this clustering.
round(deff(df$pcomp, df$course), 2)

round(deff(df$pconf, df$institution), 2)
round(deff(df$pcomp, df$institution), 2)

########################### Linear Mixed Model ###############################
################ Perceived conflict
round(mean(df$pconf),2)
round(sd(df$pconf),2)

conf.data <-df %>% select(pconf, religion3, biomajor, institution, course) %>%
  na.omit()
conf.data$religion3 <- relevel (conf.data$religion3, ref =  "Atheist")
conf.data$religion3<-factor(conf.data$religion3, levels=c("Atheist", "Agnostic", "Buddhist", "Christian - Catholic", 
                                                          "Christian - CJCLDS", "Christian - nondenominational","Christian - Protestant",
                                                          "Hindu", "Jewish", "Muslim", "Other religion"))
#Model
conf<-lmer(pconf ~ religion3 + biomajor + (1 | institution) + (1 | course), data = conf.data)
summary(conf)
plot_model(conf, show.values = TRUE, value.offset = .5, title = "Beta coefficient for p conflict")  + theme_sjplot()
summ(conf) #TABLE 1

#save coefficient
write.csv(as.data.frame(summ(conf)$coef), file="conflict.csv")
conf.data$predict.conf<-predict(conf)

# Calculate estimated marginal 
pconf.emm <- emmeans(conf, lmerTest.limit = 11428, pbkrtest.limit = 11428, specs= pairwise ~ "religion3", level = 0.99)
sig.em.conf<-multcomp::cld(pconf.emm, adjust="none", Letters=LETTERS, alpha = 0.01)
sig.em.conf

#emmeans
sig.em.conf
sig.em.conf$religion3<-factor(sig.em.conf$religion3, levels=rev(c("Atheist", "Agnostic", "Buddhist", "Christian - Catholic", 
                                                                  "Christian - CJCLDS", "Christian - nondenominational","Christian - Protestant",
                                                                  "Hindu", "Jewish", "Muslim", "Other religion")))
ggplot(sig.em.conf, aes(x = religion3,  y = emmean)) +
  geom_point(size = 2, stroke = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean  +SE, linetype = factor(religion3)), width = 0.2, linewidth = 1, linetype = "solid") +
  #geom_line(linetype="dashed", size=1)+ 
  geom_point(size = 2, stroke = 1) +
  labs(x = "Religious identity", y = "Estimated Marginal Means in \n Perceived Conflict") +
  theme_bw() + coord_flip() +
  geom_hline(yintercept = mean(sig.em.conf$emmean), color="black", linetype = "longdash")


################ Perceived Compatibility
mean(df$pcomp)
sd(df$pcomp)

comp.data <-df %>% select(pcomp, religion3, biomajor, institution, course) %>%
  na.omit()
comp.data$religion3 <- relevel (comp.data$religion3, ref =  "Atheist")
comp.data$religion3<-factor(comp.data$religion3, levels=c("Atheist", "Agnostic", "Buddhist", "Christian - Catholic", 
                                                          "Christian - CJCLDS", "Christian - nondenominational","Christian - Protestant",
                                                          "Hindu", "Jewish", "Muslim", "Other religion"))
comp<-lmer(pcomp ~ religion3 + biomajor + (1 | institution) + (1 | course), data = comp.data)
summary(comp)
plot_model(comp, show.values = TRUE, value.offset = .5, title = "Beta coefficient for p compatibility")  + theme_sjplot()
summ(comp) #TABLE 1

write.csv(as.data.frame(summ(comp)$coef), file="compatibility.csv")

comp.data$predict.comp<-predict(comp)

# Calculate estimated marginal 
pcomp.emm <- emmeans(comp, lmerTest.limit = 11428, pbkrtest.limit = 11428, specs= pairwise ~ "religion3", level = 0.99)
sig.em.comp<-multcomp::cld(pcomp.emm, adjust="none", Letters=LETTERS, alpha = 0.01)
sig.em.comp$religion3<-factor(sig.em.comp$religion3, levels=rev(c("Atheist", "Agnostic", "Buddhist", "Christian - Catholic", 
                                                                  "Christian - CJCLDS", "Christian - nondenominational","Christian - Protestant",
                                                                  "Hindu", "Jewish", "Muslim", "Other religion")))
sig.em.comp

ggplot(sig.em.comp, aes(x = religion3,  y = emmean)) +
  geom_point(size = 2, stroke = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean  +SE, linetype = factor(religion3)), width = 0.2, linewidth = 1, linetype = "solid") +
  #geom_line(linetype="dashed", size=1)+ 
  geom_point(size = 2, stroke = 1) +
  labs(x = "Religious identity", y = "Estimated Marginal Means in \n Perceived Compatibility") +
  theme_bw() + coord_flip() +
  geom_hline(yintercept = mean(sig.em.comp$emmean), color="black", linetype = "longdash")

