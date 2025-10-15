#Note revision Jun 3
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
library(simr)
library(sjPlot)
library(TAM)
library(tidyr)

library(tidyverse)
########################### All population ###############################
master_data_7_theta_values <- read_sav("~/Library/CloudStorage/OneDrive-MiddleTennesseeStateUniversity/ACTIVE RESEARCH PROJECTS/master data 7 theta values.sav")
data.all<-master_data_7_theta_values
table(data.all$course) #11995 in 72 course
#remove courses containing under 20 students
course_counts <- table(data.all$course) # Get the counts of each course
filtered_courses <- names(course_counts[course_counts >= 20]) # Filter out the courses with less than 20 students, now only 63 courses
dat<- data.all[data.all$course %in% filtered_courses, ] # Subset the original data to only include the filtered courses #1887

library(readxl)
X25_04_01_Non_religious_explanation_coding <- read_excel("25 04 01 Non religious explanation coding.xlsx")
View(X25_04_01_Non_religious_explanation_coding)

########################### Naming Variables############################### 
#cc1: Scientists and religious people cannot see eye to eye about evolution. 1 strongly AGREE, 5 s DISAGREE
#cc2: Religion and evolution can be compatible.. 1 strongly disagree, 5 s agree
#since cc1, cc3, cc5 coded reversely, we wanted to to make 5 is strongly agree 
# Reverse the coding using recode

dat <- dat %>%
  mutate(cc1fixed = recode(cc1, `1` = 5, `2` = 4, `4` = 2, `5` = 1),
         cc3fixed = recode(cc3, `1` = 5, `2` = 4, `4` = 2, `5` = 1),
         cc5fixed = recode(cc5, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) 

df <- dat %>%
  dplyr::select(cc1fixed, cc2, cc3fixed, cc4, cc5fixed, cc6, 
                  ResponseId, religion, christianden, nonrel, course, biomajor, institution, gender, semester, raceoriginal) %>%
  rename(pconf1 = cc3fixed,
         pconf2 = cc5fixed,
         pconf3 = cc1fixed,
         pcomp1 = cc4,
         pcomp2 = cc6,
         pcomp3 = cc2,) %>% 
  drop_na(religion, pconf1, pconf2, pconf3, pcomp1, pcomp2, pcomp3, biomajor) %>% 
  mutate(
    pconf = (pconf1 + pconf2 + pconf3) / 3,
    pcomp = (pcomp1 + pcomp2 + pcomp3) / 3)  #now 11,855


df <- merge(
  df,
  X25_04_01_Non_religious_explanation_coding[, c("ResponseId", "Nonrel.Other non religious")],
  by = "ResponseId",
  all.x = TRUE
)
names(df)[names(df) == "Nonrel.Other non religious"] <- "Nonrel.Other"



#Rename variable
df$religion <- as.factor(df$religion) # 8 decline to state
df$religion7 <- recode(df$religion, '1' = "Buddhist", '2'= "Christian", '3'= "Hindu", '4'="Jewish", 
                       '5'="Muslim", '6'="Non-religious", '7'="Other religion", '8'="NA")
as.data.frame(table(df$religion7))

df$christianden <- as.factor(df$christianden) #8 was decline to state , become NA
df$christianden2 <- recode(df$christianden, '1' = "Catholic", '2'= "Jehovas witness", 
                             '3'= "Orthodox", '4'="Nondenominational", '5'="Protestant", '6'="CJCLDS", '7'="Other", '8'="NA")
as.data.frame(table(df$christianden2))

table(df$nonrel) #'0' = "Agnostic", '1'= "Ateheist", '2'= "Other", '3'="NA")
table(df$Nonrel.Other) #actual Other from nonrel, 

df <- df %>%
  mutate(
    nonrel2 = case_when(
      nonrel == "0" ~ "Agnostic",
      nonrel == "1" ~ "Atheist",
      Nonrel.Other == "Other non religious" ~ "Other",
      TRUE ~ NA_character_
    )
  )

# Check the new counts
table(df$nonrel2, useNA = "ifany")
df$nonrel2<-as.factor(df$nonrel2)
as.data.frame(table(df$nonrel2))
df$religion3 <- paste(df$religion7, df$nonrel2, df$christianden2)
as.data.frame(table(df$religion3))
df$religion3<-recode(df$religion3, 'Buddhist NA NA'="Buddhist", 
                     'Christian NA Catholic' = "Christian - Catholic",
                     'Christian NA CJCLDS' = "Christian - CJCLDS",
                     'Christian NA Jehovas witness' = "Other Religion",
                     'Christian NA NA' = "NA",
                     'Christian NA Orthodox' = "Other Religion",
                     'Christian NA Nondenominational' = "Christian - Nondenominational",
                     'Christian NA Protestant' = "Christian - Protestant",
                     'Christian NA Other' = "Other Religion",
                     'Muslim NA NA'="Muslim",
                      'Hindu NA NA'="Hindu", 
                     'Jewish NA NA'="Jewish",
                     'Non-religious Agnostic NA'="Agnostic", 
                      'Non-religious Atheist NA'="Atheist", 
                      'Non-religious NA NA'= "NA",
                     'NA NA NA'= "NA",
                     'Non-religious Other NA'="Other Religion",
                     'Other religion NA NA'="Other Religion")

as.data.frame(table(df$religion3))

df$religion3 <- as.factor(df$religion3)
df$religion3 <- relevel (df$religion3, ref =  "Atheist")

df$course <-as.factor(df$course)
df$institution <-as.factor(df$institution)

df$biomajor<-to_character(df$biomajor)
df$biomajor <-as.factor(df$biomajor)

df$gender<-to_character(df$gender)
df$gender <-as.factor(df$gender)

df$raceoriginal <-as.factor(df$raceoriginal)
# '1' = "Native", '2'= "Asian",  '3'= "Black", '4'="Latinx",  '5'="Islander", '6'="Multiracial",  '7'="White", '8'="Other", '8'="NA")
#combine Black, latin, oslander. 
df$race4 <- recode(df$raceoriginal, '1' = "PEER", '2'= "Asian", 
                           '3'= "PEER", '4'="PEER", 
                          '5'="PEER", '6'="Multiracial", 
                          '7'="White", '8'="Other", '8'="NA")
df$race4 <-as.factor(df$race4)

df$semester<-to_character(df$semester)
df$semester <-factor(df$semester, levels=c("Fall 2018", "Spring 2020", "Fall 2020", "Spring 2021"))

# Replace "NA" with actual NA in df$religion3
df$religion3[df$religion3 == "NA"] <- NA

df1 <- df %>%
  filter(
    !religion3 %in% c("Muslim", "Hindu", "Buddhist", "Jewish", "Other Religion")
  ) #now 9859

table(df1$religion3)

df1 <- df1 %>%
  mutate(
    # 1) Create re.identity as a 2‐level factor
    re.identity = case_when(
      str_detect(religion3, "^Christian -") ~ "Christian",
      religion3 %in% c("Atheist", "Agnostic")     ~ "Non-religious",
      TRUE                                        ~ NA_character_
    ) %>% 
      factor(levels = c("Christian", "Non-religious")),
    
    # 2) Rename religion3 values in place
    religion3 = case_when(
      str_detect(religion3, "^Christian - Catholic")            ~ "C: Catholic",
      str_detect(religion3, "^Christian - CJCLDS")             ~ "C: CJCLDS",
      str_detect(religion3, "^Christian - Nondenominational")  ~ "C: Nondenominational",
      str_detect(religion3, "^Christian - Protestant")         ~ "C: Protestant",
      religion3 == "Atheist"                                   ~ "NR: Atheist",
      religion3 == "Agnostic"                                  ~ "NR: Agnostic",
      TRUE                                                      ~ religion3
    ) %>% 
      factor()  # optional, to keep it as a factor
  )

df1 <- df1 %>%
  filter(!is.na(religion3)) #now 9337

#see distributuon
library(naniar)

df2 <- df1 %>%
  dplyr::select(pconf1, pconf2, pconf3, pcomp1, pcomp2, pcomp3, biomajor, semester, course, gender, religion3, race4, institution) %>%
  dplyr::mutate(across(everything(), as.factor))

skimr::skim(df2)  
mcar_test(df2) 

df2 <- df2 %>% #previously 9337 now 9116
  filter(!is.na(gender) & !is.na(race4))

skimr::skim(df2)  
mcar_test(df2) 



#demographic information #TABLE 2
dem.acc <- df2 %>% dplyr::select(semester, gender, race4, biomajor, religion3)
dem.acc %>% tbl_summary(label = list(semester ~ "Semester",gender ~ "Gender",
                                     race4 ~ "Race/ethnicity", biomajor ~ "Bio Major", religion3 ~ "Religion"))
library(psych)
library(corrplot)


# Compute polychoric correlations and visualize
df2 %>%
  dplyr::select(pconf1, pconf2, pconf3, pcomp1, pcomp2, pcomp3) %>%
  polychoric() 


########################### Validity Evidence ###############################

df2 %>% dplyr::select(pcomp1, pcomp2, pcomp3) %>%
  dplyr::mutate(across(everything(), as.numeric)) %>%
  cronbach.alpha(na.rm=TRUE) #0.742


df2 %>% dplyr::select(pconf1, pconf2, pconf3) %>%
  dplyr::mutate(across(everything(), as.factor)) %>%
  cronbach.alpha(na.rm=TRUE) #0.656


########################### Validity Evidence ###############################
library(psych)

com<-df2 %>% dplyr::select(pconf1, pconf2, pconf3, pcomp1, pcomp2, pcomp3) %>%
  dplyr::mutate(across(everything(), as.numeric))
scree(com) #Scree plot
fa.parallel(com, fa = "fa", cor = "poly") #Paralelel analyis


ks.test(com$pcomp1, "pnorm", mean = mean(com$pcomp1), sd = sd(com$pcomp1))
ks.test(com$pcomp2, "pnorm", mean = mean(com$pcomp2), sd = sd(com$pcomp2))
ks.test(com$pcomp3, "pnorm", mean = mean(com$pcomp3), sd = sd(com$pcomp3))
ks.test(com$pconf1, "pnorm", mean = mean(com$pconf1), sd = sd(com$pconf1))
ks.test(com$pconf2, "pnorm", mean = mean(com$pconf2), sd = sd(com$pconf2))

# all variable is not normal, so use DWLS
# Compute the polychoric correlation matrix
poly_matrix <- polychoric(com)$rho
# Perform EFA with 2 factors (use fa() for EFA in psych)
efa_psych1 <- fa(poly_matrix, nfactors = 2, fm = "dwls", rotate = "oblimin")
# Check the results
print(efa_psych1)

# df null model =  15  with the objective function =  2.17
# The root mean square of the residuals (RMSR) is  0.02 
# The df corrected root mean square of the residuals is  0.04
#test 


#CFA semua 
com<-df2 %>% dplyr::select(pconf1, pconf2, pconf3, pcomp1, pcomp2, pcomp3) %>%
  dplyr::mutate(across(everything(), as.factor))

two.correl <- ' 
  p1 =~ pcomp1 + pcomp2 + pcomp3
  p2 =~ pconf1 + pconf2 + pconf3
  p1 ~~ p2
'
two.fit <- cfa(two.correl, data=com, std.lv=TRUE, estimator = "DWLS", ordered = TRUE)
summary(two.fit, standardized=TRUE, fit.measures=TRUE)
fitMeasures(two.fit, c("df", "chisq", "cfi", "tli", "rmsea", "rmsea.ci.lower", " rmsea.ci.upper" ,   "srmr"))


lavaanPlot(two.fit,  covs = TRUE, coefs = TRUE,  stars = c("latent", "covs"))
###
df3 <- df2 %>%
  dplyr::select(pconf1, pconf2, pconf3, pcomp1, pcomp2, pcomp3, religion3) %>%
  drop_na(religion3, pconf1, pconf2, pconf3, pcomp1, pcomp2, pcomp3) %>%
  dplyr::mutate(across(everything(), as.factor))


# Fit the configural model (no constraints across groups)
fit_configural <- cfa(two.correl, data = df3, group = "religion3", estimator = "DWLS", ordered = TRUE, group.equal = NULL)

# Fit the metric invariance model (constrain factor loadings across groups)
fit_metric <- cfa(two.correl, data = df3, group = "religion3", estimator = "DWLS", ordered = TRUE, group.equal = "loadings")

# Fit the scalar invariance model (constrain factor loadings and intercepts across groups)
fit_scalar <- cfa(two.correl, data = df3, group = "religion3", estimator = "DWLS", ordered = TRUE, group.equal = c("loadings", "intercepts"))

# Compare the models (check if adding constraints worsens fit)
anova(fit_configural, fit_metric, fit_scalar)
library(semTools)
comp<-compareFit(fit_configural, fit_metric, fit_scalar)
comp@fit$cfi

########################### Design Effect ###############################
library(Hmisc)

df2 <- df2 %>%
  mutate(
    across(
      c(pconf1, pconf2, pconf3, pcomp1, pcomp2, pcomp3),
      as.numeric
    ),
    pconf = (pconf1 + pconf2 + pconf3) / 3,
    pcomp = (pcomp1 + pcomp2 + pcomp3) / 3
  )

round(deff(df2$pconf, df2$course), 2)
round(deff(df2$pcomp, df2$course), 2)
round(deff(df2$pconf, df2$institution), 2)
round(deff(df2$pcomp, df2$institution), 2)



library(lme4)
df2$race4 <- relevel (df2$race4, ref =  "White")

df2$religion3 <- factor(df2$religion3)
# 1. Drop the “ordered” class if it exists:
df2$rel.detail <- factor(as.character(df2$rel.detail),
                         levels = levels(df2$rel.detail))

# 2. Now relevel with “NR: Atheist” as reference:
df2$rel.detail <- relevel(df2$rel.detail, ref = "NR: Atheist")


library(lme4)

# Model 1: religion only
mod1 <- lmer(pconf ~ re.identity + (1 | institution) + (1 | course), data = df2)
# Model 2: religion + race
mod2 <- lmer(pconf ~ re.identity + race4 + (1 | institution) + (1 | course), data = df2)
# Model 3: religion + gender
mod3 <- lmer(pconf ~ re.identity + gender + (1 | institution) + (1 | course), data = df2)
# Model 4: religion + biomajor
mod4 <- lmer(pconf ~ re.identity + biomajor + (1 | institution) + (1 | course), data = df2)
# Model 5: religion + race + gender
mod5 <- lmer(pconf ~ re.identity + race4 + gender + (1 | institution) + (1 | course), data = df2)
# Model 6: religion + race + biomajor
mod6 <- lmer(pconf ~ re.identity + race4 + biomajor + (1 | institution) + (1 | course), data = df2)
# Model 7: religion + gender + biomajor
mod7 <- lmer(pconf ~ re.identity + gender + biomajor + (1 | institution) + (1 | course), data = df2)
# Model 8: religion + race + gender + biomajor
mod8 <- lmer(pconf ~ re.identity + race4 + gender + biomajor + (1 | institution) + (1 | course), data = df2)

AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)


# Model 1: religion only
mod1 <- lmer(pcomp ~ re.identity + (1 | institution) + (1 | course), data = df2)
# Model 2: religion + race
mod2 <- lmer(pcomp ~ re.identity + race4 + (1 | institution) + (1 | course), data = df2)
# Model 3: religion + gender
mod3 <- lmer(pcomp ~ re.identity + gender + (1 | institution) + (1 | course), data = df2)
# Model 4: religion + biomajor
mod4 <- lmer(pcomp ~ re.identity + biomajor + (1 | institution) + (1 | course), data = df2)
# Model 5: religion + race + gender
mod5 <- lmer(pcomp ~ re.identity + race4 + gender + (1 | institution) + (1 | course), data = df2)
# Model 6: religion + race + biomajor
mod6 <- lmer(pcomp ~ re.identity + race4 + biomajor + (1 | institution) + (1 | course), data = df2)
# Model 7: religion + gender + biomajor
mod7 <- lmer(pcomp ~ re.identity + gender + biomajor + (1 | institution) + (1 | course), data = df2)
# Model 8: religion + race + gender + biomajor
mod8 <- lmer(pcomp ~ re.identity + race4 + gender + biomajor + (1 | institution) + (1 | course), data = df2)

AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)

#
df2 <- df2 %>% mutate(
    re.identity = case_when(
      religion3 %in% c("C: Protestant", "C: Catholic", "C: CJCLDS", "C: Nondenominational","C: Protestant")     ~ "Christian",
      religion3 %in% c("NR: Agnostic", "NR: Atheist")     ~ "Non-religious") %>% 
      factor(levels = c("Christian", "Non-religious")))

# Fit the model
mod.conf.a <- lmer(pconf ~ re.identity + race4 + gender +
                     (1 | institution) + (1 | course),
                   data = df2, REML = FALSE)
summ(mod.conf.a)
# Calculate estimated marginal means for re.identity
pconf.emm.a <- emmeans(mod.conf.a,
                       lmerTest.limit = 9964,
                       pbkrtest.limit = 9964,
                       specs = "re.identity",
                       level = 0.99)

# Obtain group letters (for significance) via cld
sig.em.conf.a <- multcomp::cld(pconf.emm.a,
                               adjust = "fdr",
                               Letters = LETTERS,
                               alpha = 0.01)

# Convert emmeans object to a data frame for plotting
conf.emm_df.a <- as.data.frame(sig.em.conf.a)

# Plot estimated marginal means with error bars
q1<-ggplot(conf.emm_df.a, aes(x = re.identity, y = emmean)) +
  geom_point(size = 2, stroke = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                width = 0.2, linewidth = 1) +
  labs(x = "Religious identity", y = "Estimated Marginal Means in \n Perceived Conflict") +
  theme_bw() +
  coord_flip() +
  geom_hline(yintercept = mean(conf.emm_df.a$emmean),
             color = "black", linetype = "longdash")


## for compatibility
# Fit the model
mod.comp.a <- lmer(pcomp ~ re.identity + race4 + gender + 
                     (1 | institution) + (1 | course),
                   data = df2, REML = FALSE)
summ(mod.comp.a)
# Calculate estimated marginal means for re.identity
pcomp.emm.a <- emmeans(mod.comp.a,
                       lmerTest.limit = 9964,
                       pbkrtest.limit = 9964,
                       specs = "re.identity",
                       level = 0.99)

# Obtain group letters (for significance) via cld
sig.em.comp.a <- multcomp::cld(pcomp.emm.a,
                               adjust = "fdr",
                               Letters = LETTERS,
                               alpha = 0.01)

# Convert emmeans object to a data frame for plotting
comp.emm_df.a <- as.data.frame(pcomp.emm.a)

# Plot estimated marginal means with error bars
q2<-ggplot(comp.emm_df.a, aes(x = re.identity, y = emmean)) +
  geom_point(size = 2, stroke = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                width = 0.2, linewidth = 1) +
  labs(x = "Religious identity", y = "Estimated Marginal Means in \n Perceived Compatibility") +
  theme_bw() +
  coord_flip() +
  geom_hline(yintercept = mean(comp.emm_df.a$emmean),
             color = "black", linetype = "longdash")



#PANEL B
## for conflict
# Fit the model
df2$rel.detail
mod.conf.c <- lmer(pconf ~ rel.detail + race4 + gender + 
                     (1 | institution) + (1 | course),
                   data = df2, REML= F)
summ(mod.conf.c)
# Calculate estimated marginal means for =
pconf.emm.c <- emmeans(mod.conf.c,
                       lmerTest.limit = 9964,
                       pbkrtest.limit = 9964,
                       specs = "rel.detail",
                       level = 0.99)

# Obtain group letters (for significance) via cld
sig.em.conf.c <- multcomp::cld(pconf.emm.c,
                               adjust = "fdr",
                               Letters = LETTERS,
                               alpha = 0.01)

# Convert emmeans object to a data frame for plotting
conf.emm_df.c <- as.data.frame(sig.em.conf.c)

# Plot estimated marginal means with error bars
q3<-ggplot(conf.emm_df.c, aes(x = rel.detail, y = emmean)) +
  geom_point(size = 2, stroke = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                width = 0.2, linewidth = 1) +
  labs(x = "Religious affiliations", y = "Estimated Marginal Means in \n Perceived Conflict") +
  theme_bw() +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(conf.emm_df.c$rel.detail))) +
  geom_hline(yintercept = mean(conf.emm_df.c$emmean),
             color = "black", linetype = "longdash")




## for compatibility
# Fit the model
mod.comp.c <- lmer(pcomp ~ rel.detail + race4 + gender +
                     (1 | institution) + (1 | course),
                   data = df2, REML= F)
summ(mod.comp.c)
# Calculate estimated marginal means for religion7
pcomp.emm.c <- emmeans(mod.comp.c,
                       lmerTest.limit = 9964,
                       pbkrtest.limit = 9964,
                       specs = "rel.detail",
                       level = 0.99)

# Obtain group letters (for significance) via cld
sig.em.comp.c <- multcomp::cld(pcomp.emm.c,
                               adjust = "fdr",
                               Letters = LETTERS,
                               alpha = 0.01)

# Convert emmeans object to a data frame for plotting
comp.emm_df.c <- as.data.frame(sig.em.comp.c)



q4<-ggplot(comp.emm_df.c, aes(x = rel.detail, y = emmean)) +
  geom_point(size = 2, stroke = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                width = 0.2, linewidth = 1) +
  labs(x = "Religious affiliations", y = "Estimated Marginal Means in \n Perceived Compatibility") +
  theme_bw() +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(comp.emm_df.c$rel.detail))) +
  geom_hline(yintercept = mean(comp.emm_df.c$emmean),
             color = "black", linetype = "longdash")

fig1 <- (q1|q3)
fig2 <- (q2|q4)



# 2) Save as SVG via ggsave()
ggsave(
  filename = "fig1.svg", 
  plot     = fig1, 
  device   = "svg", 
  width    = 10, 
  height   = 4, 
  units    = "in"
)

ggsave(
  filename = "fig2.svg", 
  plot     = fig2, 
  device   = "svg", 
  width    = 10, 
  height   = 4, 
  units    = "in"
)


psych::describe(
  df2[, c("pconf1", "pconf2", "pconf3", "pcomp1", "pcomp2", "pcomp3")]
)

psych::describe(
  df2[, c("pconf", "pcomp")]
)
