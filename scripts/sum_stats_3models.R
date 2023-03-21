#packages---- 
library(tidyverse) 
library(stats) 
library(ggplot2) 
library(reshape2) 
library(broom) 
library(mgcv) 
library(descr) 
#read data and get summary stats---- 
#read 2235 trials in bmc as denominator 
study<- read.csv("Data/studies.csv") 
#read 1082 trials with results reporting 
results_got <- read.csv("Data/results_got.csv") %>%  
  mutate(res_yes = 1) 
#read pmid to nctid lookup table 
nct2pmid<- read.csv("Data/nct_id2pmid.csv") 
pmid2many <- nct2pmid %>% 
  nest(nct_ids = c(nct_id))#for each paper, combine the trials 
pmid2many$nct_ids <- map(pmid2many$nct_ids, pull) 
pmid2many$nct_id_n <- map_int(pmid2many$nct_ids, length) 
#read term and condition data 
term<- read.csv("Data/trm_lvl.csv")#include every sub 
condition<- read.csv("Data/trl_cond.csv")  
order_id1 <- condition %>% 
  count(condition_preferred, sort = T) %>% 
  mutate(order_id = 1:77) 
term_trial<- term %>%  
  inner_join(pmid2many) %>% 
  unnest(nct_ids) %>% 
  rename(nct_id = nct_ids) %>% 
  left_join(condition) %>% 
  left_join(order_id1 %>% select(-n)) 
# common<- term_trial %>%
#   distinct(nct_id, mesh_term) 
  
#pick the commonest condition for each trial with sub 
trial_1con <- term_trial %>% 
  select(nct_id, condition_preferred,order_id) %>% 
  group_by(nct_id) %>% 
  arrange(order_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-order_id) %>% 
  rename(condition_commonest = condition_preferred) #49 cons 
#write.csv(trial_1con, "trial_to_1con.csv") 
#pick the commonest condition for 2235 trial  
trial_2235_1con <- condition %>% 
  left_join(order_id1) %>% 
  select(nct_id, condition_preferred,order_id) %>% 
  group_by(nct_id) %>% 
  arrange(order_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-order_id) %>% 
  rename(condition_commonest_2235 = condition_preferred)  
term_trial_1con <- term_trial %>% 
  inner_join(trial_1con) #here is one condition for one trial with their corresponding sub terms 
trial_con_list <- term_trial_1con %>% 
  select(pmid, nct_id, condition_commonest) %>% 
  nest(con = c(pmid, nct_id)) 
trial_con_list$condition <- map(trial_con_list$con, ~.x %>% 
                                  summarise(n_trials = sum(!duplicated(nct_id)), 
                                            n_papers = sum(!duplicated(pmid)))) 
trial_con_list <- trial_con_list %>%  
  unnest(condition)  #so far it keeps the multi to multi number 
#read trial level data with subgroups 
trial_sub <- read.csv("Data/trl_lvl.csv") %>%  
  mutate(sub_yes = 1) 
term_trial_1con<- term_trial_1con %>% 
  left_join(trial_sub) 
denom<- study %>% 
  left_join(results_got) %>% 
  mutate(res_yes = if_else(is.na(res_yes), 0L,1L)) %>% 
  left_join(trial_sub %>% select(nct_id, sub_yes)) %>% 
  mutate(sub_yes = if_else(is.na(sub_yes), 0L,1L)) %>% 
  left_join(trial_1con) %>% 
  left_join(trial_2235_1con) %>% 
  select(nct_id, res_yes, sub_yes, condition_commonest, condition_commonest_2235) %>% 
  mutate(con_withsub = if_else(condition_commonest == condition_commonest_2235, 1L, 0L), 
         con_withsub = if_else(is.na(con_withsub), 0L, 1L), 
         res_withsub = if_else(res_yes == sub_yes & res_yes == 1, 1L,0L)) 

stats_denom <- denom %>% 
  group_by(condition_commonest_2235) %>% 
  count(con_withsub) %>% 
  mutate(num_withsub = if_else(con_withsub == "1", n, 0L), 
         total_n_trial_inbmc2235 = sum(n), 
         percent = round(num_withsub/total_n_trial_inbmc2235,2))  %>% 
  filter(con_withsub == "1") %>% 
  rename(proportion_alltrial = percent) 
stats_denom_res <- denom %>% 
  filter(res_yes ==1) %>% 
  group_by(condition_commonest_2235) %>% 
  count(res_withsub) %>% 
  mutate(num_res_withsub = if_else(res_withsub == "1", n, 0L), 
         n_trial_with_res = sum(n), 
         percent = round(num_res_withsub/n_trial_with_res,2))  %>% 
  filter(res_withsub == "1") %>% 
  select(-n) %>% 
  rename(proportion_res = percent) 
stats_denom<- stats_denom %>% 
  left_join(stats_denom_res) %>% 
  rename(condition_commonest = condition_commonest_2235) 

#the number of subgroups in each condition 
df2 <- term_trial_1con %>% #as here limit one trial with one condition,it's 45. if not limit, it's 51 
  group_by(condition_commonest) %>% 
  summarise(n_sub = n_distinct(mesh_term)) 

df4<- term_trial_1con %>%  
  select(condition_commonest, mesh_term, nct_id) %>% 
  group_by(condition_commonest, mesh_term, nct_id) %>% 
  slice(1) %>% 
  ungroup() %>%  
  group_by(condition_commonest,  mesh_term) %>% 
  mutate(popu_sub = seq_along(mesh_term), 
         freq_sub = max(popu_sub)) %>% 
  distinct(condition_commonest,  mesh_term, freq_sub) %>%  
  group_by(condition_commonest) %>% 
  arrange(condition_commonest, desc(freq_sub)) %>%  
  mutate(top5 = seq_along(mesh_term)) %>%  
  filter(top5 %in% c("1", "2", "3", "4", "5", "6")) %>% #some might have the same freq in the 5th order, just pick one here; here choose top 6 as mean to delete some unclassifiable in the middle 
  group_by(condition_commonest) %>%  
  summarise(top5_sub = paste0(mesh_term," (", freq_sub, ")", collapse = "; ")) 
#var merge 
df_stats<- merge(df2, df4, by = "condition_commonest") %>%  
  merge(df4) %>% 
  left_join(stats_denom) %>% 
  arrange(desc(n_sub)) 
#write.csv(df_stats, "summary_stats.csv")#add some more editing in excel 
#get number of single, double sub in trials
num_sub<- term_trial %>% 
  group_by(nct_id) %>% 
  summarise(n_sub = n_distinct(mesh_term)) 
freq(num_sub$n_sub)
#get common sub
com_sub <- term_trial_1con %>%
  group_by(mesh_term, condition_commonest) %>% 
  summarise(n_sub = n_distinct(nct_id)) %>% 
  mutate(propor = n_sub/524)
com_sub2 <- term_trial_1con %>%
  group_by(mesh_term) %>% 
  summarise(n_sub = n_distinct(nct_id)) %>% 
  mutate(propor = n_sub/524)
  #model prepare---- 
study <- study %>% 
  select(nct_id, start_date, number_of_arms, enrollment, completion_date)#after some checking, here should pick start_date rather than 1st received date 
#select a lead sponsor for each trial 
sponsor <- read.csv("Data/sponsors.csv") 
sponsor<- sponsor %>%  
  group_by(nct_id) %>%  
  arrange(desc(lead_or_collaborator)) %>%  
  slice(1) %>% 
  ungroup() %>% 
  select(nct_id, agency_class) 
study <- study %>% 
  left_join(sponsor) 
res_data2<- study %>% 
  left_join(results_got) %>% 
  left_join(trial_2235_1con) %>% 
  mutate(res_yes = if_else(is.na(res_yes), 0L,1L), 
         number_of_arms = as.character(number_of_arms), 
         year_start = as.numeric(format(as.Date(start_date),'%Y')), 
         year_comp = as.numeric(format(as.Date(completion_date), '%Y')), 
         agency_class = if_else(agency_class %in% c("NIH", "U.S. Fed"), "Government", agency_class), 
         number_of_arms  = as.integer(number_of_arms), 
         n_arms_c = case_when(number_of_arms <=2 ~ "Few", 
                              number_of_arms >2 ~ "Multiple", 
                              TRUE ~ "Few"), 
         industry = as.character(if_else(agency_class == "Industry", 1L, 0L)), 
         fup_length = year_comp - year_start) %>% 
  rename(condition_commonest = condition_commonest_2235) 
# #get comparisons for variables
# compar<- res_data2 %>% 
#   left_join(denom %>% select(nct_id, sub_yes)) %>% 
#   group_by(sub_yes) %>% 
#   median(fup_length, na.rm = TRUE)
# yes_sub <- compar %>% filter(sub_yes == 1)
# no_sub <- compar %>% filter(sub_yes == 0)
#model for the presence of results reporting in 2235 trials---- 
res_yesno<- glm(res_yes ~ year_start + fup_length + n_arms_c + log(enrollment, base = 10) + industry, data = res_data2,  
                family = binomial) 
res_md1<- tidy(res_yesno) 
res_data2 <- res_data2 %>%  
  mutate(condition_commonest_f = factor(condition_commonest)) 
res_data2$condition_commonest_f <- relevel(res_data2$condition_commonest_f, "Asthma") 

res_yesno2 <- update(res_yesno,.~. + condition_commonest_f) 
summary(res_yesno2) 
res_md2<- tidy(res_yesno2, parametric = TRUE, conf.int = TRUE)  
#model for the presence of sub in 1082 trials with results---- 
res_sub<- res_data2 %>%  
  filter(res_yes == "1") %>% 
  left_join(trial_sub %>% select(nct_id, sub_yes)) %>% 
  mutate(sub_yes = if_else(is.na(sub_yes), 0L, 1L)) 


sub_inres<- glm(sub_yes ~ year_start + fup_length + n_arms_c + log(enrollment, base = 10) + industry, data = res_sub,  
                family = binomial) 
summary(sub_inres) 
sub_resmd1<- tidy(sub_inres) 

res_sub <- res_sub %>%  
  mutate(condition_commonest_f = factor(condition_commonest)) 
res_sub$condition_commonest_f <- relevel(res_sub$condition_commonest_f, "Asthma") 

sub_inres2 <- update(sub_inres,.~. + condition_commonest_f) 
summary(sub_inres2) 
sub_resmd2<- tidy(sub_inres2, parametric = TRUE, conf.int = TRUE) 
#plot for enrollment---- 
plotdata<- res_data2 %>%  
  left_join(trial_sub %>% select(nct_id, sub_yes)) %>% 
  mutate(sub_yes = if_else(is.na(sub_yes), 0L, 1L)) %>% 
  filter(sub_yes == "1") %>% 
  left_join(term_trial_1con %>% select(nct_id, mesh_term)) %>% 
  group_by(nct_id) %>% 
  mutate(n_sub_pertrial = sum(!duplicated(mesh_term))) %>% 
  ungroup() 


nosg <- res_data2 %>%  
  anti_join(plotdata, by = "nct_id") %>% 
  mutate(n_sub_pertrial = 0) 

plot_enrol_yr <- ggplot(plotdata, aes(x = enrollment, y = n_sub_pertrial)) +  
  geom_point(position = "jitter", alpha = 0.1) +  
  ylab("Number of subgroups") + xlab("Enrolment size (in 10-fold log scale)") +  
  scale_x_log10() + geom_rug(data = nosg, mapping = aes(x = enrollment)) + 
  theme(text = element_text(size=30, color = "black"))+ theme_bw() 
plot_enrol_yr 

#number of sub reported---- 
mod1 <- glm(n_sub_pertrial ~ year_start + n_arms_c + log(enrollment, base = 10) +  
              industry + fup_length, data = plotdata,  
            family = "quasipoisson") 
mod1_chk <- gam(n_sub_pertrial ~ year_start + n_arms_c + s(log(enrollment)) +  
                  industry + fup_length, data = plotdata,  
                family = "quasipoisson") 
summary(mod1) 
unad_con<- tidy(mod1, parametric = TRUE, conf.int = TRUE) 
plot(mod1_chk) 

mod2 <- update(mod1, . ~ . -log(enrollment)) # not adjust for enro and conditions 
summary(mod2) 
plotdata <- plotdata %>%  
  mutate(condition_commonest_f = factor(condition_commonest)) 
plotdata$condition_commonest_f <- relevel(plotdata$condition_commonest_f, "Asthma") 
mod3 <- update(mod1, . ~ . + condition_commonest_f) # a full model 

summary(mod3)  
unad <- update(mod1, . ~ condition_commonest_f) 
summary(unad) 
adj <- tidy(mod3, parametric = TRUE, conf.int = TRUE) 
#3 models together---- 
sub_num <- bind_rows(Subgroup_reporting = sub_resmd2, Number_of_subgroups = adj, Results_reporting = res_md2,  .id = "model") 
sub_num <- sub_num %>%  
  filter(!is.na(conf.low) & !is.na(conf.high)) %>% #temperately keep this 
  rename(lci = conf.low, 
         uci = conf.high) %>% 
  mutate(term = str_replace(term, "condition_commonest_f", ""))  
sub_num$term[sub_num$term == "fup_length"] <-"Duration of follow up" 
sub_num$term[sub_num$term == "n_arms_cMultiple"] <- "Number of arms > 2" 
sub_num$term[sub_num$term == "industry1"] <- "Industry1" 
sub_num$term[sub_num$term == "year_start"] <- "Start year" 
sub_num$term <- fct_inorder(sub_num$term) 
plot_2mod <- ggplot(sub_num %>% filter(!term == "(Intercept)"), aes(x = term, y = estimate, ymin = lci, ymax = uci)) + 
  geom_point() + 
  geom_linerange() + 
  facet_wrap(~ model) + 
  coord_flip() + 
  geom_hline(yintercept = 0) 
plot_2mod 

summary_val<- sub_num %>%  
  mutate(or = round(exp(estimate), 2), 
         lowci = round(exp(lci), 2), 
         highci = round(exp(uci), 2), 
         orci = paste0(or, " (", lowci,", ", highci, ")"), 
         sig = if_else(p.value < 0.05, "sig", "none"))  
#write.csv(summary_val, "three_model_results.csv") # updated on 240223 after modify a bit diabetes
