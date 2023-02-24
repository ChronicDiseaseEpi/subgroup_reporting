library(readxl) 
library(tidyverse)
#read study data 
study<- read.csv("Data/studies.csv") 
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
#pick the commonest condition for each trial with sub 
trial_1con <- term_trial %>% 
  select(nct_id, condition_preferred,order_id) %>% 
  group_by(nct_id) %>% 
  arrange(order_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-order_id)  
#write.csv(trial_1con, "trial_to_1con.csv") 
#pick the commonest condition for 2235 trial  
trial_2235_1con <- condition %>% 
  left_join(order_id1) %>% 
  #  select(nct_id, condition_preferred,order_id) %>% 
  group_by(nct_id) %>% 
  arrange(order_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-order_id, -n)  
cond_smry <- trial_2235_1con %>%  
  mutate(tree3cond = str_sub(mesh_broad, 1, 3)) %>%  
  distinct(tree3cond, nct_id, condition_preferred) 
#read mesh tree data 
msh_tree_label<- read.csv("Data/msh_tree2lbl.csv") 
msh_tree<- read.csv("Data/msh_tree.csv") 
msh_1tree<- msh_tree %>%  
  group_by(mesh_code) %>%
  slice(1) %>% #need to check with David, keep one tree num for each mesh code
  ungroup() %>%
  left_join(msh_tree_label %>% rename(mesh_tree = trees)) 

htmp_smry <- term_trial %>%  
  select(nct_id, mesh_term, mesh_code) %>% 
  left_join(msh_1tree) 

# htmp_smry2 <- htmp_smry %>%  
#   filter(tree_lbl == "C: Diseases") %>%  
#   mutate(tree3sub = str_sub(mesh_tree, 1, 3)) %>%  
#   distinct(tree3sub, nct_id) 

htmap_clabel<- htmp_smry %>%  
  filter(tree_lbl == "C: Diseases") %>%  
  mutate(tree3sub = str_sub(mesh_tree, 1, 3)) %>%  
  distinct(tree3sub, nct_id, mesh_term) 

tree_name <- read_excel("Data/disease_tree_name.xlsx") #here I added this excel

htmap_clabel_nosame <- htmap_clabel %>%  
  anti_join(cond_smry %>%  
              rename(mesh_term = condition_preferred) %>%  
              distinct(nct_id, mesh_term)) 


tot_smry <- cond_smry %>%  
  left_join(htmap_clabel_nosame) %>%  
  as_tibble() %>%  
  arrange(nct_id) %>% 
  left_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease)) %>% 
  left_join(tree_name %>% rename(tree3sub = tree_num, subgroups = disease)) 

tot_smry <- tot_smry %>%  
  group_by(tree3cond) %>%  
  mutate(tot = sum(!duplicated(nct_id))) %>%  
  ungroup() %>%  
  group_by(tree3cond, tree3sub) %>%  
  mutate(n = sum(!duplicated(nct_id))) %>%  
  ungroup() %>%  
  distinct(tree3cond, tree3sub, n, tot, conditions, subgroups) 
tot_smry <- tot_smry %>%  
  mutate(m = n/tot) %>% 
  mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")"), 
         tree_subgroup = paste0(tree3sub, " - ", subgroups)) 
blanks <- expand.grid(tree3cond = c( "C15",  "C07", "C11"), 
                      tree3sub = c("C01", "C04", "C05", "C06", "C07", "C08", "C10", "C11", "C12",  
                                   "C14", "C15", "C17", "C18", "C20", "C26")) 
blanks <- blanks %>% #to show the diagonal patterns 
  inner_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease)) %>%  
  inner_join(tree_name %>% rename(tree3sub = tree_num, subgroups   = disease)) %>% 
  mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=0)"), 
         tree_subgroup = paste0(tree3sub, " - ", subgroups)) 

tot_smry2 <- bind_rows(blanks = blanks, res = tot_smry, .id = "df")    
tot_smry2 <- tot_smry2 %>%  
  mutate(m = 100*m, 
         mlbl = if_else(m<10, format(round(m,1), nsmall = 1), as.character(round(m)))) #keep 2 numbers cor each 
# tot_smry <- tot_smry %>%  
#   group_by(tree3cond) %>%  
#   mutate(m2 = m/max(m)) %>%  
#   ungroup() 
plot1 <- ggplot(tot_smry2 %>%  
                  filter(df == "res", !is.na(tree3sub), ! tree3sub == "C23", !tree3cond == "C23"),  
                aes(x = tree_subgroup, y = tree_condition, fill = m, label = mlbl)) + 
  xlab("Subgroups") + 
  ylab("Conditions") + 
  geom_tile(colour = NA)+ 
  geom_tile(colour = NA, fill = NA, data = tot_smry2 %>% filter(df == "blanks")) + 
  geom_text(colour = "white") + 
  theme_bw() +  
  theme(text = element_text(size=10, color = "black"),  
        axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_fill_viridis_c("%", guide = FALSE) 
plot1  

#pdf("temp.pdf") 
# plot1 
# plot2 
#dev.off() 

# #not run below yet, prob not necessary 
# ##diag---- 
# htmp_lab <- htmap %>%  
#   inner_join(allcodes) %>%  
#   select(mesh_type, nm, tree, mesh_code, nct_id, mesh_term) %>% 
#   distinct() 
# htmap_diag<- htmp_lab %>%  
#   filter(mesh_type == "dnstc") 
#  
# tot_diag <- cond_smry %>%  
#   left_join(htmap_diag) %>% 
#   as_tibble() %>%  
#   left_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease)) %>%  
#   mutate(tree3lab = str_sub(tree, 1,3)) 
#  
#  
# tot_diag <- tot_diag %>%  
#   group_by(tree3cond) %>%  
#   mutate(tot = sum(!duplicated(nct_id))) %>%  
#   ungroup() %>%  
#   group_by(tree3cond, nm) %>%  
#   mutate(n = sum(!duplicated(nct_id))) %>%  
#   ungroup() %>%  
#   distinct(tree3cond, nm, n, tot, conditions, tree3lab) 
# tot_diag <- tot_diag %>%  
#   mutate(m = n/tot) %>% 
#   mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")"), 
#          tree_subgroup = paste0(tree3lab, " - ", nm)) 
#  
# blanks_diag <- data.frame(tree3cond = c( "C10","C11", "C12", "C17", "C19", "C20", "C23"), 
#                       tot = c("161", "10", "40", "65", "10", "12", "43")) %>%  
#   mutate(tot = as.integer(tot)) 
# blanks_diag <- blanks_diag %>% #to show the diagonal patterns 
#   inner_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease))  
#  
# tot_diag2<- bind_rows(blanks_diag = blanks_diag, res = tot_diag, .id = "df") %>%  
#   mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")")) 
#  
#  
# tot_diag2 <- tot_diag2 %>%  
#   mutate(m = 100*m, 
#          mlbl = if_else(m<10, format(round(m,1), nsmall = 1), as.character(round(m))))  
# plot_diag <- ggplot(tot_diag2 %>%  filter(df == "res",!is.na(tree3lab)),  
#                 aes(x = tree_subgroup, y = tree_condition, fill = m, label = mlbl)) + 
#   xlab("Subgroups of Diagnosis [E01]") + 
#   ylab("Conditions") + 
#   geom_tile(colour = NA)+ 
#   geom_tile(colour = NA, fill = NA, data = tot_diag2 %>% filter(df == "blanks_diag", !tree3cond == "C13" )) + 
#   geom_text(colour = "white") + 
#   theme_bw() +  
#   theme(text = element_text(size=10, color = "black"),  
#         axis.text.x = element_text(angle=45, hjust=1)) + 
#   scale_fill_viridis_c("%", guide = FALSE) 
# plot_diag  
#  
# #therapy---- 
# htmap_therap<- htmp_lab %>%  
#   filter(mesh_type == "therap") 
#  
# tot_therap <- cond_smry %>%  
#   left_join(htmap_therap) %>% 
#   as_tibble() %>%  
#   left_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease)) %>%  
#   mutate(tree3lab = str_sub(tree, 1,3)) 
#  
#  
# tot_therap <- tot_therap %>%  
#   group_by(tree3cond) %>%  
#   mutate(tot = sum(!duplicated(nct_id))) %>%  
#   ungroup() %>%  
#   group_by(tree3cond, nm) %>%  
#   mutate(n = sum(!duplicated(nct_id))) %>%  
#   ungroup() %>%  
#   distinct(tree3cond, nm, n, tot, conditions, tree3lab) 
# tot_therap <- tot_therap %>%  
#   mutate(m = n/tot) %>% 
#   mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")"), 
#          tree_subgroup = paste0(tree3lab, " - ", nm)) 
# blanks_therap <- data.frame(tree3cond = c( "C11", "C19",  "C23"), 
#                           tot = c( "10",  "10", "43")) %>%  
#   mutate(tot = as.integer(tot)) 
# blanks_therap <- blanks_therap %>% #to show the diagonal patterns 
#   inner_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease))  
# tot_therap2<- bind_rows(blanks_therap = blanks_therap, res = tot_therap, .id = "df") %>%  
#   mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")")) 
#  
# tot_therap2 <- tot_therap2 %>%  
#   mutate(m = 100*m, 
#          mlbl = if_else(m<10, format(round(m,1), nsmall = 1), as.character(round(m)))) 
# plot_therap <- ggplot(tot_therap2 %>%  filter(!is.na(tree3lab) & !tree3cond == "C13"),  
#                     aes(x = tree_subgroup, y = tree_condition, fill = m, label = mlbl)) + 
#   xlab("Subgroups of Therapeutic Uses [D27.505.954]") + 
#   ylab("Conditions") + 
#   geom_tile(colour = NA)+ 
#   geom_tile(colour = NA, fill = NA, data = tot_therap2 %>% filter(df == "blanks_therap", !tree3cond == "C13") ) + 
#   geom_text(colour = "white") + 
#   theme_bw() +  
#   theme(text = element_text(size=10, color = "black"),  
#         axis.text.x = element_text(angle=45, hjust=1)) + 
#   scale_fill_viridis_c("%", guide = FALSE) 
# plot_therap  
#  
# #physiodrug---- 
# htmap_physiodrug<- htmp_lab %>%  
#   filter(mesh_type == "physiodrug") 
#  
# tot_physiodrug <- cond_smry %>%  
#   left_join(htmap_physiodrug) %>% 
#   as_tibble() %>%  
#   left_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease)) %>%  
#   mutate(tree3lab = str_sub(tree, 1,3)) 
#  
#  
# tot_physiodrug <- tot_physiodrug %>%  
#   group_by(tree3cond) %>%  
#   mutate(tot = sum(!duplicated(nct_id))) %>%  
#   ungroup() %>%  
#   group_by(tree3cond, nm) %>%  
#   mutate(n = sum(!duplicated(nct_id))) %>%  
#   ungroup() %>%  
#   distinct(tree3cond, nm, n, tot, conditions, tree3lab) 
# tot_physiodrug <- tot_physiodrug %>%  
#   mutate(m = n/tot) %>% 
#   mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")"), 
#          tree_subgroup = paste0(tree3lab, " - ", nm)) 
# blanks_physiodrug <- data.frame(tree3cond = c( "C11", "C19",  "C23"), 
#                             tot = c( "10",  "10", "43")) %>%  
#   mutate(tot = as.integer(tot)) 
# blanks_physiodrug <- blanks_physiodrug %>% #to show the diagonal patterns 
#   inner_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease))  
# tot_physiodrug2<- bind_rows(blanks_physiodrug = blanks_physiodrug, res = tot_physiodrug, .id = "df") %>%  
#   mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")")) 
#  
# tot_physiodrug2 <- tot_physiodrug2 %>%  
#   mutate(m = 100*m, 
#          mlbl = if_else(m<10, format(round(m,1), nsmall = 1), as.character(round(m)))) 
# plot_physiodrug <- ggplot(tot_physiodrug2 %>%  filter(!is.na(tree3lab) & !tree3cond == "C13"),  
#                       aes(x = tree_subgroup, y = tree_condition, fill = m, label = mlbl)) + 
#   xlab("Subgroups of Physiological Effects of Drugs [D27.505.696]") + 
#   ylab("Conditions") + 
#   geom_tile(colour = NA)+ 
#   geom_tile(colour = NA, fill = NA, data = tot_physiodrug2 %>% filter(df == "blanks_physiodrug", !tree3cond == "C13") ) + 
#   geom_text(colour = "white") + 
#   theme_bw() +  
#   theme(text = element_text(size=10, color = "black"),  
#         axis.text.x = element_text(angle=45, hjust=1)) + 
#   scale_fill_viridis_c("%", guide = FALSE) 
# plot_physiodrug 
#  
# #phenom---- 
# htmap_phenom<- htmp_lab %>%  
#   filter(mesh_type == "phenom") 
#  
# tot_phenom <- cond_smry %>%  
#   left_join(htmap_phenom) %>% 
#   as_tibble() %>%  
#   left_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease)) %>%  
#   mutate(tree3lab = str_sub(tree, 1,3)) 
#  
#  
# tot_phenom <- tot_phenom %>%  
#   group_by(tree3cond) %>%  
#   mutate(tot = sum(!duplicated(nct_id))) %>%  
#   ungroup() %>%  
#   group_by(tree3cond, nm) %>%  
#   mutate(n = sum(!duplicated(nct_id))) %>%  
#   ungroup() %>%  
#   distinct(tree3cond, nm, n, tot, conditions, tree3lab) 
# tot_phenom <- tot_phenom %>%  
#   mutate(m = n/tot) %>% 
#   mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")"), 
#          tree_subgroup = paste0(tree3lab, " - ", nm)) 
#  
# blanks_phenom <- data.frame(tree3cond = c( "C06", "C10","C11", "C17", "C19", "C20", "C23"), 
#                           tot = c("73", "161", "10", "65", "10", "12", "43")) %>%  
#   mutate(tot = as.integer(tot)) 
# blanks_phenom <- blanks_phenom %>% #to show the diagonal patterns 
#   inner_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease))  
#  
# tot_phenom2<- bind_rows(blanks_phenom = blanks_phenom, res = tot_phenom, .id = "df") %>%  
#   mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")")) 
#  
# tot_phenom2 <- tot_phenom2 %>%  
#   mutate(m = 100*m, 
#          mlbl = if_else(m<10, format(round(m,1), nsmall = 1), as.character(round(m)))) 
# plot_phenom <- ggplot(tot_phenom2 %>%  filter(!is.na(tree3lab) & !tree3cond == "C13"),  
#                       aes(x = tree_subgroup, y = tree_condition, fill = m, label = mlbl)) + 
#   xlab("Subgroups of Phenomena and Processes [G]") + 
#   ylab("Conditions") + 
#   geom_tile(colour = NA)+ 
#   geom_tile(colour = NA, fill = NA, data = tot_phenom2 %>% filter(df == "blanks_phenom", !tree3cond == "C13")) + 
#   geom_text(colour = "white") + 
#   theme_bw() +  
#   theme(text = element_text(size=10, color = "black"),  
#         axis.text.x = element_text(angle=45, hjust=1)) + 
#   scale_fill_viridis_c("%", guide = FALSE) 
# plot_phenom 
# #moa---- 
# htmap_moa<- htmp_lab %>%  
#   filter(mesh_type == "moa") 
#  
# tot_moa <- cond_smry %>%  
#   left_join(htmap_moa) %>% 
#   as_tibble() %>%  
#   left_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease)) %>%  
#   mutate(tree3lab = str_sub(tree, 1,3)) 
#  
#  
# tot_moa <- tot_moa %>%  
#   group_by(tree3cond) %>%  
#   mutate(tot = sum(!duplicated(nct_id))) %>%  
#   ungroup() %>%  
#   group_by(tree3cond, nm) %>%  
#   mutate(n = sum(!duplicated(nct_id))) %>%  
#   ungroup() %>%  
#   distinct(tree3cond, nm, n, tot, conditions, tree3lab) 
# tot_moa <- tot_moa %>%  
#   mutate(m = n/tot) %>% 
#   mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")"), 
#          tree_subgroup = paste0(tree3lab, " - ", nm)) 
# blanks_moa <- data.frame(tree3cond = c( "C06","C11", "C19", "C20", "C23"), 
#                             tot = c("73", "10", "10", "12", "43")) %>%  
#   mutate(tot = as.integer(tot)) 
# blanks_moa <- blanks_moa %>% #to show the diagonal patterns 
#   inner_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease))  
#  
# tot_moa2<- bind_rows(blanks_moa = blanks_moa, res = tot_moa, .id = "df") %>%  
#   mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")")) 
#  
# tot_moa2 <- tot_moa2 %>%  
#   mutate(m = 100*m, 
#          mlbl = if_else(m<10, format(round(m,1), nsmall = 1), as.character(round(m)))) 
# plot_moa <- ggplot(tot_moa2 %>%  filter(!is.na(tree3lab) & !tree3cond == "C13"),  
#                       aes(x = tree_subgroup, y = tree_condition, fill = m, label = mlbl)) + 
#   xlab("Subgroups of Molecular Mechanisms of Pharmacological Action [D27.505.519]") + 
#   ylab("Conditions") + 
#   geom_tile(colour = NA)+ 
#   geom_tile(colour = NA, fill = NA, data = tot_moa2 %>% filter(df == "blanks_moa", !tree3cond == "C13")) + 
#   geom_text(colour = "white") + 
#   theme_bw() +  
#   theme(text = element_text(size=10, color = "black"),  
#         axis.text.x = element_text(angle=45, hjust=1)) + 
#   scale_fill_viridis_c("%", guide = FALSE) 
# plot_moa 
#  
# #anat, too few, no need---- 
# htmap_anat<- htmp_lab %>%  
#   filter(mesh_type == "anat") 
#  
# tot_anat <- cond_smry %>%  
#   left_join(htmap_anat) %>% 
#   as_tibble() %>%  
#   left_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease)) %>%  
#   mutate(tree3lab = str_sub(tree, 1,3)) 
#  
#  
# tot_anat <- tot_anat %>%  
#   group_by(tree3cond) %>%  
#   mutate(tot = sum(!duplicated(nct_id))) %>%  
#   ungroup() %>%  
#   group_by(tree3cond, nm) %>%  
#   mutate(n = sum(!duplicated(nct_id))) %>%  
#   ungroup() %>%  
#   distinct(tree3cond, nm, n, tot, conditions, tree3lab) 
# tot_anat <- tot_anat %>%  
#   mutate(m = n/tot) %>% 
#   mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")"), 
#          tree_subgroup = paste0(tree3lab, " - ", nm)) 
# tot_anat2 <- tot_anat2 %>%  
#   mutate(m = 100*m, 
#          mlbl = if_else(m<10, format(round(m,1), nsmall = 1), as.character(round(m)))) 
# plot_anat <- ggplot(tot_anat2 %>%  filter(!is.na(tree3lab) & !tree3cond == "C13"),  
#                    aes(x = tree_subgroup, y = tree_condition, fill = m, label = mlbl)) + 
#   xlab("Subgroups") + 
#   ylab("Conditions") + 
#   geom_tile(colour = NA)+ 
#   geom_tile(colour = NA, fill = NA, data = tot_anat2 %>% filter(!tree3cond == "C13")) + 
#   geom_text(colour = "white") + 
#   theme_bw() +  
#   theme(text = element_text(size=10, color = "black"),  
#         axis.text.x = element_text(angle=45, hjust=1)) + 
#   scale_fill_viridis_c("%", guide = FALSE) 
# plot_anat 
