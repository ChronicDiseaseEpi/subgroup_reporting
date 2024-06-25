library(readxl) 
library(tidyverse)

#read pmid to nctid lookup table ---- 
nct2pmid<- read.csv("Data/nct_id2pmid.csv") 
pmid2many <- nct2pmid %>% 
  nest(nct_ids = c(nct_id))#for each paper, combine the trials 
pmid2many$nct_ids <- map(pmid2many$nct_ids, pull) 
pmid2many$nct_id_n <- map_int(pmid2many$nct_ids, length) 
rm(nct2pmid)
#read subgroup term data, include every sub ---- 
term<- read.csv("Data/trm_lvl.csv")
#fix errors
term <- term %>%
  mutate(mesh_term = if_else(original_strings_caught_from_tt == "baseline composite mayo clinic score", "Colitis, Ulcerative", mesh_term),
         mesh_term_f = if_else(original_strings_caught_from_tt == "baseline composite mayo clinic score", "Colitis, Ulcerative", mesh_term_f),
         mesh_code = if_else(original_strings_caught_from_tt == "baseline composite mayo clinic score", "D003093", mesh_code),
         mesh_term = if_else(pmid == "22704916" & original_strings_caught_from_tt == "Medical History: Disease Duration", "Crohn Disease", mesh_term),
         mesh_term_f = if_else(pmid == "22704916" & original_strings_caught_from_tt == "Medical History: Disease Duration", "Crohn Disease", mesh_term_f),
         mesh_code = if_else(pmid == "22704916" & original_strings_caught_from_tt == "Medical History: Disease Duration", "D003424", mesh_code))
         # mesh_term = if_else(pmid == "21642014" & original_strings_caught_from_tt == "CDAI score", "Crohn Disease", mesh_term),
         # mesh_term_f = if_else(pmid == "21642014" & original_strings_caught_from_tt == "CDAI score", "Crohn Disease", mesh_term_f),
         # mesh_code = if_else(pmid == "21642014" & original_strings_caught_from_tt == "CDAI score", "D003424", mesh_term))
#read trial condition data ----
condition<- read.csv("Data/trl_cond.csv")  
#order by frequency
order_id1 <- condition %>% 
  count(condition_preferred, sort = T) %>% 
  mutate(order_id = 1:77)
#merge subgroup, trial, and condition ----
term_trial<- term %>%  
  inner_join(pmid2many) %>% 
  unnest(nct_ids) %>% 
  rename(nct_id = nct_ids) %>% 
  left_join(condition, multiple = "all") %>% 
  left_join(order_id1 %>% select(-n)) 
#choose the commonest condition for each trial ----
#pick the commonest condition for each trial with subgroup reporting
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
rm(order_id1)
cond_smry <- trial_2235_1con %>%  
  mutate(tree3cond = str_sub(mesh_broad, 1, 3)) %>%  
  distinct(tree3cond, nct_id, condition_preferred) 
rm(trial_2235_1con)
#read mesh tree data ----
msh_tree_label<- read.csv("Data/msh_tree2lbl.csv") 
msh_tree<- read.csv("Data/msh_tree.csv") 
# keep one tree num for each mesh code
msh_1tree<- msh_tree %>%  
  group_by(mesh_code) %>%
  slice(1) %>% # keep one tree num for each mesh code
  ungroup() %>%
  left_join(msh_tree_label %>% rename(mesh_tree = trees)) 
rm(msh_tree_label,msh_tree )

#join subgroup data with mesh tree ----
htmp_smry <- term_trial %>%  
  select(nct_id, mesh_term, mesh_code) %>% 
  left_join(msh_1tree) 
#choose disease subgroup terms and remove duplicates
htmap_clabel<- htmp_smry %>%  
  filter(tree_lbl == "C: Diseases") %>%  
  mutate(tree3sub = str_sub(mesh_tree, 1, 3)) %>%  
  distinct(tree3sub, nct_id, mesh_term) 

#read disease tree name----
tree_name <- read_excel("Data/disease_tree_name.xlsx") 

#exclude the subgroup term where same as the condition----
htmap_clabel_nosame <- htmap_clabel %>%  
  anti_join(cond_smry %>%  
              rename(mesh_term = condition_preferred) %>%  
              distinct(nct_id, mesh_term))  %>%
  mutate(mesh_term = if_else(nct_id == "NCT00552058", "Crohn Disease", mesh_term),#HERE to fix a litter error
         tree3sub = if_else(nct_id == "NCT00552058", "C06", tree3sub))
rm(htmap_clabel, htmp_smry)
#join trial condition with subgroup terms, and their disease systems ----  
tot_smry <- cond_smry %>%  
  left_join(htmap_clabel_nosame, multiple = "all") %>%  
  as_tibble() %>%  
  arrange(nct_id) %>% 
  left_join(tree_name %>% rename(tree3cond = tree_num, conditions = disease)) %>% 
  left_join(tree_name %>% rename(tree3sub = tree_num, subgroups = disease)) 

tot_smry <- tot_smry %>%  
  group_by(tree3cond) %>%  
  mutate(tot = sum(!duplicated(nct_id))) %>%  #for each condition tree, how many unique trials
  ungroup() %>%  
  group_by(tree3cond, tree3sub) %>%  
  mutate(n = sum(!duplicated(nct_id))) %>%  #for each sub tree in each condition, how many unique trials
  ungroup() %>%  
  distinct(tree3cond, tree3sub, n, tot, conditions, subgroups) 
tot_smry <- tot_smry %>%  
  mutate(m = n/tot) %>% 
  mutate(tree_condition = paste0(tree3cond, " - ", conditions, " (n=", tot, ")"), 
         tree_subgroup = paste0(tree3sub, " - ", subgroups)) 

#make the plot----
tot_smry2 <- tot_smry %>%  
  mutate(m = 100*m, 
         mlbl = if_else(m<10, format(round(m,1), nsmall = 1), as.character(round(m)))) #keep 2 numbers for each 
tot_smry2 <- tot_smry2 %>% 
  mutate(brdr = if_else(tree3cond == tree3sub, "red", NA))
rm(cond_smry, condition, htmap_clabel_nosame, msh_1tree, pmid2many, term, term_trial, tot_smry, tree_name, trial_1con)

plot1 <- ggplot(tot_smry2 %>%  
                  filter(!is.na(tree3sub), ! tree3sub == "C23", 
                         !tree3cond %in% c("C20", "C23", "C15", "C07","C11")),  
                aes(x = tree_subgroup, y = tree_condition, fill = m, label = mlbl,
                    colour = brdr)) + 
  xlab("Disease systems for subgroups") + 
  ylab("Disease systems for index conditions") + 
  geom_tile(linewidth = 0.75)+ 
#  geom_tile(colour = NA, fill = NA, data = tot_smry2 %>% filter(df == "blanks")) + 
  geom_text(colour = "white") + 
  theme_bw() +  
  theme(text = element_text(size=16, color = "black"),  
        axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_fill_viridis_c("%", guide = "none") +
  scale_colour_identity()
plot1  

# tiff("tile.tiff", res = 1200, compression = "lzw", units = "cm", width = 35, height = 25) 
# plot1 
# dev.off() 
