library(tidyverse) 
mydf <- read_csv("three_model_results.csv") 

## drop itnercepts 
mydf <- mydf %>%  
  filter(!term == "(Intercept)") 


tc <- c("(Intercept)", "Duration of follow up", "Number of arms > 2", "log(enrollment, base = 10)", "Industry1", "Start year") 
cond <- setdiff(mydf$term, tc) 

cond_order <- mydf %>%  
  filter(term %in% cond) %>%  
  select(model, term, estimate) %>%  
  spread(model, estimate) %>%  
  arrange(Subgroup_reporting) 


cardiomettbe <-  
  c("Acute Coronary Syndrome", "Angina Pectoris",  
    "Atherosclerosis",  
    "Atrial Fibrillation", "Coronary Artery Disease",  
    "Diabetes Mellitus, Type 1", "Diabetes Mellitus, Type 2",  "Diabetes Mellitus", "Diabetic Nephropathies",
    "Heart Failure",  
    "Hypercholesterolemia", "Hypertension",  
    "Ischemic Attack, Transient",   
    "Myocardial Infarction",  
    "Peripheral Arterial Disease", "Prediabetic State",  
    "Stroke", "Cerebral Infarction", "Brain Ischemia", 
    "Venous Thromboembolism", "Hyperlipidemias", "Venous Thrombosis", "Thromboembolism","Pulmonary Embolism", 
    "Retinal Vein Occlusion") 
resp <- c("Idiopathic Interstitial Pneumonias", "Pulmonary Disease, Chronic Obstructive", "Rhinitis") 
gi <- c("Crohn Disease", "Esophagitis", "Gastroesophageal Reflux","Colitis, Ulcerative") 
#gu <- c("Prostatic Hyperplasia", "Urinary Bladder, Overactive", "Urticaria")#other 
neuro <- c("Alzheimer Disease",  
           "Migraine Disorders", "Multiple Sclerosis","Seizures", "Parkinson Disease") 
ms <- c("Arthritis, Psoriatic",  
        "Arthritis, Rheumatoid",     
        "Gout", 
        "Lupus Erythematosus, Systemic",  
        "Lupus Nephritis",  "Osteoarthritis", "Osteoporosis",   
        "Spondylarthropathies", "Spondylitis, Ankylosing") 

mydf <- mydf %>%  
  mutate(term_type =   
           case_when( 
             term %in% tc ~ " General", 
             term %in% cardiomettbe ~ "Cardiometabolic and thromboembolic", 
             # term %in% resp ~ "Respiratory", 
             #term %in% gu ~ "Genitorurinary", 
             # term %in% gi ~ "Gastrointenstinal", 
             term %in% neuro ~ "Neurological", 
             term %in% ms ~ "Musculoskeletal", 
             TRUE ~ "Other" 
           ), 
         model_f = factor(model, levels = c("Subgroup_reporting", "Number_of_subgroups", "Results_reporting"),  
                          labels = c("Any subgroup", "Total subgroups", "Any result"))) %>%  
  mutate(term = case_when( 
    term == "log(enrollment, base = 10)" ~ "Enrollment", 
    term == "Industry1" ~ "Industry", 
    TRUE ~ term)) 

plot1 <- ggplot(mydf, aes(x = term, y = estimate, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error, colour = model_f)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_linerange(position = position_dodge(width = 0.5)) + 
  coord_flip() + 
  facet_wrap(~term_type, drop = TRUE, scales = "free_y") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_color_discrete("") + 
  theme_bw() + 
  scale_y_continuous("OR (any subgroup and any result) and RR (total subgroups)", 
                     breaks = log(c(0.25, 1, 4, 16)), 
                     labels =    (c(0.25, 1, 4, 16))) + 
  scale_x_discrete("") 
plot1 

tiff("Forstplots.tiff", res = 600, compression = "lzw", units = "cm", width = 35, height = 20) 
plot1 
dev.off() 
