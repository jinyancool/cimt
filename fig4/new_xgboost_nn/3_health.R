pkgs <- c("jhtools", "glue", "tidyverse","ggthemes","patchwork")
for (pkg in pkgs){
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}") %>% checkdir()
setwd(workdir)

input_model_dir <- "./clinical/figures/fig4/model/model_ans"
feature_list <- read_rds(glue("{input_model_dir}/search_ans_feature_list.rds"))
model_list <- read_rds(glue("{input_model_dir}/search_ans_model_list.rds"))

model_embedding_dir <- "./clinical/figures/fig4/model/model_embedding/total_pca" 
raw_pca_frame <- read_rds(glue("{model_embedding_dir}/raw_pca_frame.rds"))
naomit_embedding <- read_rds(glue("{model_embedding_dir}/naomit_embedding.rds"))

out_dir <- "./clinical/figures/fig4/model/model_embedding/healthy" %>% checkdir

top_tb <- cbind(raw_pca_frame, naomit_embedding) %>% as_tibble() %>% mutate(label = "background")
p01 <- top_tb %>%
  mutate(healthy = naomit_embedding$fatty_liver == 0 &
           naomit_embedding$LDL< 2 &
           naomit_embedding$systolic_blood_pressure < 130 &
           naomit_embedding$diastolic_pressure < 80 &
           naomit_embedding$bmi < 24) %>%
  arrange(desc(is.na(healthy)),healthy) %>%
  ggplot(aes(x = PC1, y = PC2, color = healthy)) +
  geom_point() +
  theme_few() +
  scale_color_manual(values = c("FALSE" = "#cf3c27", "TRUE" = "#3a74b7")) +
  theme(legend.position = "bottom")
real_heal_th1 <- top_tb %>%
  mutate(healthy = naomit_embedding$fatty_liver == 0 &
           naomit_embedding$LDL< 2 &
           naomit_embedding$systolic_blood_pressure < 130 &
           naomit_embedding$diastolic_pressure < 80 &
           naomit_embedding$bmi < 24) %>% 
  group_by(age, gender_level) %>% 
  dplyr::summarise(max_data = mean(max_data)) %>% 
  dplyr::mutate(gender_level = as.character(gender_level),
                max_data = 10^max_data-1) %>% 
  dplyr::filter(age < 75) %>% 
  ggplot(., aes(x = age, y = max_data, color = gender_level)) + 
  geom_smooth(size = 1, se = F, span =0.5) +
  theme_few() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 1))+
  scale_y_continuous(limits = c(0.5,1.8)) +
  scale_color_manual(values = c("0" = "#cf3c27", "1" = "#3a74b7")) +
  ggtitle("IMT") +
  ylab("IMT")
real_heal_th_predict <-top_tb %>%
  mutate(healthy = naomit_embedding$fatty_liver == 0 &
           naomit_embedding$LDL< 2 &
           naomit_embedding$systolic_blood_pressure < 130 &
           naomit_embedding$diastolic_pressure < 80 &
           naomit_embedding$bmi < 24) %>% 
  group_by(age,gender_level) %>% 
  dplyr::summarise(preidct = mean(preidct)) %>% 
  dplyr::mutate(gender_level = as.character(gender_level),
                preidct = 10^preidct-1) %>% 
  dplyr::filter(age < 75) %>%
  ggplot(., aes(x = age, y = preidct, color = gender_level)) + 
  geom_smooth(size = 1, se = F, span = 0.5) +
  theme_few() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 1)) +
  scale_y_continuous(limits = c(0.5,1.8)) +
  scale_color_manual(values = c("0" = "#cf3c27", "1" = "#3a74b7")) +
  ggtitle("preidct")

p02 <- real_heal_th1 + real_heal_th_predict + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
pdf(glue("{out_dir}/pick_healthy.pdf"), width = 13, height = 5)
print((p01 + theme(legend.position = "bottom")) + p02 + 
        plot_layout(widths = c(1,2)))
dev.off()
write_rds(list(p01, real_heal_th1, real_heal_th_predict), file = glue("{out_dir}/pca_healthy.rds"))















