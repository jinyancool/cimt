pkgs <- c("fs", "configr", "jhtools", "glue", "tidyverse")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}") %>% checkdir()
setwd(workdir)

config_fn <- glue("~/projects/{project}/code/{dataset}/human/clinical/configs/configs.yaml")
gender_col <- show_me_the_colors(config_fn, iterm = "gender")
level_col <- show_me_the_colors(config_fn, iterm = "level")
vertical_line_age_col <- show_me_the_colors(config_fn, iterm = "vertical_line_age")

source("/cluster/home/ztao_jh/projects/healthman/code/zhanglei/human/clinical/fig4/new_xgboost_nn/model_info.R")
out_put_dir <- "./clinical/tables"
rawobj <- read_rds(glue("{out_put_dir}/newadd_test.rds"))
input_dir <- "./clinical/figures/fig4/model/model_ans" %>% checkdir
model_list <- read_rds(glue("{input_dir}/search_ans_model_list.rds"))

table_frame <- rawobj %>% ungroup()
table_frame$max_data <- log10(1 + table_frame$max_data)
table_frame$gender_level <- ifelse(table_frame$gender_level == "Male", 1, 0)

test_df <- table_frame %>% arrange(!!sym(main_value))
test_df$test_index <- 1:nrow(test_df)
pddf <- main_predict(model_list,test_df, jx, strats_value, main_value, width)

out_dir_point <- "./clinical/figures/fig4/model/model_predict_point" %>% checkdir

cor_number <- cor(pddf$preidct, pddf$true, method = "spearman")
pp1 <- ggplot(pddf, aes(x = preidct, y = true)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(se = F, span = 1) +
  theme_bmbdc(font_size = 16, fill = "white")+
  labs(caption = glue::glue("cor : {signif(cor_number,3)}"))  
pdf(glue("{out_dir_point}/add_data_point.pdf"),width = 6, height = 5)
print(
  pp1
)
dev.off()

male_frame <- pddf %>% dplyr::filter(gender == 1)
male_cor_number <- cor(male_frame$preidct, male_frame$true, method = "spearman")
female_frame <- pddf %>% dplyr::filter(gender == 0)
female_cor_number <- cor(female_frame$preidct, female_frame$true, method = "spearman")
pp2 <- ggplot(pddf %>% dplyr::filter(gender == 1), aes(x = preidct, y = true)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(se = F, span = 1)  +
  labs(caption = glue("cor male : {signif(male_cor_number,4)}"))  +
  theme_bmbdc(font_size = 16, fill = "white")
pp3 <- ggplot(pddf %>% dplyr::filter(gender == 0), aes(x = preidct, y = true)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(se = F, span = 1) +
  labs(caption = glue("cor female: {signif(female_cor_number,4)}"))  +
  theme_bmbdc(font_size = 16, fill = "white")
pdf(glue("{out_dir_point}/add_data_point_gender.pdf"),width = 4.5, height = 4)
print(
  pp2
)
print(
  pp3
)
dev.off()
write_rds(list(total = pp1, male = pp2, female = pp3), file = glue("{out_dir_point}/add_data_point_oldsplit.rds"))



