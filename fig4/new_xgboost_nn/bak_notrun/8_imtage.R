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
data_dir <- "./clinical/tables"
rawobj <- read_rds(glue("{data_dir}/raw_data.rds"))
input_dir <- "./clinical/figures/fig4/model/model_ans" %>% checkdir
model_list <- read_rds(glue("{input_dir}/search_ans_model_list.rds"))

table_frame <- rawobj %>% ungroup()
table_frame$max_data <- log10(1 + table_frame$max_data)
table_frame$gender_level <- ifelse(table_frame$gender_level == "Male", 1, 0)
table_frame$test_index <- 1:nrow(table_frame)

all_features <- lapply(model_list, function(x){
  x$feature_names
}) %>% unlist() %>% unique()
test_df <- table_frame[,c(all_features, "max_data", "test_index")]

age_i <- 20
full_frame <- lapply(20:80, function(age_i){
  sub_all_frame <- test_df
  sub_all_frame[["age"]] <- age_i
  tmp <- main_predict(model_list, sub_all_frame, jx, strats_value, main_value, width) %>% 
    dplyr::mutate(true_age = test_df$age,
                  delta = preidct - true,
                  abs_delta = abs(delta),
                  delta_age = age - true_age,
                  abs_delta_age = abs(delta_age)) 
  tmp
}) %>% list_rbind() %>% 
  group_by(index) %>% 
  slice_min(order_by = abs_delta, n = 1) %>% 
  slice_min(order_by = abs_delta_age, n = 1) %>% 
  ungroup()
mit_age_path <- "./clinical/figures/fig4/model/mit_age" %>% checkdir
write_rds(full_frame, file = glue("{mit_age_path}/predict.rds"))

v <-  c("lymphocyte_percentage", "white_blood_cell_count","HDL","fatty_liver",
        "uric_acid","age","LDL","bmi","NHDL","gender_level","systolic_blood_pressure",
        "glycated_hemoglobinA1","urea","neutrophils","diastolic_pressure")
full_framex <- full_frame %>% 
  dplyr::select(test_index = index, preidct, true, predict_age = age, true_age) %>% 
  left_join(table_frame %>% dplyr::select(all_of(
   c(v, "test_index","age_level","IMT")
  )), by = "test_index")  %>% 
  dplyr::mutate(error = preidct - true) %>% 
  dplyr::filter(abs_delta < 0.1)

# cor(full_framex$age, full_framex$predict_age, method = "spearman")
# # 0.5567176

full_framex %>% 
  dplyr::select(all_of(c(v,"error"))) %>% 
  dplyr::mutate(error_sign = sign(error)) %>% 
  pivot_longer(-"error_sign") %>% 
  group_by(error_sign, name) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  ungroup() %>% pivot_wider(id_cols = "name", names_from = "error_sign") %>% 
  dplyr::filter(name != "error")


all_point <- ggplot(full_framex, 
                    aes(x = predict_age, y = true_age, color = error)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0) +
  scale_color_gradient2(low = "blue",mid = "grey90", high = "red") +
  theme_bmbdc(font_size = 16, fill = "white")
pdf(glue("{mit_age_path}/error_predict_age.pdf"))
print(all_point)
dev.off()

all_point_list <- list()
for(f in base::setdiff(v, c("gender_level","age"))){
  all_point_list[[f]] <- ggplot(full_framex, aes(x = .data[[f]], y = error, color = IMT)) + 
    geom_point(alpha = 0.5) +
    theme_bmbdc(font_size = 16, fill = "white") +
    coord_cartesian(clip = "off") +
    geom_smooth(method = "lm",se = F, linetype="dashed") +
    scale_color_viridis_c()
}
multi_plot(fig_fn = glue("{mit_age_path}/error_predict_age.pdf"), p_list = all_point_list)



coor_plot <- lapply(v, function(x){
  tmp <- cor.test(full_framex[[x]], full_framex$error, method = "spearman")
  tibble(
    feature = x,
    p.value = tmp$p.value,
    R = tmp$estimate)
}) %>% list_rbind() %>% dplyr::filter(!feature %in% c("gender_level","age")) %>% 
  arrange(desc(R)) %>% 
  dplyr::mutate(feature = factor(feature, levels = feature)) %>% 
  ggplot(aes(x = feature, y = R, color = R, size = -log10(p.value))) + 
  geom_point() +
  theme_bmbdc(font_size = 16, fill = "white") +
  scale_color_gradient2(low = "blue",mid = "grey90", high = "red") +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey20")+ 
  geom_hline(yintercept = 0.03, color = "grey90") +
  geom_hline(yintercept = - 0.03, color = "grey90") +
  coord_flip(clip = "off") +
pdf(glue("{mit_age_path}/pvalue.pdf"))
print(coor_plot)
dev.off()



library(patchwork)
pleft <- all_point +
  all_point_list[["glycated_hemoglobinA1"]] + 
  all_point_list[["systolic_blood_pressure"]] + 
  all_point_list[["diastolic_pressure"]] + 
  all_point_list[["urea"]] + 
  all_point_list[["fatty_liver"]] + 
  all_point_list[["bmi"]] + 
  all_point_list[["NHDL"]] + 
  all_point_list[["LDL"]] + 
  all_point_list[["uric_acid"]] + 
  all_point_list[["lymphocyte_percentage"]] + 
  all_point_list[["HDL"]]  + 
  plot_layout(ncol = 4, widths = c(1,1,1,1))
pright <- coor_plot
pdf(glue("{mit_age_path}/make1.pdf"), width = 25, height = 10)
print((pleft | pright) + plot_layout(ncol = 2, widths = c(4,1.5)) +
  plot_annotation(tag_level = "A"))
dev.off()
 

