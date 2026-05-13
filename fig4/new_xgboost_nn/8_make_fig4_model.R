pkgs <- c("ggthemes", "jhtools", "glue", "patchwork", "tidyverse")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}") %>% checkdir()
setwd(workdir)

# ========================= fig4 modelA =====================================
out_dir_point <- "./clinical/figures/fig4/model/model_predict_point" %>% checkdir
old_test <- read_rds(file = glue("{out_dir_point}/point_oldsplit.rds"))
add_test <- read_rds(file = glue("{out_dir_point}/add_data_point_oldsplit.rds"))
add_test2 <- read_rds(file = glue("{out_dir_point}/add_data_point_oldsplit2.rds"))
model_importance_dir <- "./clinical/figures/fig4/model/model_importance" %>% checkdir
importance_heatmap <- read_rds(file = glue("{model_importance_dir}/heatmap_importance2.rds"))
out_dir <- "./clinical/figures/fig4/model/model_make" %>% checkdir
p1 <- (old_test[[1]] + labs(tag = "B")) + (old_test[[3]] + labs(tag = "C")) + (old_test[[2]] + labs(tag = "D"))
p2 <- (add_test[[1]] + labs(tag = "E")) + (add_test[[3]] + labs(tag = "F")) + (add_test[[2]] + labs(tag = "G"))
p3 <- (add_test2[[1]] + labs(tag = "H")) + (add_test2[[3]] + labs(tag = "I"))+ (add_test2[[2]] + labs(tag = "J"))

pdf(glue("{out_dir}/fig4A.pdf"), width = 14, height = 8)
print(((importance_heatmap + labs(tag = "A")) + p1/p2)/p3)
dev.off()
# ========================= fig4 modelB =====================================
healthy_dir <- "./clinical/figures/fig4/model/model_embedding/healthy"
healthy_obj <- read_rds(glue("{healthy_dir}/pca_healthy.rds"))
arrow_dir <- "./clinical/figures/fig4/model/model_embedding/arrow"
arrow_pca_obj <- read_rds(glue("{arrow_dir}/arrow_pca.rds"))
total_pca_dir <- "./clinical/figures/fig4/model/model_embedding/total_pca"
total_pca <- read_rds(glue("{total_pca_dir}/pca_total_patchwork.rds"))

pdf(glue("{out_dir}/fig4B.pdf"), width = 14, height = 13)
print(
  total_pca[[1]] + total_pca[[2]] + arrow_pca_obj[["HDL"]][["figb"]] + 
    arrow_pca_obj[["age"]][["figb"]] + arrow_pca_obj[["gender_level"]][["figb"]] + arrow_pca_obj[["fatty_liver"]][["figb"]] +
    healthy_obj[[1]] + healthy_obj[[2]] + healthy_obj[[3]] +
    plot_annotation(tag_level = "A")
)
dev.off()
# ========================= fig4 modelC =====================================
pdf(glue("{out_dir}/fig4C.pdf"), width = 14, height = 10)
print(
  arrow_pca_obj[["age"]][["figa"]] + arrow_pca_obj[["gender_level"]][["figa"]] + arrow_pca_obj[["systolic_blood_pressure"]][["figa"]] + arrow_pca_obj[["diastolic_pressure"]][["figa"]] + arrow_pca_obj[["glycated_hemoglobinA1"]][["figa"]] + 
    arrow_pca_obj[["HDL"]][["figa"]] + arrow_pca_obj[["LDL"]][["figa"]] + arrow_pca_obj[["NHDL"]][["figa"]] + arrow_pca_obj[["bmi"]][["figa"]] + arrow_pca_obj[["fatty_liver"]][["figa"]] +
    arrow_pca_obj[["white_blood_cell_count"]][["figa"]] + arrow_pca_obj[["lymphocyte_percentage"]][["figa"]] + arrow_pca_obj[["neutrophils"]][["figa"]] + arrow_pca_obj[["urea"]][["figa"]] + arrow_pca_obj[["uric_acid"]][["figa"]] +
    plot_annotation(tag_level = "A") + plot_layout(ncol = 5) & 
    theme(legend.position = "none") 
)
dev.off()

