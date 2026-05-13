pkgs <- c("fs", "configr", "ggthemes", "jhtools", "glue", "patchwork", "tidyverse", "ggpubr")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}") %>% checkdir()
setwd(workdir)

box_pth <- "./clinical/figures/fig1/total_box" %>% checkdir()
bar_pth <- "./clinical/figures/fig1/total_bar" %>% checkdir()


rds_fn <- "clinical/tables/raw_data.rds"
rawobj <- read_rds(rds_fn)
config_fn <- glue("clinical/configs/configs.yaml")
gender_col <- show_me_the_colors(config_fn, iterm = "gender")
level_col <- show_me_the_colors(config_fn, iterm = "level")
vertical_line_age_col <- show_me_the_colors(config_fn, iterm = "vertical_line_age")


# =========================== 总体盒子图 =======================================
nolevel <- c(
  "IMT_level",
  "gender_level","age_level", "age", # 性别 年龄
  "systolic_blood_pressure","diastolic_pressure", # 收缩压 舒张压
  "heart_rate","height","weight", # 心率 升高 体重
  "abdominal_circumference","bmi", # 腹围 BMI
  "white_blood_cell_count","neutrophils","neutrophil_percentage", # 血
  "lymphocyte_percentage","monocyte_percentage","hemoglobin","platelet_count","platelet_volume",
  "urinary_microalbumin", "urine_microalbuminuria_creatinine_ratio", "total_protein", "albumin", #尿微量蛋白  白蛋白 总蛋白
  "alanine_aminotransferase", # 谷丙转氨酶
  "aspartate_aminotransferase", # 谷草转氨酶
  "creatinine", # 肌酐
  "urea","uric_acid", # 尿素 尿酸
  "glomerular_filtration_rate","homocysteine", # 肾小球滤过率 同型半胱氨酸
  "triglycerides","fasting_blood_sugar", # 甘油三酯  空腹血糖
  "apolipoproteinA1","apolipoproteinB", # 载脂蛋白A1 载脂蛋白B
  "apolipoproteinE","glycated_hemoglobinA1", # 载脂蛋白E 糖化血红蛋白A1
  "glycated_hemoglobinA1C","fasting_C_peptide", # 糖化血红蛋白A1C  空腹C肽
  "fasting_insulin","hsc_reactive_protein", # 空腹胰岛素  超敏C反应蛋白
  "sialic_acid","free_fatty_acid",# 唾液酸 游离脂肪酸
  "Hydroxyvitamin_D3", # 羟基维生素D3
  "TC","HDL","LDL","NHDL", # TC HDL LDL NHDL
  "NHDL_age", "LDL_age",  # NHDL*age LDL*age
  "sleep"
)
boxsub1_nolevel_frame <- rawobj[, nolevel]
boxsub1_nolevel_frame$IMT_level <- factor(boxsub1_nolevel_frame$IMT_level, levels = c("Normal", "Level1", "Level2"))
ic <- nolevel[-c(1, 2, 3)]
my_comparisons <- list(c("Normal", "Level1"),c("Level1","Level2"),c("Normal","Level2"))

pt <- list()
#for(i in ic){
#  select_v <- c("IMT_level",i)
#  boxsub1_nolevel_framex <- boxsub1_nolevel_frame %>%
#    dplyr::select(all_of(select_v)) %>%
#    na.omit()
#  pt[[i]] <- ggboxplot(boxsub1_nolevel_framex, x = "IMT_level", y = i, fill = "IMT_level",
#                       palette = level_col) +
#    stat_compare_means(comparisons = my_comparisons, method = "wilcox.test") +
#    theme_bmbdc(font_size = 16, fill = "white") + 
#    labs(caption = glue("sample_size:{nrow(boxsub1_nolevel_framex)}")) + 
#    coord_cartesian(clip = "off")
#}

ic_sub <- c("glycated_hemoglobinA1", "glycated_hemoglobinA1C")
for(i in ic_sub){
  select_v <- c("IMT_level",i)
  boxsub1_nolevel_framex <- boxsub1_nolevel_frame %>%
    dplyr::select(all_of(select_v)) %>%
    na.omit()
  pt[[i]] <- ggboxplot(boxsub1_nolevel_framex, x = "IMT_level", y = i, fill = "IMT_level",
                       palette = level_col) +
    stat_compare_means(comparisons = my_comparisons, method = "wilcox.test") +
    theme_bmbdc(font_size = 16, fill = "white") +
    labs(caption = glue("sample_size:{nrow(boxsub1_nolevel_framex)}")) +
    coord_cartesian(clip = "off")
}
multi_plot(pt, fig_fn = glue("{box_pth}/level_boxplot.pdf"), width = 12, height = 10)
write_rds(pt, file = glue("{box_pth}/level_boxplot.rds"))

# =========================== 柱子图准备 =======================================
chi_p <- function(df){
  tb <- table(df)
  p <- fisher.test(tb,simulate.p.value=TRUE)$p.value
  p
}
save_table <- function(df){
  cn <- colnames(df)
  tb <- table(df) %>% as.data.frame() %>% 
    mutate(group = "raw_data")
  tb1 <- table(df) %>% as.data.frame() %>% 
    group_by(.data[[cn[[1]]]]) %>% 
    dplyr::mutate(sum = sum(Freq)) %>% 
    dplyr::mutate(Freq = Freq/sum , group = "normalize_lable") %>% 
    dplyr::select(-sum)
  tb2 <- table(df) %>% as.data.frame() %>% 
    group_by(.data[[cn[[2]]]]) %>% 
    dplyr::mutate(sum = sum(Freq)) %>% 
    dplyr::mutate(Freq = Freq/sum , group = "normalize_feature") %>% 
    dplyr::select(-sum)
  ans <- rbind(tb,tb1,tb2) %>% pivot_wider(names_from = cn[[1]], values_from = "Freq") %>% 
    mutate(feature = cn[[2]]) 
  colnames(ans)[1] <- "feature_level"
  ans <- ans %>% dplyr::select(feature, feature_level, group, everything())
  ans
}
level_v <- c(
  "IMT_level",
  "gender_level","age_level", # 性别 年龄
  "drinking_level","smoking_level", # 抽烟 喝酒
  "systolic_blood_pressure_level", "diastolic_pressure_level","blood_pressure_level", # 血压
  "abdominal_circumference_level", "bmi_level", # 腹围bmi
  "white_blood_cell_count_level","neutrophils_level","neutrophil_percentage_level", # 血
  "lymphocyte_percentage_level","monocyte_percentage_level",
  "hemoglobin_level","platelet_count_level","platelet_volume_level","total_protein_level",
  "albumin_level","alanine_aminotransferase_level", "aspartate_aminotransferase_level", # 总蛋白 转氨酶
  "creatinine_level", "urea_level","uric_acid_level", # 肌酐尿素尿酸
  "glomerular_filtration_rate_level", "triglycerides_level", # 肾小球滤过率  甘油三酯
  "TC_level","HDL_level", # 总胆固醇 HDL
  "LDL_level","fasting_blood_sugar_level", # LDL 空腹血糖
  "fatty_liver", # 脂肪肝
  "NHDL_level",
  "glycated_hemoglobinA1C_level", "glycated_hemoglobinA1_level",
  "history_heart_brain","history_diabetes","history_cancer","history_heart",
  "history_sleep","history_mental","history_no","spots","sleep","drug_blood_pressure",
  "drug_sugar","drug_lipid","drug_thyroid","drug_heart","drug_vessel","drug_gout",
  "drug_depress","drug_anxiety","drug_sleep"
)
barsub1_level_frame <- rawobj[, level_v]
barsub1_level_frame$IMT_level <- factor(barsub1_level_frame$IMT_level, levels = c("Normal", "Level1", "Level2"))
ic <- colnames(barsub1_level_frame)[-1]

# =========================== 总体柱子图 =======================================
# 百分比柱子
pt <- list()
csv_frame <- list()
#for(i in ic){
#  barsub1_level_framex <- barsub1_level_frame %>% dplyr::select(all_of(c("IMT_level", i))) %>% na.omit()
#  if(class(barsub1_level_framex[[i]]) != "factor"){
#    barsub1_level_framex[[i]] <- as.character(barsub1_level_framex[[i]])
#  }
#  barsub1_level_framex <- barsub1_level_framex %>% 
#    group_by(.data[[i]]) %>% 
#    dplyr::mutate(count = n()) %>% 
#    dplyr::filter(count > 10) %>% 
#    ungroup() %>% 
#    dplyr::select(-count)
#  if(length(barsub1_level_framex[[i]] %>% unique()) == 1)
#    next
#  pt[[i]] <- ggplot(barsub1_level_framex, aes(x = .data[[i]], fill = IMT_level)) +
#    geom_bar(position = "fill") +
#    theme_bmbdc(font_size = 16, fill = "white") + 
#    theme (legend.position= "top") + 
#    scale_fill_manual(values = level_col) +
#    labs(caption = glue("sample_size:{nrow(barsub1_level_framex)}
#                 pvalue:{signif(chi_p(barsub1_level_framex), 3)}")) +
#    ylab("percent") + 
#    coord_cartesian(clip = "off")
#  csv_frame[[i]] <- save_table(barsub1_level_framex)
#}
ic_sub <- c("glycated_hemoglobinA1C_level", "glycated_hemoglobinA1_level")

for(i in ic_sub){
   barsub1_level_framex <- barsub1_level_frame %>% dplyr::select(all_of(c("IMT_level", i))) %>% na.omit()
   if(class(barsub1_level_framex[[i]]) != "factor"){
     barsub1_level_framex[[i]] <- as.character(barsub1_level_framex[[i]])
  }
  barsub1_level_framex <- barsub1_level_framex %>%
    group_by(.data[[i]]) %>%
    dplyr::mutate(count = n()) %>%
    dplyr::filter(count > 10) %>%
    ungroup() %>%
    dplyr::select(-count)
  if(length(barsub1_level_framex[[i]] %>% unique()) == 1)
     next
   pt[[i]] <- ggplot(barsub1_level_framex, aes(x = .data[[i]], fill = IMT_level)) +
    geom_bar(position = "fill") +
    theme_bmbdc(font_size = 16, fill = "white") +
    theme (legend.position= "top") +
    scale_fill_manual(values = level_col) +
    labs(caption = glue("sample_size:{nrow(barsub1_level_framex)}
                 pvalue:{signif(chi_p(barsub1_level_framex), 3)}")) +
    ylab("percent") +
    coord_cartesian(clip = "off")
  csv_frame[[i]] <- save_table(barsub1_level_framex)
}

multi_plot(pt,
           fig_fn = glue("{bar_pth}/feature_barplot_fill.pdf"),
           width = 12, height = 10,
           ncol = 2, nrow = 2)
write_csv(csv_frame %>% purrr::list_rbind(), file = glue("{bar_pth}/feature_barplot_fill.csv"))
write_rds(pt, file = glue("{bar_pth}/feature_barplot_fill.rds"))
# ============================ 柱子分页 ========================================
pt <- list()
#pdf(glue("{bar_pth}/feature_barplot_fill_face.pdf"), width = 12, height = 4)
#for(i in ic){
#  barsub1_level_framex <- barsub1_level_frame %>%
#    dplyr::select(all_of(c("IMT_level", i,"gender_level","age_level"))) %>%
#    na.omit()
#  if(class(barsub1_level_framex[[i]]) != "factor") {
#    barsub1_level_framex[[i]] <- as.character(barsub1_level_framex[[i]])
#  }
#  pt[[i]] <- ggplot(barsub1_level_framex, aes(x = .data[[i]], fill = IMT_level)) +
#    geom_bar(position = "fill") + scale_fill_manual(values = level_col) +
#    theme_bmbdc(font_size = 16, fill = "white") + 
#    theme (legend.position = "top") +
#    labs(caption = glue("sample_size:{nrow(barsub1_level_framex)}")) +
#    facet_grid(gender_level ~ age_level) +
#    ylab("percent") + 
#    coord_cartesian(clip = "off")
#  print(
#    pt[[i]]
#  )
#}
#dev.off()

ic_sub <- c("systolic_blood_pressure_level", "LDL_level",
            "diastolic_pressure_level", "HDL_level")
pdf(glue("{bar_pth}/feature_barplot_fill_face_sub.pdf"), width = 12, height = 4)
for(i in ic_sub){
  barsub1_level_framex <- barsub1_level_frame %>%
    dplyr::select(all_of(c("IMT_level", i,"gender_level","age_level"))) %>%
    na.omit()
  if(class(barsub1_level_framex[[i]]) != "factor") {
    barsub1_level_framex[[i]] <- as.character(barsub1_level_framex[[i]])
  }
  pt[[i]] <- ggplot(barsub1_level_framex, aes(x = .data[[i]], fill = IMT_level)) +
    geom_bar(position = "fill") + scale_fill_manual(values = level_col) +
    theme_bmbdc(font_size = 16, fill = "white") +
    theme (legend.position = "top") +
    labs(caption = glue("sample_size:{nrow(barsub1_level_framex)}")) +
     facet_grid(gender_level ~ age_level) +
     ylab("percent") +
     coord_cartesian(clip = "off")
   print(
     pt[[i]]
  )
}
dev.off()

write_rds(pt, file = glue("{bar_pth}/feature_barplot_fill_face.rds"))

