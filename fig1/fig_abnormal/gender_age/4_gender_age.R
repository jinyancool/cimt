pkgs <- c("fs", "configr", "ggthemes", "jhtools", "glue", "patchwork", "tidyverse", "ggpubr")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}") %>% checkdir()
setwd(workdir)

box_pth <- "./clinical/figures/fig1/gender_age" %>% checkdir()
rds_fn <- "clinical/tables/raw_data.rds"
rawobj <- read_rds(rds_fn)
config_fn <- glue("clinical/configs/configs.yaml")
gender_col <- show_me_the_colors(config_fn, iterm = "gender")
level_col <- show_me_the_colors(config_fn, iterm = "level")
vertical_line_age_col <- show_me_the_colors(config_fn, iterm = "vertical_line_age")


# =========================== 性别盒子图 =======================================
nolevel <- c(
  "gender_level","age_level", # 性别 年龄
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
  "TC","HDL","LDL","NHDL" # TC HDL LDL NHDL
)
boxsub1_nolevel_frame <- rawobj[, nolevel]
boxsub1_nolevel_frame$gender_level <- factor(boxsub1_nolevel_frame$gender_level, levels = c("Female", "Male"))
ic <- nolevel[-c(1, 2)]
my_comparisons <- list(c("Female", "Male"))

pt <- list()
csvf <- list()
for(i in ic){
  select_v <- c("gender_level", "age_level", i)
  boxsub1_nolevel_framex <- boxsub1_nolevel_frame %>%
    dplyr::select(all_of(select_v)) %>%
    na.omit()
  csvf[[i]] <- boxsub1_nolevel_framex %>% 
    group_by(gender_level, age_level) %>% 
    summarise("{i}" := mean(.data[[i]])) %>% 
    ungroup()
  pt[[i]] <- ggboxplot(boxsub1_nolevel_framex, x = "gender_level", y = i, fill = "gender_level",
                       palette = gender_col) +
    stat_compare_means(comparisons = my_comparisons, method = "wilcox.test") +
    theme_bmbdc(font_size = 16, fill = "white") + 
    ggtitle(i) +
    theme(strip.background = element_rect(fill = NA, colour = NA), plot.title = element_text(hjust = 1)) +
    facet_wrap(~ age_level, nrow = 1) +
    labs(caption = glue("sample_size:{nrow(boxsub1_nolevel_framex)}")) + 
    xlab("") +
    ylab("") +
    coord_cartesian(clip = "off")
}
multi_plot(pt, fig_fn = glue("{box_pth}/age_gender.pdf"), width = 12, height = 10, ncol = 1, nrow = 3)
write_rds(pt, file = glue("{box_pth}/age_gender.rds"))
write_csv(csvf %>% purrr::reduce(left_join, by = c("gender_level", "age_level")) %>% arrange(age_level), file = glue("{box_pth}/age_gender.csv"))
