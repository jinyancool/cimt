pkgs <- c("fs", "futile.logger", "configr", "tidyverse",
          "jhtools", "glue", "patchwork",  "ComplexHeatmap", "ggpubr")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}/") %>% checkdir()
setwd(workdir)
in_put_dir <- "./clinical/tables"
rawobj <- read_rds(glue::glue("{in_put_dir}/raw_data.rds")) %>% 
  mutate(
    NL = `NL` %>% str_remove("岁") %>% as.numeric(),
    age_bin = str_extract(age_level, pattern = "\\d+") %>% as.numeric()
  ) %>% dplyr::filter(NL <= 70)
config_fn <- glue("./clinical/configs/configs.yaml")
gender_col <- show_me_the_colors(config_fn, iterm = "gender")
level_col <- show_me_the_colors(config_fn, iterm = "level")
vertical_line_age_col <- show_me_the_colors(config_fn, iterm = "vertical_line_age")

names(level_col)[2] <- "Risk"

out_path <- checkdir("./clinical/figures/fig2/feature")
csv_pth <- checkdir(glue("{out_path}/csv"))

level1 <- c(
  "IMT_level",
  "gender_level","age_level",# 性别 年龄
  "systolic_blood_pressure_level","blood_pressure_level", # 血压
  "bmi_level", # 腹围bmi
  "TC_level","HDL_level", # 总胆固醇 HDL
  "LDL_level", # LDL 空腹血糖
  "fatty_liver", # 脂肪肝
  "glycated_hemoglobinA1_level","glycated_hemoglobinA1C_level"
)
boxsub3_core_frame <- rawobj[, level1] 
v2 <- level1[-c(1, 2, 3, 4, 5)]
boxsub3_core_frame$IMT_level <- factor(boxsub3_core_frame$IMT_level, levels = c("Normal", "Level1", "Level2" ))
boxsub3_core_frame <- boxsub3_core_frame %>% mutate(
  new_systolic_blood_pressure_level = case_when(
    is.na(systolic_blood_pressure_level) ~ NA,
    systolic_blood_pressure_level %in% c(0) ~ "Normal",
    systolic_blood_pressure_level %in% c(1, 2, 3) ~ "Risk"
  ),
  new_blood_pressure_level = case_when(
    is.na(blood_pressure_level) ~ NA,
    blood_pressure_level %in% c(0) ~ "Normal",
    blood_pressure_level %in% c(1, 2, 3) ~ "Risk"
  ),
  new_bmi_level = case_when(
    is.na(bmi_level) ~ NA,
    bmi_level %in% c(0) ~ "Normal",
    bmi_level %in% c(1, 2) ~ "Risk"
  ),
  new_LDL_level = case_when(
    is.na(LDL_level) ~ NA,
    LDL_level %in% c(0, 1) ~ "Normal",
    LDL_level %in% c(2, 3, 4) ~ "Risk"
  ),
  new_fatty_liver = case_when(
    is.na(fatty_liver) ~ NA,
    fatty_liver %in% c(0, 1) ~ "Normal",
    fatty_liver %in% c(2, 3) ~ "Risk"
  ),
  age_level = str_remove(age_level, pattern = "<= "),
  IMT_level = case_when(
    is.na(IMT_level) ~ NA,
    IMT_level %in% c("Normal") ~ "Normal",
    IMT_level %in% c("Level1", "Level2") ~ "Level1orLevel2"
  )
)
new_v <- c("new_systolic_blood_pressure_level", "new_blood_pressure_level", "new_bmi_level", "new_LDL_level", "new_fatty_liver")
# ======== 分面：性别x厚度等级，分组：特征，x：年龄，y：厚度百分比 =============
pfisher.test <- function(x, y){
  if(nrow(x) > 1){
    p <- fisher.test(x, y)$p.value
  }else{
    p <- NA
  }
  p
}

plist <- list()
pdf(glue("{out_path}/feature_smooth.pdf"), 
    width = 12, height = 4)
for(i in new_v){
  subf1 <- boxsub3_core_frame %>% 
    dplyr::select("gender_level","age_level", "IMT_level", {{i}}) %>% na.omit() %>%
    group_by(gender_level, age_level, IMT_level, .data[[i]]) %>%
    dplyr::summarise(count = n()) %>% ungroup()
  subf2 <- subf1 %>% group_by(gender_level, age_level, .data[[i]]) %>%
    dplyr::summarise(total_number = sum(count)) %>% ungroup()
  full_sub <- subf1 %>% left_join(subf2) %>% dplyr::mutate(pct = count / total_number * 100)
  full_sub[[i]] <- as.character(full_sub[[i]] )
  plist[[i]] <- ggplot() +
    geom_smooth(data = full_sub, mapping = aes(x = age_level, y = pct, group = .data[[i]], color = .data[[i]]), se = F, span = 1.5) + 
    scale_color_manual(values = level_col) +
    geom_point(data = full_sub, mapping = aes(x = age_level, y = pct, group = .data[[i]], color = .data[[i]], size = total_number)) +
    theme_bmbdc(font_size = 16, fill = "white") + 
    facet_grid(gender_level ~ IMT_level) +
    labs(colour = "Risk_level", size = "number_of_people", title = i %>% str_remove_all("new_|_level")) +
    theme(plot.title = element_text(hjust = 1))
  
  test_frame <- full_sub[,c(1, 2,3,4,5)] %>%
    pivot_wider(names_from = "IMT_level", values_from = "count", id_cols = all_of(c("gender_level","age_level", i))) %>%
    na.omit() %>%
    group_by(gender_level, age_level) %>%
    summarise(p.value = pfisher.test(pick(`Level1orLevel2`, Normal))) %>%
    mutate(p.value = signif(p.value, 3)) %>%
    ungroup()

  write_csv(full_sub %>% dplyr::mutate(pct = signif(pct, 5)) %>% left_join(test_frame, by = c("gender_level", "age_level")), glue("{csv_pth}/{i}_feature_smooth.csv"))
  print(plist[[i]])
}
dev.off()
write_rds(plist, glue("{out_path}/feature_smooth.rds"))





