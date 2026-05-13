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

rawobj <- readxl::read_xlsx("clinical/tables/fix/缙云_颈动脉.xlsx")
nac <- apply(rawobj, 2, function(x) sum(is.na(x)))
rawobj <- rawobj[,nac != nrow(rawobj)]

fixsheet <- function(sheet){
  sheet <- sheet %>% dplyr::mutate(
    right = `右侧颈动脉` %>% as.numeric(),
    left = `左侧颈动脉` %>% as.numeric(),
    left_level = case_when(
      is.na(left) ~ NA,
      left < 1 ~ "Normal",
      left < 1.5 ~ "Level1",
      T  ~ "Level2"
    ),
    left_level = factor(left_level, levels = c("Normal", "Level1", "Level2")),
    
    right_level = case_when(
      is.na(right) ~ NA,
      right < 1 ~ "Normal",
      right < 1.5 ~ "Level1",
      T ~ "Level2"
    ),
    right_level = factor(right_level, levels = c("Normal", "Level1", "Level2")),
    
    fatty_liver = `脂肪肝`,
    fatty_liver = case_when(
      fatty_liver == 5 ~ NA,
      T ~ fatty_liver
    ),
    
    gender = `XB`,
    gender_level = case_when(
      is.na(gender) ~ NA,
      gender == "女" ~ "Female",
      gender == "男" ~ "Male"
    ),
    
    age =  `NL` %>% str_remove("岁") %>% as.numeric(),
    age_level = case_when(
      is.na(age) ~ NA,
      age <= 30 ~ "<= 30",
      age <= 40 ~ "<= 40",
      age <= 50 ~ "<= 50",
      age <= 60 ~ "<= 60",
      age <= 70 ~ "<= 70",
      T ~ "> 70"
    ),
    
    systolic_blood_pressure =  `收缩压` %>% as.numeric(),
    systolic_blood_pressure_level = case_when(
      is.na(systolic_blood_pressure)  ~ NA,
      systolic_blood_pressure < 130 ~ 0,
      systolic_blood_pressure < 140 ~ 1,
      systolic_blood_pressure < 160  ~ 2,
      T ~ 3
    ),
    diastolic_pressure =  `舒张压` %>% as.numeric(),
    diastolic_pressure_level = case_when(
      is.na(diastolic_pressure) ~ NA,
      diastolic_pressure < 80 ~ 0,
      diastolic_pressure < 90 ~ 1,
      diastolic_pressure < 100 ~ 2,
      T ~ 3
    ),
    blood_pressure_level = case_when(
      is.na(systolic_blood_pressure) | is.na(diastolic_pressure) ~ NA,
      (systolic_blood_pressure < 130 & diastolic_pressure < 80) ~ 0,
      (systolic_blood_pressure < 140 & diastolic_pressure < 90) ~ 1,
      (systolic_blood_pressure < 160 & diastolic_pressure < 100) ~ 2,
      T ~ 3
    ),
    
    heart_rate = `心率` %>% as.numeric(),
    height = `身高` %>% as.numeric(),
    weight = `体重` %>% as.numeric(),
    
    bmi =  `体重指数` %>% as.numeric(),
    bmi_level = case_when(
      is.na(bmi) ~ NA,
      bmi < 24 ~ 0,
      bmi < 28 ~ 1,
      T ~ 2
    ),
    
    white_blood_cell_count =  `白细胞计数` %>% as.numeric(),
    white_blood_cell_count_level = case_when(
      is.na(white_blood_cell_count) ~ NA,
      white_blood_cell_count < 4 ~ 0,
      white_blood_cell_count < 10 ~ 1,
      T ~ 2
    ),
    
    neutrophils =  `中性粒细胞计数` %>% as.numeric(),
    neutrophils_level = case_when(
      is.na(neutrophils) ~ NA,
      neutrophils < 2 ~ 0,
      neutrophils < 7 ~ 1,
      T ~ 2
    ),
    
    neutrophil_percentage =  `中性粒细胞百分比` %>% as.numeric(),
    neutrophil_percentage_level = case_when(
      is.na(neutrophil_percentage) ~ NA,
      neutrophil_percentage < 50 ~ 0,
      neutrophil_percentage < 70 ~ 1,
      T ~ 2
    ),
    
    lymphocyte_percentage =  `淋巴细胞百分比` %>% as.numeric(),
    lymphocyte_percentage_level = case_when(
      is.na(lymphocyte_percentage) ~ NA,
      lymphocyte_percentage < 20 ~ 0,
      lymphocyte_percentage < 40 ~ 1,
      T ~ 2
    ),
    # monocyte_percentage =  `单核细胞MXD` %>% as.numeric(),
    # monocyte_percentage_level = case_when(
    #   is.na(monocyte_percentage) ~ NA,
    #   monocyte_percentage < 3 ~ 0,
    #   monocyte_percentage < 10 ~ 1,
    #   T ~ 2
    # ),
    hemoglobin =  `血红蛋白` %>% as.numeric(),
    hemoglobin_level = case_when(
      is.na(hemoglobin) ~ NA,
      hemoglobin < 113 ~ 0,
      hemoglobin < 151 ~ 1,
      T ~ 2
    ),
    
    platelet_count =  `血小板计数` %>% as.numeric(),
    platelet_count_level = case_when(
      is.na(platelet_count) ~ NA,
      platelet_count < 101 ~ 0,
      platelet_count < 320 ~ 1,
      T ~ 2
    ),
    platelet_volume =  `血小板压积` %>% as.numeric(),
    platelet_volume_level = case_when(
      is.na(platelet_volume) ~ NA,
      platelet_volume < 0.108 ~ 0,
      platelet_volume < 0.282 ~ 1,
      T ~ 2
    ),
    
    total_protein =  `总蛋白` %>% as.numeric(),
    total_protein_level = case_when(
      is.na(total_protein) ~ NA,
      total_protein < 65 ~ "Low",
      total_protein < 85 ~ "Normal",
      T ~ "High"
    ),
    total_protein_level = factor(total_protein_level, levels = c("Low", "Normal", "High")),
    
    albumin =  `白蛋白` %>% as.numeric(),
    albumin_level = case_when(
      is.na(albumin) ~ NA,
      albumin < 40 ~ "Low",
      albumin < 55 ~ "Normal",
      T ~ "High"
    ),
    albumin_level = factor(albumin_level, levels = c("Low", "Normal", "High")),
    
    alanine_aminotransferase =  `谷丙转氨酶` %>% as.numeric(),
    alanine_aminotransferase_level = case_when(
      is.na(alanine_aminotransferase) ~ NA,
      alanine_aminotransferase < 7 ~ "Low",
      alanine_aminotransferase < 40 ~ "Normal",
      T ~"High"
    ),
    alanine_aminotransferase_level = factor(alanine_aminotransferase_level, levels = c("Low", "Normal", "High")),
    
    aspartate_aminotransferase =  `谷草转氨酶` %>% as.numeric(),
    aspartate_aminotransferase_level = case_when(
      is.na(aspartate_aminotransferase) ~ NA,
      aspartate_aminotransferase < 13 ~ "Low",
      aspartate_aminotransferase < 35 ~ "Normal",
      T ~"High"
    ),
    aspartate_aminotransferase_level = factor(aspartate_aminotransferase_level, levels = c("Low", "Normal", "High")),
    
    creatinine =  `肌酐` %>% as.numeric(),
    creatinine_level = case_when(
      is.na(creatinine) ~ NA,
      creatinine < 41 ~ "Low",
      creatinine < 73 ~ "Normal",
      T ~"High"
    ),
    creatinine_level = factor(creatinine_level, levels = c("Low", "Normal", "High")),
    
    
    urea =  `尿素` %>% as.numeric(),
    urea_level = case_when(
      is.na(urea) ~ NA,
      urea < 2.6 ~ "Low",
      urea < 7.5 ~ "Normal",
      T ~"High"
    ),
    urea_level = factor(urea_level, levels = c("Low", "Normal", "High")),
    
    uric_acid =  `尿酸` %>% as.numeric(),
    uric_acid_level = case_when(
      is.na(uric_acid) ~ NA,
      uric_acid < 155 ~ "Low",
      uric_acid < 357 ~ "Normal",
      T ~"High"
    ),
    uric_acid_level = factor(uric_acid_level, levels = c("Low", "Normal", "High")),
    
    glomerular_filtration_rate =  `肾小球率过滤` %>% as.numeric(),
    glomerular_filtration_rate_level = case_when(
      is.na(glomerular_filtration_rate) ~ NA,
      glomerular_filtration_rate >= 90 ~ "G1",
      glomerular_filtration_rate >= 60 ~ "G2",
      glomerular_filtration_rate >= 45 ~ "G3a",
      glomerular_filtration_rate >= 30 ~ "G3b",
      glomerular_filtration_rate >= 15 ~ "G4",
      T ~ "G5"
    ),
    homocysteine =  `同型半胱氨酸` %>% as.numeric(),
    triglycerides =  `甘油三酯` %>% as.numeric(),
    triglycerides_level = case_when(
      is.na(triglycerides) ~ NA,
      triglycerides < 0.3 ~ "Low",
      triglycerides < 1.7 ~ "Normal",
      T ~ "High"
    ),
    triglycerides_level = factor(triglycerides_level, levels = c("Low", "Normal", "High")),
    
    TC =  `总胆固醇` %>% as.numeric(),
    TC_level = case_when(
      is.na(TC) ~ NA,
      TC < 3.14 ~ "Low",
      TC < 5.86 ~ "Normal",
      T ~ "High"
    ),
    TC_level = factor(TC_level, levels = c("Low", "Normal", "High")),
    
    HDL =  `高密度脂蛋白C` %>% as.numeric(),
    HDL_level = case_when(
      is.na(HDL) ~ NA,
      HDL < 0.88 ~ "Low",
      HDL < 2.04 ~ "Normal",
      T ~"High"
    ),
    HDL_level = factor(HDL_level, levels = c("Low", "Normal", "High")),
    
    LDL =  `低密度脂蛋白C` %>% as.numeric(),
    LDL_level = case_when(
      is.na(LDL) ~ NA,
      LDL < 1.8 ~ 0,
      LDL < 2.6 ~ 1,
      LDL < 3.4 ~ 2,
      LDL < 4.9 ~ 3,
      T ~ 4
    ),
    
    fasting_blood_sugar =  `空腹血糖` %>% as.numeric(),
    fasting_blood_sugar_level = case_when(
      is.na(fasting_blood_sugar) ~ NA,
      fasting_blood_sugar < 3.9 ~ "Low",
      fasting_blood_sugar < 6.1 ~ "Normal",
      T ~ "High"
    ),
    fasting_blood_sugar_level = factor(fasting_blood_sugar_level, levels = c("Low", "Normal", "High")),
    
    glycated_hemoglobinA1 =  `糖化血红蛋白` %>% as.numeric(),
    fasting_C_peptide =  `空腹C肽` %>% as.numeric(),
    NHDL = TC - HDL,
    NHDL_age = NHDL * age,
    LDL_age = LDL * age,
  )
  sheet <- sheet %>% rowwise() %>% mutate(
    max_data = max(left, right)
  ) %>% ungroup() %>% 
    mutate(
      max_level = case_when(
        is.na(max_data) ~ NA,
        max_data < 1 ~ "Normal",
        max_data < 1.5 ~ "Level1",
        T ~ "Level2"
      ) %>% factor(., levels = c("Normal", "Level1", "Level2")),
      IMT = max_data,
      IMT_level = max_level
    )
  sheet
}
rawobj <- fixsheet(rawobj)
out_put_dir <- "./clinical/tables"
write_rds(rawobj, file = glue("{out_put_dir}/newadd_test2.rds"))




