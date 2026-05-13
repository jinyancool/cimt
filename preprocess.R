pkgs <- c("fs", "configr", "jhtools", "glue", "tidyverse")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}/") %>% checkdir()
setwd(workdir)
# ================================ 第一部分 过滤离散化 ====================================
# 9136 个
rawobj <-  "/cluster/home/ztao_jh/projects/healthman/data/zhanglei/human/clinical/raw.rds" %>%
  read_rds() %>% mutate(
    yearx = year(TIJIANRQ),
    monthx = month(TIJIANRQ),
    dayx = day(TIJIANRQ)
  )
rawobj <- rawobj[rawobj$"是否采纳" == "采纳",]

fixsheet <- readxl::read_excel("/cluster/home/ztao_jh/projects/healthman/analysis/zhanglei/human/clinical/tables/fix/fix.xlsx", 
                               col_types = c("text", "text", "text", "guess", "numeric","text","text","numeric","numeric", 
                                             rep("guess", times = 75)))
for(i in fixsheet$"匹配"){
  rawobj[rawobj$"匹配" == i,c("右侧颈动脉", "左侧颈动脉" )] <- fixsheet[fixsheet$"匹配" == i,c("右侧颈动脉", "左侧颈动脉")]
}
newsheet <- readxl::read_excel("/cluster/home/ztao_jh/projects/healthman/analysis/zhanglei/human/clinical/tables/fix/2023-08-15.xls") %>% 
  dplyr::select("TIJIANKABM","TIJIANRQ","现服药情况","家族史","生活方式运动","生活方式睡眠" ) %>% 
  mutate(
    yearx = year(TIJIANRQ),
    monthx = month(TIJIANRQ),
    dayx = day(TIJIANRQ)
  ) %>% dplyr::select(-"TIJIANRQ")
rawobj <- rawobj %>% left_join(newsheet, by = c("TIJIANKABM","yearx", "monthx","dayx"))

nac <- apply(rawobj, 2, function(x) sum(is.na(x)))
rawobj <- rawobj[,nac != nrow(rawobj)]
rawobj <- rawobj %>% dplyr::select(-c("超声与体征时间差", "是否采纳", "XINGMING", "yearx", "monthx", "dayx"))

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
    
    drinking =  `饮酒`,
    drinking_level = case_when(
      is.na(drinking) ~ NA,
      drinking == "不饮" ~ "No Drinking",
      T ~ "Drinking"),
    
    smoking =  `吸烟`,
    smoking_level = case_when(
      is.na(smoking) ~ NA,
      smoking == "不吸" ~ "No Smoking",
      T ~ "Smoking"),
    
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
    
    abdominal_circumference =  `腹围` %>% as.numeric(),
    abdominal_circumference_level = case_when(
      is.na(abdominal_circumference) | is.na(gender) ~ NA,
      (abdominal_circumference < 90 & gender == "男") | (abdominal_circumference < 85 & gender == "女") ~ 0,
      T ~ 1
    ),
    
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
    
    neutrophils =  `中性粒细胞` %>% as.numeric(),
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
    monocyte_percentage =  `单核细胞百分比` %>% as.numeric(),
    monocyte_percentage_level = case_when(
      is.na(monocyte_percentage) ~ NA,
      monocyte_percentage < 3 ~ 0,
      monocyte_percentage < 10 ~ 1,
      T ~ 2
    ),
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
    
    urinary_microalbumin =  `尿微量白蛋白` %>% as.numeric(),
    urine_microalbuminuria_creatinine_ratio =  `尿微量白蛋白尿肌酐比值` %>% as.numeric(),
    
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
    
    glomerular_filtration_rate =  `肾小球滤过率` %>% as.numeric(),
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
      HDL < 0.9 ~ 0,
      HDL <= 1.19 ~ 1,
      HDL <= 1.29 ~ 2,
      HDL <= 1.6 ~ 3,
      HDL > 1.6 ~ 4
    ),
    # HDL_level = factor(HDL_level, levels = c("Low", "Normal", "High")),
    
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
    
    apolipoproteinA1 =  `载脂蛋白A1` %>% as.numeric(),
    apolipoproteinB =  `载脂蛋白B` %>% as.numeric(),
    apolipoproteinE =  `载脂蛋白E` %>% as.numeric(),
    glycated_hemoglobinA1 =  `糖化血红蛋白A1` %>% as.numeric(),
    glycated_hemoglobinA1C =  `糖化血红蛋白A1C` %>% as.numeric(),
    fasting_C_peptide =  `空腹C肽` %>% as.numeric(),
    fasting_insulin =  `空腹胰岛素` %>% as.numeric(),
    hsc_reactive_protein =  `超敏C反应蛋白` %>% as.numeric(),
    sialic_acid =  `唾液酸` %>% as.numeric(),
    free_fatty_acid =  `游离脂肪酸` %>% as.numeric(),
    Hydroxyvitamin_D3 =  `羟基维生素D3` %>% as.numeric(),
    
    glycated_hemoglobinA1_level = case_when(
      is.na(glycated_hemoglobinA1) ~ NA,
      glycated_hemoglobinA1 < 5.5 ~ 0,
      glycated_hemoglobinA1 < 6.1 ~ 1,
      glycated_hemoglobinA1 < 6.5 ~ 2,
      glycated_hemoglobinA1 < 7 ~ 3,
      T ~ 4
    ),
    
    glycated_hemoglobinA1C_level = case_when(
      is.na(glycated_hemoglobinA1C) ~ NA,
      glycated_hemoglobinA1C < 5.5 ~ 0,
      glycated_hemoglobinA1C < 6.1 ~ 1,
      glycated_hemoglobinA1C < 6.5 ~ 2,
      glycated_hemoglobinA1C < 7 ~ 3,
      T ~ 4
    ),
    
    NHDL = TC - HDL,
    NHDL_level = case_when(
      is.na(NHDL) ~ NA,
      NHDL < 2.0 ~ 0,
      NHDL < 2.6 ~ 1,
      NHDL < 3.4 ~ 2,
      NHDL < 4.2 ~ 3,
      T ~ 4
    ),
    
    NHDL_age = NHDL * age,
    LDL_age = LDL * age,
    
    history_heart_brain = str_detect(`家族史`,pattern = "心脑血管疾病"),
    history_diabetes = str_detect(`家族史`,pattern = "糖尿病"),
    history_cancer = str_detect(`家族史`,pattern = "恶性肿瘤"),
    history_heart = str_detect(`家族史`,pattern = "心血管疾病"),
    history_sleep = str_detect(`家族史`,pattern = "睡眠障碍"),
    history_mental = str_detect(`家族史`,pattern = "精神障碍"),
    history_no = str_detect(`家族史`,pattern = "无异常"),
    spots = case_when(
      `生活方式运动` == "未达到" ~ 0,
      `生活方式运动` == "达到" ~ 1
    ),
    sleep = case_when(
      `生活方式睡眠` == "少" ~ 0,
      `生活方式睡眠` == "较少" ~ 1,
      `生活方式睡眠` == "一般" ~ 2,
      `生活方式睡眠` == "好" ~ 3,
      `生活方式睡眠` == "较好" ~ 4
    ),
    drug_blood_pressure = str_detect(`现服药情况`,pattern = "高血压"),
    drug_sugar = str_detect(`现服药情况`,pattern = "降糖药"),
    drug_lipid = str_detect(`现服药情况`,pattern = "降脂药|他汀"),
    drug_thyroid = str_detect(`现服药情况`,pattern = "优甲乐|甲状腺|甲亢"),
    drug_heart = str_detect(`现服药情况`,pattern = "心脏病|心肌病|冠心"),
    drug_vessel = str_detect(`现服药情况`,pattern = "血管|血栓|活血|补血|抗凝"),
    drug_gout = str_detect(`现服药情况`,pattern = "尿酸|痛风"),
    drug_depress = str_detect(`现服药情况`,pattern = "抑郁|黛力新"),
    drug_anxiety = str_detect(`现服药情况`,pattern = "焦虑|黛力新"),
    drug_sleep = str_detect(`现服药情况`,pattern = "安眠")
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
rawobj[is.na(rawobj$right),]$"右侧颈动脉" <- c("0.4", "0.5")
rawobj[is.na(rawobj$systolic_blood_pressure),]$"收缩压"[34] <- 130
rawobj[is.na(rawobj$diastolic_pressure),]$"舒张压"[34] <- 89
rawobj[is.na(rawobj$heart_rate),]$"心率"[34] <- 58
rawobj[is.na(rawobj$height),]$"身高"[88] <- 171
rawobj[is.na(rawobj$weight),]$"体重"[88] <- 84.7
rawobj[is.na(rawobj$abdominal_circumference),]$"腹围"[!is.na(rawobj[is.na(rawobj$abdominal_circumference),]$"腹围")] <- c(91, 65)
rawobj[is.na(rawobj$bmi),]$"体重指数"[!is.na(rawobj[is.na(rawobj$bmi),]$"体重指数")][41] <- 28.8
rawobj[is.na(rawobj$platelet_count),]$"血小板计数"[!is.na(rawobj[is.na(rawobj$platelet_count),]$"血小板计数")][2] <- 63
rawobj[is.na(rawobj$hsc_reactive_protein),]$"超敏C反应蛋白"[!is.na(rawobj[is.na(rawobj$hsc_reactive_protein),]$"超敏C反应蛋白")] <- 0
rawobj[is.na(rawobj$Hydroxyvitamin_D3),]$"羟基维生素D3"[!is.na(rawobj[is.na(rawobj$Hydroxyvitamin_D3),]$"羟基维生素D3")] <- 200
rawobj <- fixsheet(rawobj)
rawobj <- rawobj[!is.na(rawobj$right) & !is.na(rawobj$left),]
# 9135

framingham <- rawobj[,c("smoking_level", "age", "gender_level","systolic_blood_pressure","HDL","TC","drug_blood_pressure")] %>% 
  mutate(framingham_age = case_when(
    is.na(gender_level) | is.na(age) ~ NA,
    gender_level == "Male" & age <= 34 ~ 0,
    gender_level == "Male" & age <= 39 ~ 2,
    gender_level == "Male" & age <= 44 ~ 5,
    gender_level == "Male" & age <= 49 ~ 7,
    gender_level == "Male" & age <= 54 ~ 8,
    gender_level == "Male" & age <= 59 ~ 10,
    gender_level == "Male" & age <= 64 ~ 11,
    gender_level == "Male" & age <= 69 ~ 12,
    gender_level == "Male" & age <= 74 ~ 14,
    gender_level == "Male" & age > 74 ~ 15,
    gender_level == "Female" & age <= 34 ~ 0,
    gender_level == "Female" & age <= 39 ~ 2,
    gender_level == "Female" & age <= 44 ~ 4,
    gender_level == "Female" & age <= 49 ~ 5,
    gender_level == "Female" & age <= 54 ~ 7,
    gender_level == "Female" & age <= 59 ~ 8,
    gender_level == "Female" & age <= 64 ~ 9,
    gender_level == "Female" & age <= 69 ~ 10,
    gender_level == "Female" & age <= 74 ~ 11,
    gender_level == "Female" & age > 74 ~ 12
  ),
  framingham_HDL = case_when(
    is.na(HDL) ~ NA,
    HDL < 0.9 ~ 2,
    HDL <= 1.19 ~ 1,
    HDL <= 1.29 ~ 0,
    HDL <= 1.6 ~ -1,
    HDL > 1.6 ~ -2
  ),
  framingham_TC = case_when(
    is.na(gender_level) | is.na(TC) ~ NA,
    gender_level == "Male" & TC < 4.1 ~ 0,
    gender_level == "Male" & TC <= 5.19 ~ 1,
    gender_level == "Male" & TC <= 6.19 ~ 2,
    gender_level == "Male" & TC <= 7.2 ~ 3,
    gender_level == "Male" & TC > 7.2 ~ 4,
    
    gender_level == "Female" & TC < 4.1 ~ 0,
    gender_level == "Female" & TC <= 5.19 ~ 1,
    gender_level == "Female" & TC <= 6.19 ~ 3,
    gender_level == "Female" & TC <= 7.2 ~ 4,
    gender_level == "Female" & TC > 7.2 ~ 5
  ),
  framingham_smoker = case_when(
    is.na(gender_level) | is.na(smoking_level) ~ NA,
    gender_level == "Male" & smoking_level == "Smoking" ~ 4,
    gender_level == "Female" & smoking_level == "Smoking" ~ 3,
    gender_level == "Male" & smoking_level == "No Smoking" ~ 0,
    gender_level == "Female" & smoking_level == "No Smoking" ~ 0
  ),
  framingham_systolic_blood_pressure = case_when(
    is.na(gender_level) | is.na(systolic_blood_pressure) | is.na(drug_blood_pressure) ~ NA,
    gender_level == "Male" & systolic_blood_pressure < 120 & !drug_blood_pressure ~ -2,
    gender_level == "Male" & systolic_blood_pressure <= 129 & !drug_blood_pressure ~ 0,
    gender_level == "Male" & systolic_blood_pressure <= 139 & !drug_blood_pressure ~ 1,
    gender_level == "Male" & systolic_blood_pressure <= 149 & !drug_blood_pressure ~ 2,
    gender_level == "Male" & systolic_blood_pressure <= 159 & !drug_blood_pressure ~ 2,
    gender_level == "Male" & systolic_blood_pressure > 159 & !drug_blood_pressure ~ 3,
    
    gender_level == "Male" & systolic_blood_pressure < 120 & drug_blood_pressure ~ 0,
    gender_level == "Male" & systolic_blood_pressure <= 129 & drug_blood_pressure ~ 2,
    gender_level == "Male" & systolic_blood_pressure <= 139 & drug_blood_pressure ~ 3,
    gender_level == "Male" & systolic_blood_pressure <= 149 & drug_blood_pressure ~ 4,
    gender_level == "Male" & systolic_blood_pressure <= 159 & drug_blood_pressure ~ 4,
    gender_level == "Male" & systolic_blood_pressure > 159 & drug_blood_pressure ~ 5,
    
    gender_level == "Female" & systolic_blood_pressure < 120 & !drug_blood_pressure  ~ - 3,
    gender_level == "Female" & systolic_blood_pressure <= 129 & !drug_blood_pressure ~ 0,
    gender_level == "Female" & systolic_blood_pressure <= 139 & !drug_blood_pressure ~ 1,
    gender_level == "Female" & systolic_blood_pressure <= 149 & !drug_blood_pressure ~ 2,
    gender_level == "Female" & systolic_blood_pressure <= 159 & !drug_blood_pressure ~ 4,
    gender_level == "Female" & systolic_blood_pressure > 159 & !drug_blood_pressure  ~ 5,
    
    gender_level == "Female" & systolic_blood_pressure < 120 & drug_blood_pressure  ~ - 1,
    gender_level == "Female" & systolic_blood_pressure <= 129 & drug_blood_pressure ~ 2,
    gender_level == "Female" & systolic_blood_pressure <= 139 & drug_blood_pressure ~ 3,
    gender_level == "Female" & systolic_blood_pressure <= 149 & drug_blood_pressure ~ 5,
    gender_level == "Female" & systolic_blood_pressure <= 159 & drug_blood_pressure ~ 6,
    gender_level == "Female" & systolic_blood_pressure > 159 & drug_blood_pressure  ~ 7
  )) %>% rowwise() %>% mutate(framingham_total = sum(c_across(starts_with("framingham"))))

out_put_dir <- "./clinical/tables"
checkdir(out_put_dir)
write_rds(rawobj, file = glue::glue("{out_put_dir}/raw_data.rds"))
write_rds(framingham, file = glue::glue("{out_put_dir}/framingham_data.rds"))




