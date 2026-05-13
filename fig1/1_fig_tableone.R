pkgs <- c("fs", "futile.logger", "configr", "stringr", "ggpubr", "ggthemes", "factoextra", "cluster",
          "jhtools", "glue", "ggsci", "patchwork", "tidyverse", "dplyr", "Seurat", "ComplexHeatmap",
          "ggpointdensity", "ggpubr","tableone")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}") %>% checkdir()
setwd(workdir)
"./clinical/figures/fig1/total_bar" %>% checkdir()
in_put_dir <- "clinical/tables"
rawobj <- read_rds(glue("{in_put_dir}/raw_data.rds"))
config_fn <- glue("~/projects/{project}/code/{dataset}/human/clinical/configs/configs.yaml")
gender_col <- show_me_the_colors(config_fn, iterm = "gender")
level_col <- show_me_the_colors(config_fn, iterm = "level")
vertical_line_age_col <- show_me_the_colors(config_fn, iterm = "vertical_line_age")

feature_level_table_pth <- checkdir("./clinical/figures/fig1/table/feature_level_table")
summary_pth <- checkdir("./clinical/figures/fig1/table/summary")
# ========================== feature level mean ================================
level_v <- c(
  "age_level","gender_level",
  "systolic_blood_pressure_level", "diastolic_pressure_level", # 血压
  "systolic_blood_pressure", "diastolic_pressure", # 血压
  "abdominal_circumference_level", "bmi_level", # 腹围bmi
  "abdominal_circumference", "bmi", # 腹围bmi
  "white_blood_cell_count_level","neutrophils_level","neutrophil_percentage_level", # 血
  "white_blood_cell_count","neutrophils","neutrophil_percentage", # 血
  "lymphocyte_percentage_level","monocyte_percentage_level",
  "lymphocyte_percentage","monocyte_percentage",
  "hemoglobin_level","platelet_count_level","platelet_volume_level","total_protein_level",
  "hemoglobin","platelet_count","platelet_volume","total_protein",
  "albumin_level","alanine_aminotransferase_level", "aspartate_aminotransferase_level", # 总蛋白 转氨酶
  "albumin","alanine_aminotransferase", "aspartate_aminotransferase", # 总蛋白 转氨酶
  "creatinine_level", "urea_level","uric_acid_level", # 肌酐尿素尿酸
  "creatinine", "urea","uric_acid", # 肌酐尿素尿酸
  "glomerular_filtration_rate_level", "triglycerides_level", # 肾小球滤过率  甘油三酯
  "glomerular_filtration_rate", "triglycerides", # 肾小球滤过率  甘油三酯
  "TC_level","HDL_level", # 总胆固醇 HDL
  "TC","HDL", # 总胆固醇 HDL
  "LDL_level","fasting_blood_sugar_level", # LDL 空腹血糖
  "LDL","fasting_blood_sugar" # LDL 空腹血糖
)
barsub1_level_frame <- rawobj[, level_v]


vs <- level_v[-c(1,2)] %>% str_remove("_level$") %>% unique()
for(v in vs){
  tmp <- barsub1_level_frame %>% dplyr::select(all_of(c(v, paste0(v,"_level"), "age_level", "gender_level"))) %>% 
    na.omit() %>% 
    group_by(.data[[paste0(v,"_level")]], age_level, gender_level) %>% 
    summarise(mean = mean(.data[[v]]), sample_size = n()) %>% 
    as.data.frame()
  write_csv(tmp, file = glue("{feature_level_table_pth}/feature_table_{v}.csv"))
}

# ========================== final 1 ===========================================
level_v <- c(
  "IMT_level",
  "gender_level", # 性别
  "age_level", # 年龄
  "height","weight", "bmi_level", # 升高 体重 bmi
  "abdominal_circumference", # 腹围
  "drinking_level","smoking_level","spots", # 抽烟 喝酒 运动
  "systolic_blood_pressure_level", "diastolic_pressure_level","blood_pressure_level", # 血压
  "heart_rate","fasting_blood_sugar_level", # 心率 空腹血糖
  "triglycerides_level","TC_level", "HDL_level","LDL_level",  #  甘油三酯 总胆固醇 HDL LDL
  "creatinine_level", # 肌酐
  "urea_level","uric_acid_level", # 尿素 尿酸
  "glomerular_filtration_rate_level", # 肾小球滤过率
  "fatty_liver", # 脂肪肝
  "alanine_aminotransferase_level", "aspartate_aminotransferase_level", #  转氨酶
  "hemoglobin_level","white_blood_cell_count_level","neutrophils_level", # 血
  "neutrophil_percentage_level", "lymphocyte_percentage_level","monocyte_percentage_level",
  "platelet_count_level","platelet_volume_level","total_protein_level","albumin_level",
  "history_heart_brain","history_diabetes","history_cancer","history_heart",
  "history_sleep","history_mental","history_no","sleep",
  "drug_blood_pressure","drug_sugar","drug_lipid","drug_thyroid","drug_heart","drug_vessel","drug_gout",
  "drug_depress","drug_anxiety","drug_sleep"
)

barsub1_level_frame <- rawobj[, level_v]
barsub1_level_frame$IMT_level <- factor(barsub1_level_frame$IMT_level, levels = c("Normal", "Level1", "Level2"))

## Vector of variables to summarize
myVars <- level_v[-1]
## Vector of categorical variables that need transformation
catVars <- c(
  "gender_level", # 性别
  "age_level", # 年龄
  "bmi_level", # 升高 体重 bmi
  "drinking_level","smoking_level","spots", # 抽烟 喝酒 运动
  "systolic_blood_pressure_level", "diastolic_pressure_level","blood_pressure_level", # 血压
  "fasting_blood_sugar_level", # 心率 空腹血糖
  "triglycerides_level","TC_level", "HDL_level","LDL_level",  #  甘油三酯 总胆固醇 HDL LDL
  "creatinine_level", # 肌酐
  "urea_level","uric_acid_level", # 尿素 尿酸
  "glomerular_filtration_rate_level", # 肾小球滤过率
  "fatty_liver", # 脂肪肝
  "alanine_aminotransferase_level", "aspartate_aminotransferase_level", #  转氨酶
  "hemoglobin_level","white_blood_cell_count_level","neutrophils_level", # 血
  "neutrophil_percentage_level", "lymphocyte_percentage_level","monocyte_percentage_level",
  "platelet_count_level","platelet_volume_level","total_protein_level","albumin_level",
  "history_heart_brain","history_diabetes","history_cancer","history_heart",
  "history_sleep","history_mental","history_no","sleep",
  "drug_blood_pressure","drug_sugar","drug_lipid","drug_thyroid","drug_heart","drug_vessel","drug_gout",
  "drug_depress","drug_anxiety","drug_sleep"
)
## Create a TableOne object
tab_level <- CreateTableOne(vars = myVars, data = barsub1_level_frame, factorVars = catVars, strata = "IMT_level")
tab_levelmat <- print(tab_level,  quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab_levelmat, file = glue("{summary_pth}/total_table1.csv"))
# ========================== final 2 ========================================
level_v <- c(
  "IMT",
  "age_level", # 年龄
  "gender_level", # 性别
  "drinking_level","smoking_level", # 抽烟 喝酒 
  "bmi_level", "abdominal_circumference_level",   # 升高 体重  腹围 bmi
  "blood_pressure_level", # 血压
  "fasting_blood_sugar_level", # 心率 空腹血糖
  "triglycerides_level","TC_level", "HDL_level","LDL_level",  #  甘油三酯 总胆固醇 HDL LDL
  "creatinine_level", # 肌酐
  "urea_level","uric_acid_level", # 尿素 尿酸
  "white_blood_cell_count_level", # 血
  "neutrophil_percentage_level", "lymphocyte_percentage_level","monocyte_percentage_level"
)

barsub1_level_frame <- rawobj[, level_v] %>% ungroup()

i <- "gender_level"
table2 <- lapply(c(
  "gender_level", # 性别
  "drinking_level","smoking_level", # 抽烟 喝酒 
  "bmi_level", "abdominal_circumference_level",   # 升高 体重  腹围 bmi
  "blood_pressure_level", # 血压
  "fasting_blood_sugar_level", # 心率 空腹血糖
  "triglycerides_level","TC_level", "HDL_level","LDL_level",  #  甘油三酯 总胆固醇 HDL LDL
  "creatinine_level", # 肌酐
  "urea_level","uric_acid_level", # 尿素 尿酸
  "white_blood_cell_count_level", # 血
  "neutrophil_percentage_level", "lymphocyte_percentage_level","monocyte_percentage_level"
), function(i){
  add_line <- tibble(
    age_level =  barsub1_level_frame$age_level %>% unique,
    "{i}" := i,
    value = NA
  )
  
  sub_barsub1_level_frame <- barsub1_level_frame %>% dplyr::select(all_of(c("IMT", "age_level", i))) %>% 
    group_by(age_level, !!sym(i)) %>% 
    summarise(mean = mean(IMT, na.rm = T), sd = sd(IMT, na.rm = T), value = paste0(signif(mean, 3), "±", signif(sd,3)) %>% as.character()) %>% 
    na.omit() %>% dplyr::select(-all_of(c("mean", "sd"))) %>% 
    ungroup() %>% rbind(add_line, .) %>% 
    pivot_wider(values_from = "value", names_from = "age_level")
  colnames(sub_barsub1_level_frame)[1] <- "feature"
  sub_barsub1_level_frame
}) %>% list_rbind()

for(i in 1:nrow(table2)){
  if(sum(is.na(table2[i,])) == (ncol(table2) - 1)){
    next
  }else{
    table2[i,][is.na(table2[i,])] <- "--"
  }
}
write.csv(table2, file = glue("{summary_pth}/total_table2.csv"), row.names = F, na = "")

# =================== 离散指标单做 =============================================
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
  "history_heart_brain","history_diabetes","history_cancer","history_heart",
  "history_sleep","history_mental","history_no","spots","sleep","drug_blood_pressure",
  "drug_sugar","drug_lipid","drug_thyroid","drug_heart","drug_vessel","drug_gout",
  "drug_depress","drug_anxiety","drug_sleep"
)
barsub1_level_frame <- rawobj[, level_v]
barsub1_level_frame$IMT_level <- factor(barsub1_level_frame$IMT_level, levels = c("Normal", "Level1", "Level2"))

myVars <- level_v[-1]
catVars <- level_v[-1]
tab_level <- CreateTableOne(vars = myVars, data = barsub1_level_frame, factorVars = catVars, strata = "IMT_level")
tab_levelmat <- print(tab_level,  quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab_levelmat, file = glue("{summary_pth}/total_levelmat.csv"))

# ======================= 连续型单做 ===========================================
nolevel <- c(
  "IMT_level",
  "gender_level","age_level", "age", # 性别 年龄
  "IMT",
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
  "NHDL_age", "LDL_age" # NHDL*age LDL*age
)
barsub1_level_frame <- rawobj[, nolevel]
barsub1_level_frame$IMT_level <- factor(barsub1_level_frame$IMT_level, levels = c("Normal", "Level1", "Level2"))

myVars <- nolevel[-1]
catVars <- nolevel[2:3]
tab_numeric <- CreateTableOne(vars = myVars, data = barsub1_level_frame, factorVars = catVars, strata = "IMT_level")
tab_numericmat <- print(tab_numeric, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab_numericmat, file = glue("{summary_pth}/total_numericmat.csv"))



