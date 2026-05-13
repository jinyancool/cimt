pkgs <- c("fs", "configr", "ggthemes", "jhtools", "glue", "patchwork", "tidyverse", "ggpubr")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}") %>% checkdir()
setwd(workdir)

make_pth <- "./clinical/figures/fig1/make_fig" %>% checkdir()
feature_barplot_fill_face <- "./clinical/figures/fig1/total_bar/feature_barplot_fill_face.rds" %>% read_rds()
age_gender_face <- "./clinical/figures/fig1/gender_age/age_gender.rds" %>% read_rds()

p1 <- (feature_barplot_fill_face[["systolic_blood_pressure_level"]] + guides(fill = "none") + labs(tag = "A")) +
  (feature_barplot_fill_face[["diastolic_pressure_level"]] + guides(fill = "none")) +
  (feature_barplot_fill_face[["blood_pressure_level"]]) + 
  plot_layout(guides='collect',ncol = 1) & theme(legend.position='bottom', plot.caption = element_blank()) 
p2 <- (feature_barplot_fill_face[["LDL_level"]] + guides(fill = "none") + labs(tag = "B")) +
  (feature_barplot_fill_face[["HDL_level"]] + guides(fill = "none")+
     scale_x_discrete(guide = guide_axis(n.dodge = 2))) +
  (feature_barplot_fill_face[["fatty_liver"]]) + 
  plot_layout(guides='collect',ncol = 1) & theme(legend.position='bottom', plot.caption = element_blank()) 
figup <- p1 | p2 
p3 <- (feature_barplot_fill_face[["abdominal_circumference_level"]] + guides(fill = "none") + labs(tag = "C")) +
  (feature_barplot_fill_face[["bmi_level"]])+ 
  plot_layout(guides='collect',ncol = 1) & theme(legend.position='bottom', plot.caption = element_blank()) 
p4 <- (feature_barplot_fill_face[["white_blood_cell_count_level"]] + guides(fill = "none") + labs(tag = "D")) +
  (feature_barplot_fill_face[["glycated_hemoglobinA1C_level"]]) +
  plot_layout(guides='collect',ncol = 1) & theme(legend.position='bottom', plot.caption = element_blank()) 
figdn <- p3 | p4
full_fug <- figup / figdn + 
  plot_layout(heights = c(3,2))  

pdf(glue("{make_pth}/face_bar.pdf"), width = 12, height = 17)
print(full_fug)
dev.off()





p <- (age_gender_face[["abdominal_circumference"]] + guides(fill = "none")) +
  (age_gender_face[["bmi"]] + guides(fill = "none")) + 
  (age_gender_face[["systolic_blood_pressure"]] + guides(fill = "none")) +
  (age_gender_face[["diastolic_pressure"]] + guides(fill = "none")) +
  (age_gender_face[["glycated_hemoglobinA1"]] + guides(fill = "none")) +
  (age_gender_face[["glycated_hemoglobinA1C"]] + guides(fill = "none")) + 
  (age_gender_face[["NHDL"]] + guides(fill = "none")) +
  (age_gender_face[["LDL"]] + guides(fill = "none")) +
#  (age_gender_face[["HDL"]] + guides(fill = "none")) +
#  (age_gender_face[["TC"]] + guides(fill = "none")) +
#  (age_gender_face[["triglycerides"]] + guides(fill = "none")) + 
#  (age_gender_face[["urea"]] + guides(fill = "none")) +
#  (age_gender_face[["uric_acid"]] + guides(fill = "none")) + 
#  (age_gender_face[["white_blood_cell_count"]] + guides(fill = "none")) +
#  (age_gender_face[["neutrophils"]] + guides(fill = "none")) +
#  (age_gender_face[["neutrophil_percentage"]] + guides(fill = "none")) +
#  (age_gender_face[["lymphocyte_percentage"]] + guides(fill = "none")) +
  plot_annotation(tag_levels = c('A'))+ 
  plot_layout(guides='collect',ncol = 2) & theme(legend.position='bottom', plot.caption = element_blank()) 

pdf(glue("{make_pth}/face_age_gender.pdf"), width = 18, height = 20)
print(p)
dev.off()


