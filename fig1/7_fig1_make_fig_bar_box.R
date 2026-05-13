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
box_pth <- "./clinical/figures/fig1/total_box/level_boxplot.rds"
barsingle_pth <- "./clinical/figures/fig1/total_bar/feature_barplot_fill.rds"
barface_pth <- "./clinical/figures/fig1/total_bar/feature_barplot_fill_face.rds" 
loLDL_hiMax_bar_pth <- "./clinical/figures/fig1/highmax_lowldl/loLDL_hiMax_bar_new_lable.rds"
age_drug_pth <- "./clinical/figures/fig1/drug_age/drug_age_50.rds"
age_drug_pth2 <- "./clinical/figures/fig1/drug_age/drug_age_60.rds"

rds_fn <- "clinical/tables/raw_data.rds"
rawobj <- read_rds(rds_fn)
config_fn <- glue("clinical/configs/configs.yaml")
gender_col <- show_me_the_colors(config_fn, iterm = "gender")
level_col <- show_me_the_colors(config_fn, iterm = "level")
vertical_line_age_col <- show_me_the_colors(config_fn, iterm = "vertical_line_age")

box_plot <- read_rds(box_pth)
barsingle_plot <- read_rds(barsingle_pth)
barface_plot <- read_rds(barface_pth)
lldl_hmax_bar_plot <- read_rds(loLDL_hiMax_bar_pth)
age_drug_plot <- read_rds(age_drug_pth)
age_drug_plot2 <- read_rds(age_drug_pth2)

pdf(glue("{make_pth}/fig1.A.pdf"), width = 8, height = 6)
print(
  (box_plot[["age"]] + guides(fill = "none")) + barsingle_plot[["gender_level"]] +
    plot_annotation(tag_levels = c('A')) + 
    plot_layout(guides='collect', widths = c(2, 1.3))  & theme(legend.position='bottom', plot.caption = element_blank()) 
)
dev.off()

pdf(glue("{make_pth}/fig1.B.pdf"), width = 12, height = 6)
print(
  (box_plot[["systolic_blood_pressure"]] + guides(fill = "none")) +
    (box_plot[["diastolic_pressure"]] + guides(fill = "none")) + 
    barsingle_plot[["blood_pressure_level"]] +
    plot_annotation(tag_levels = c('A')) + 
    plot_layout(guides='collect', widths = c(2, 2, 1.3))  & theme(legend.position='bottom', plot.caption = element_blank()) 
)
dev.off()

pdf(glue("{make_pth}/fig1.C.pdf"), width = 13, height = 6)
print(
  (box_plot[["HDL"]] + guides(fill = "none")) +
    (box_plot[["LDL"]] + guides(fill = "none")) + 
    (box_plot[["NHDL"]] + guides(fill = "none")) + 
    (barsingle_plot[["fatty_liver"]] + guides(fill = "none")) +
    (barsingle_plot[["HDL_level"]] + guides(fill = "none")) +
    (barsingle_plot[["LDL_level"]] + guides(fill = "none")) +
    (barsingle_plot[["NHDL_level"]] + guides(fill = "none")) +
    (barsingle_plot[["triglycerides_level"]]) +
    plot_annotation(tag_levels = c('A')) + 
    plot_layout(ncol = 4, guides='collect', widths = c(2, 2, 2, 2))  & theme(legend.position='bottom', plot.caption = element_blank()) 
)
dev.off()

pdf(glue("{make_pth}/fig1.D.pdf"), width = 12, height = 9)
print(
  (barsingle_plot[["drug_blood_pressure"]] + guides(fill = "none")) +
    (barsingle_plot[["drug_sugar"]] + guides(fill = "none")) +
    (barsingle_plot[["drug_lipid"]] + guides(fill = "none")) +
    (lldl_hmax_bar_plot[["drug_blood_pressure"]]+ guides(fill = "none")) +
    (lldl_hmax_bar_plot[["drug_sugar"]]+ guides(fill = "none")) +
    (lldl_hmax_bar_plot[["drug_lipid"]] + guides(fill = guide_legend(title = "drug_used"))) +
    (lldl_hmax_bar_plot[["glycated_hemoglobinA1_level"]]+ guides(fill = "none")) +
    (lldl_hmax_bar_plot[["glycated_hemoglobinA1C_level"]]) +
    (barsingle_plot[["smoking_level"]]+ guides(fill = "none")) +
    plot_annotation(tag_levels = c('A')) + 
    plot_layout(ncol = 3, guides='collect', widths = c(2, 2, 2))  & theme(legend.position ='bottom', plot.caption = element_blank()) 
)
dev.off()

pdf(glue("{make_pth}/fig1.E.pdf"), width = 12, height = 6)
print(
  (lldl_hmax_bar_plot[["fatty_liver"]]+ guides(fill = "none")) +
    (lldl_hmax_bar_plot[["abdominal_circumference_level"]] + guides(fill = "none")) +
    (lldl_hmax_bar_plot[["bmi_level"]]) +
    plot_annotation(tag_levels = c('A')) + 
    plot_layout(ncol = 3, guides='collect', widths = c(2, 2, 2))  & theme(legend.position ='bottom', plot.caption = element_blank()) 
)
dev.off()

pdf(glue("{make_pth}/fig1.F.pdf"), width = 9, height = 10)
print(
  (box_plot[["glycated_hemoglobinA1"]] + guides(fill = "none")) +
    (box_plot[["glycated_hemoglobinA1C"]] + guides(fill = "none")) + 
    (barsingle_plot[["glycated_hemoglobinA1_level"]] + guides(fill = "none"))+
    (barsingle_plot[["glycated_hemoglobinA1C_level"]])+
    plot_annotation(tag_levels = c('A')) + 
    plot_layout(ncol = 2, guides='collect', widths = c(2, 2))  & theme(legend.position='bottom', plot.caption = element_blank()) 
)
dev.off()

pdf(glue("{make_pth}/fig1.G.pdf"), width = 8, height = 8)
print(
  (box_plot[["bmi"]] + guides(fill = "none")) +
    (box_plot[["abdominal_circumference"]] + guides(fill = "none")) + 
    (barsingle_plot[["bmi_level"]] + guides(fill = "none")) +
    (barsingle_plot[["abdominal_circumference_level"]]) +
    plot_annotation(tag_levels = c('A')) + 
    plot_layout(ncol = 2, guides='collect', widths = c(2, 2))  & theme(legend.position='bottom', plot.caption = element_blank()) 
)
dev.off()

pdf(glue("{make_pth}/fig1.age_drug50.pdf"), width = 8, height = 6)
print((age_drug_plot[["drug_blood_pressure"]] + guides(fill = "none")) + 
  (age_drug_plot[["drug_sugar"]] + guides(fill = "none")) + 
  age_drug_plot[["drug_lipid"]] +
  plot_annotation(tag_levels = c('A')) + 
  plot_layout(ncol = 3, guides='collect')  & theme(legend.position='bottom', plot.caption = element_blank())
)
dev.off()

pdf(glue("{make_pth}/fig1.age_drug60.pdf"), width = 8, height = 6)
print((age_drug_plot2[["drug_blood_pressure"]] + guides(fill = "none")) + 
        (age_drug_plot2[["drug_sugar"]] + guides(fill = "none")) + 
        age_drug_plot2[["drug_lipid"]] +
  plot_annotation(tag_levels = c('A')) + 
  plot_layout(ncol = 3, guides='collect')  & theme(legend.position='bottom', plot.caption = element_blank()) 
)
dev.off()
