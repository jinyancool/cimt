pkgs <- c("fs", "configr", "ggthemes", "jhtools", "glue", "patchwork", "tidyverse", "ggpubr")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}") %>% checkdir()
setwd(workdir)
in_dir <- "clinical/figures/fig3/line_plot" %>% checkdir
out_dir <- "clinical/figures/fig3/line_plot_make_fig" %>% checkdir
config_fn <- glue("./clinical/configs/configs.yaml")
gender_col <- show_me_the_colors(config_fn, iterm = "gender")
level_col <- show_me_the_colors(config_fn, iterm = "level")
vertical_line_age_col <- show_me_the_colors(config_fn, iterm = "vertical_line_age")
cor_col <- show_me_the_colors(config_fn, iterm = "cor")
plist1 <- read_rds(glue("{in_dir}/plist_70.rds"))
plist2 <- read_rds(glue("{in_dir}/plist_80.rds"))


pdf(glue("{out_dir}/line_figA.pdf"), width = 20, height = 20)
print(
  (plist1[["age"]] | plist1[["glycated_hemoglobinA1"]] | plist1[["glycated_hemoglobinA1C"]])/
    (plist1[["systolic_blood_pressure"]] | plist1[["diastolic_pressure"]] | plist1[["blood_pressure_level"]])/
    (plist1[["abdominal_circumference"]] | plist1[["bmi"]] | plist1[["triglycerides"]])/
    (plist1[["NHDL"]] | plist1[["HDL"]] | plist1[["LDL"]] )
)
dev.off()

pdf(glue("{out_dir}/line_figB.pdf"), width = 24, height = 5)
print(
  (plist2[["white_blood_cell_count"]] | plist2[["neutrophils"]] | plist2[["neutrophil_percentage"]] | plist2[["lymphocyte_percentage"]])
)
dev.off()
