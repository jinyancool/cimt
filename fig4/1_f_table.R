pkgs <- c("fs", "futile.logger", "configr", "stringr", "ggpubr", "ggthemes", "factoextra", "cluster",
          "jhtools", "glue", "ggsci", "patchwork", "tidyverse", "dplyr", "Seurat", "ComplexHeatmap",
          "ggpointdensity", "ggpubr")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}/") %>% checkdir()
setwd(workdir)
in_put_dir <- "./clinical/tables"
rawobj <- read_rds(glue::glue("{in_put_dir}/raw_data.rds"))
framingham <- read_rds(glue::glue("{in_put_dir}/framingham_data.rds"))

out_dir <- "./clinical/figures/fig4/framingham" %>% checkdir
draw_df <- tibble(
  framingham_total = framingham$framingham_total,
  max_data = rawobj$max_data,
  gender_level = rawobj$gender_level,
  age = rawobj$age
) %>% na.omit() %>% dplyr::filter(age <= 70.5)
rawobj <- rawobj %>% dplyr::filter(age <= 70.5)

ftmale <- draw_df$gender_level == "Male"
ftfemale <- draw_df$gender_level == "Female"
c_age_male <- cor.test(draw_df$max_data[ftmale], draw_df$age[ftmale], method = "spearman")$estimate %>% 
  signif(.,3)
c_age_female <- cor.test(draw_df$max_data[ftfemale], draw_df$age[ftfemale], method = "spearman")$estimate %>% 
  signif(.,3)
c_f_male <- cor.test(draw_df$max_data[ftmale], draw_df$framingham_total[ftmale], method = "spearman")$estimate %>% 
  signif(.,3)
c_f_female <- cor.test(draw_df$max_data[ftfemale], draw_df$framingham_total[ftfemale], method = "spearman")$estimate %>% 
  signif(.,3)

pftfemale_f <- ggplot(draw_df[ftfemale,], aes(x = framingham_total, y = max_data)) + geom_point() +
  theme_bmbdc(font_size = 16, fill = "white") + 
  labs(caption = glue("Cor female : {c_f_female}")) +
  xlab("framingham_score")+
  ylab("IMT")
pftmale_f <- ggplot(draw_df[ftmale,], aes(x = framingham_total, y = max_data)) + geom_point() +
  theme_bmbdc(font_size = 16, fill = "white") + 
  labs(caption = glue("Cor male : {c_f_male}"))  +
  xlab("framingham_score")+
  ylab("IMT")
pftfemale_age <- ggplot(draw_df[ftfemale,], aes(x = age, y = max_data)) + geom_point() +
  theme_bmbdc(font_size = 16, fill = "white") + 
  labs(caption = glue("Cor female : {c_age_female}")) +
  ylab("IMT")
pftmale_age <- ggplot(draw_df[ftmale,], aes(x = age, y = max_data)) + geom_point() +
  theme_bmbdc(font_size = 16, fill = "white") + 
  labs(caption = glue("Cor male : {c_age_male}")) +
  ylab("IMT")

pdf(glue("{out_dir}/framingham_point.pdf"),width = 18, height = 5)
print(pftfemale_f + pftmale_f + pftfemale_age + pftmale_age + 
        plot_layout(nrow = 1) + plot_annotation(tag_level = "A")) 
dev.off()





