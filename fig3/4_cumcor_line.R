pkgs <- c("fs", "configr", "ggthemes", "jhtools", "glue", "patchwork", "tidyverse", "ggpubr")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}") %>% checkdir()
setwd(workdir)
in_put_dir <- "clinical/tables"
rawobj <- read_rds(glue::glue("{in_put_dir}/raw_data.rds"))

config_fn <- glue("./clinical/configs/configs.yaml")
gender_col <- show_me_the_colors(config_fn, iterm = "gender")
level_col <- show_me_the_colors(config_fn, iterm = "level")
vertical_line_age_col <- show_me_the_colors(config_fn, iterm = "vertical_line_age")
cor_col <- show_me_the_colors(config_fn, iterm = "cor")


out_dir <- "clinical/figures/fig3/cumline" %>% checkdir
cormf <- function(df, key_value = "IMT", main_value = "age", width = 10){
  df <- df %>% arrange(!!sym(main_value))
  min_main_value <- min(df[[main_value]])
  max_main_value <- max(df[[main_value]])
  
  strats_value <- seq(from = min_main_value, to = max_main_value - width)
  bingl <- parallel::mclapply(strats_value, function(stv){
    ft1 <- df[[main_value]] >= stv
    ft2 <- df[[main_value]] <= (stv + width)
    subdf <- df %>% dplyr::filter(ft1 & ft2)
    windowi <- lapply(colnames(subdf), function(x){
      if(x != key_value){
        .df <- subdf %>% dplyr::select(all_of(c(x, key_value))) %>% na.omit()
        if(nrow(.df) <= 5){
          ans <- tibble(R = NA, pvalue = NA)
        }else{
          tmp <- cor.test(.df[[1]], .df[[2]], method = "spearman")
          ans <- tibble(
            R = tmp$estimate,
            pvalue = tmp$p.value)
        }
      }else{
        ans <- tibble(
          R = 1,
          pvalue = 0
        )
      }
      return(ans)
    }) %>% list_rbind()
    windowi$feature <- colnames(subdf)
    windowi$start_point <- stv
    return(windowi)
  }, mc.cores = 20L) %>% list_rbind()
  bingl
}
cal_cumline <- function(nolevel1, max_age = 70){
  nolevelx <- c(nolevel1, nolevel1 %>% str_remove(pattern = "_age"), "age")
  long_matrix <- list()
  for(gender_i in gender_v){
    tw2cor_male <- rawobj %>% ungroup() %>% dplyr::filter(gender_level == {{ gender_i }}) %>% dplyr::select(all_of(unique(c("IMT", "age", nolevelx))))
    long_matrix[[gender_i]] <- cormf(df = tw2cor_male, key_value = "IMT", main_value = "age", width = 10) %>% 
      na.omit() %>% 
      mutate(log10_pvalue = -log10(pvalue)) %>%
      dplyr::filter(start_point <= {{ max_age }})
  }
  data_main <- long_matrix %>% purrr::list_rbind(names_to = "group")
  data_main
}


width <- 10
group <- "gender_level"
gender_v <- c("Female", "Male")

select_feature_cumage <- c("systolic_blood_pressure_age","diastolic_pressure_age","blood_pressure_level_age", # 收缩压 舒张压
                           "glycated_hemoglobinA1_age", "glycated_hemoglobinA1C_age", 
                           "triglycerides_age","TC_age", "NHDL_age", "LDL_age"  
)
rawobj <- rawobj %>% mutate(
  systolic_blood_pressure_age = systolic_blood_pressure * age,
  diastolic_pressure_age = diastolic_pressure * age,
  blood_pressure_level_age = blood_pressure_level * age,
  triglycerides_age = triglycerides * age,
  glycated_hemoglobinA1_age = glycated_hemoglobinA1 * age,
  glycated_hemoglobinA1C_age = glycated_hemoglobinA1C * age,
  free_fatty_acid_age = free_fatty_acid * age,
  TC_age = TC * age
)
nolevel1 <- select_feature_cumage
corframe <- cal_cumline(select_feature_cumage)

plist <- list()
for(i in select_feature_cumage){
  ix <- str_remove(i, pattern = "_age")
  cor_lab <- c(level_col[["Level12"]], level_col[["Normal"]])
  names(cor_lab) <- c(i, ix)
  plist[[i]] <- corframe %>% 
    dplyr::filter(feature %in% c(i ,ix)) %>% 
    ggplot(aes(x = start_point, y = R, color = feature)) +
    geom_line() +
    theme_bmbdc(font_size = 16, fill = "white") + 
    facet_wrap(~ group) +
    geom_hline(yintercept = 0, color = "red") +
    scale_color_manual(values = cor_lab)
}
multi_plot(plist, fig_fn = glue("{out_dir}/cumline.pdf"), width = 12, height = 10, ncol = 1)


full_out_dir <- "clinical/figures/fig3/line_plot_make_fig" %>% checkdir
pdf(glue("{full_out_dir}/cumline.pdf"), width = 14, height = 20)
plist[["systolic_blood_pressure_age"]] +
  plist[["diastolic_pressure_age"]] +
  plist[["blood_pressure_level_age"]] +
  plist[["glycated_hemoglobinA1_age"]] +
  plist[["glycated_hemoglobinA1C_age"]] +
  plist[["triglycerides_age"]] +
  plist[["TC_age"]] +
  plist[["NHDL_age"]] +
  plist[["LDL_age"]] +
  plot_annotation(tag_levels = c('A'))+ 
  plot_layout(ncol = 2) & theme(legend.position='bottom') 
dev.off()








