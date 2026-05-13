pkgs <- c("ggthemes", "jhtools", "glue", "patchwork", "tidyverse", "ggpubr")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}/") %>% checkdir()
setwd(workdir)
chi_p <- function(df){
  tb <- table(df)
  p <- fisher.test(tb,simulate.p.value=TRUE)$p.value
  p
}
save_table <- function(df){
  cn <- colnames(df)
  tb <- table(df) %>% as.data.frame() %>% 
    mutate(group = "raw_data")
  tb1 <- table(df) %>% as.data.frame() %>% 
    group_by(.data[[cn[[1]]]]) %>% 
    dplyr::mutate(sum = sum(Freq)) %>% 
    dplyr::mutate(Freq = Freq/sum , group = "normalize_lable") %>% 
    dplyr::select(-sum)
  tb2 <- table(df) %>% as.data.frame() %>% 
    group_by(.data[[cn[[2]]]]) %>% 
    dplyr::mutate(sum = sum(Freq)) %>% 
    dplyr::mutate(Freq = Freq/sum , group = "normalize_feature") %>% 
    dplyr::select(-sum)
  ans <- rbind(tb,tb1,tb2) %>% pivot_wider(names_from = cn[[1]], values_from = "Freq") %>% 
    mutate(feature = cn[[2]]) 
  colnames(ans)[1] <- "feature_level"
  ans <- ans %>% dplyr::select(feature, feature_level, group, everything())
  ans
}

in_put_dir <- "./clinical/tables"
rawobj <- read_rds(glue::glue("{in_put_dir}/raw_data.rds"))
config_fn <- glue("clinical/configs/configs.yaml")
gender_col <- show_me_the_colors(config_fn, iterm = "gender")
level_col <- show_me_the_colors(config_fn, iterm = "level")
vertical_line_age_col <- show_me_the_colors(config_fn, iterm = "vertical_line_age")
stages <- show_me_the_colors(config_fn, iterm = "stages")

logical_v <- c("FALSE" = level_col[["Normal"]], "TRUE" = level_col[["Level12"]])


out_path <- "./clinical/figures/fig1/drug_age" %>% checkdir()

vm <- c(
  "drug_blood_pressure", "drug_sugar","drug_lipid","drug_thyroid","drug_heart",
  "drug_vessel","drug_gout", "drug_depress","drug_anxiety","drug_sleep"
)

# ====================== 50 ============================================
abnormalsheetx <- rawobj[, c(vm, "age_level")] %>%
  dplyr::mutate(
    age_level = case_when(
      age_level %in% c("<= 30", "<= 40", "<= 50") ~ "<= 50",
      T ~ "> 50"),
    age_level = factor(age_level, levels = c("<= 50", "> 50"))
  )

pl <- list()
csv_frame <- list()
for(i in 1:length(vm)){
  i = vm[[i]]
  boxsub1_level_framex <- abnormalsheetx %>% 
    dplyr::select(all_of(c("age_level", i))) %>% 
    na.omit() 
  boxsub1_level_framex[[i]] <- as.character(boxsub1_level_framex[[i]])

  pl[[i]] <- ggplot(boxsub1_level_framex, aes(x = age_level, fill = .data[[i]])) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = logical_v) +
    guides(fill = guide_legend(title = "drug_used")) + 
    coord_cartesian(clip = "off") +
    theme_bmbdc(font_size = 16, fill = "white") + 
    theme (legend.position= "top") + 
    labs(caption = glue("sample_size:{nrow(boxsub1_level_framex)}
                  pvalue:{signif(chi_p(boxsub1_level_framex[,c('age_level', i)]), 3)}")) +
    ylab("percent") + 
    xlab(i) 
  csv_frame[[i]] <- save_table(boxsub1_level_framex[,c("age_level", i)])
}
multi_plot(pl, fig_fn = glue("{out_path}/drug_age_50.pdf"),
           width = 12, height = 10)
write_csv(csv_frame %>% purrr::list_rbind(), file = glue("{out_path}/drug_age_50.csv"))
write_rds(pl, file = glue("{out_path}/drug_age_50.rds"))
# ====================== 60 ============================================
abnormalsheetx <- rawobj[, c(vm, "age_level")] %>%
  dplyr::mutate(
    age_level = case_when(
      age_level %in% c("<= 30", "<= 40", "<= 50", "<= 60") ~ "<= 60",
      T ~ "> 60"),
    age_level = factor(age_level, levels = c("<= 60", "> 60"))
  )

pl <- list()
csv_frame <- list()
for(i in 1:length(vm)){
  i = vm[[i]]
  boxsub1_level_framex <- abnormalsheetx %>% 
    dplyr::select(all_of(c("age_level", i))) %>% 
    na.omit() 
  boxsub1_level_framex[[i]] <- as.character(boxsub1_level_framex[[i]])
  
  pl[[i]] <- ggplot(boxsub1_level_framex, aes(x = age_level, fill = .data[[i]])) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = logical_v) +
    guides(fill = guide_legend(title = "drug_used")) + 
    coord_cartesian(clip = "off") +
    theme_bmbdc(font_size = 16, fill = "white") + 
    theme (legend.position= "top") + 
    labs(caption = glue("sample_size:{nrow(boxsub1_level_framex)}
                  pvalue:{signif(chi_p(boxsub1_level_framex[,c('age_level', i)]), 3)}")) +
    ylab("percent") + 
    xlab(i) 
  csv_frame[[i]] <- save_table(boxsub1_level_framex[,c("age_level", i)])
}
multi_plot(pl, fig_fn = glue("{out_path}/drug_age_60.pdf"),
           width = 12, height = 10)
write_csv(csv_frame %>% purrr::list_rbind(), file = glue("{out_path}/drug_age_60.csv"))
write_rds(pl, file = glue("{out_path}/drug_age_60.rds"))



