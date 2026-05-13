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


out_path <- checkdir("./clinical/figures/fig1/highmax_lowldl")
rawobj <- rawobj %>% mutate(
  new_lable = case_when(LDL < 1.8 & IMT >= 1 ~ "loLDL_hiMax",
                         LDL < 1.8 & IMT < 1 ~ "loLDL_loMax",
                         LDL >= 1.8 & IMT >= 1 ~ "hiLDL_hiMax",
                         LDL >= 1.8 & IMT < 1 ~ "hiLDL_loMax",
                         T ~ "other")
)
vm <- c(
  "history_heart_brain","history_diabetes","history_cancer","history_heart",
  "history_sleep","history_mental","history_no","spots","sleep","drug_blood_pressure",
  "drug_sugar","drug_lipid","drug_thyroid","drug_heart","drug_vessel","drug_gout",
  "drug_depress","drug_anxiety","drug_sleep","LDL_level","bmi_level","NHDL_level","fatty_liver",
  "abdominal_circumference_level","glycated_hemoglobinA1C_level", "glycated_hemoglobinA1_level"
)
abnormalsheetx <- rawobj[, c(vm, "new_lable","gender_level","age_level","IMT_level")] %>%
  dplyr::filter(new_lable != "other")
abnormalsheetx$new_lable <- factor(abnormalsheetx$new_lable, 
                                    levels = c("loLDL_hiMax", "hiLDL_hiMax", 
                                               "loLDL_loMax", "hiLDL_loMax"))
ic <- vm
pl <- list()
csv_frame <- list()
for(i in 1:length(ic)){
  i = ic[[i]]
  boxsub1_level_framex <- abnormalsheetx %>% 
    dplyr::select(all_of(c("new_lable", i))) %>% 
    na.omit() 
  boxsub1_level_framex[[i]] <- as.character(boxsub1_level_framex[[i]])
  if(all(boxsub1_level_framex[[i]] %in% c("0", "1"))){
    boxsub1_level_framex[[i]] <- ifelse(boxsub1_level_framex[[i]] == 0, "FALSE", "TRUE")
  }

  if(all(boxsub1_level_framex[[i]] %in% c("TRUE", "FALSE"))){
    pl[[i]] <- ggplot(boxsub1_level_framex, aes(x = new_lable, fill = .data[[i]])) +
      geom_bar(position = "fill") + 
      theme_bmbdc(font_size = 16, fill = "white") + 
      theme (legend.position= "top") + 
      labs(caption = glue("sample_size:{nrow(boxsub1_level_framex)}
                    pvalue:{signif(chi_p(boxsub1_level_framex[,c('new_lable', i)]), 3)}")) +
      ylab("percent") + 
      coord_cartesian(clip = "off") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
    pl[[i]] <- pl[[i]] +
      scale_fill_manual(values = logical_v)+
      xlab(i)
  }else{
    if(i == "HDL_level"){
      boxsub1_level_framex[[i]][boxsub1_level_framex[[i]] == "Low"] <- 0
      boxsub1_level_framex[[i]][boxsub1_level_framex[[i]] == "Normal"] <- 1
      boxsub1_level_framex[[i]][boxsub1_level_framex[[i]] == "High"] <- 2
    }
    boxsub1_level_framex[[i]] <- paste0("stage", as.numeric(boxsub1_level_framex[[i]]) + 1)
    pl[[i]] <- ggplot(boxsub1_level_framex, aes(x = new_lable, fill = .data[[i]])) +
      geom_bar(position = "fill") + 
      theme_bmbdc(font_size = 16, fill = "white") + 
      theme (legend.position= "top") + 
      labs(caption = glue("sample_size:{nrow(boxsub1_level_framex)}
                    pvalue:{signif(chi_p(boxsub1_level_framex[,c('new_lable', i)]), 3)}")) +
      ylab("percent") + 
      coord_cartesian(clip = "off") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      xlab(i)
    pl[[i]] <- pl[[i]] +
      scale_fill_manual(name = "stages", values = stages)
  }
  csv_frame[[i]] <- save_table(boxsub1_level_framex[,c("new_lable", i)])
}
multi_plot(pl, fig_fn = glue("{out_path}/loLDL_hiMax_bar_new_lable.pdf"), 
           width = 12, height = 10)
write_csv(csv_frame %>% purrr::list_rbind(), file = glue("{out_path}/bar_new_lable.csv"))
write_rds(pl, file = glue("{out_path}/loLDL_hiMax_bar_new_lable.rds"))





