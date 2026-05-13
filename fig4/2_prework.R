pkgs <- c("ggthemes", "jhtools", "glue", "patchwork", "tidyverse", "ggpubr","xgboost")
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
out_dir <- "./clinical/figures/fig4/model/prework" %>% checkdir()

all_features <- c(
  "max_data",
  "age", "gender_level",
  "systolic_blood_pressure","diastolic_pressure", # 收缩压 舒张压
  "bmi", # 腹围 BMI
  "fatty_liver", # 脂肪肝
  "HDL","LDL","NHDL",
  "glycated_hemoglobinA1", # 糖化血红蛋白A1
  "white_blood_cell_count","neutrophils","neutrophil_percentage", # 血
  "lymphocyte_percentage","monocyte_percentage",
  "urea","uric_acid" # 尿素 尿酸
) %>% unique()

main_value <- "age"
width <- 10
main_predict <- "max_data"

table_frame <- rawobj %>% dplyr::select(all_of(all_features)) %>% 
  ungroup() %>% 
  dplyr::mutate(
    max_data = log10(1 + max_data),
    gender_level = ifelse(gender_level == "Male", 1, 0)
  )

df <- table_frame %>% mutate(
  main_key = 1:nrow(.)
) %>% arrange(.data[[main_value]])

# ====================== 利用锚点处模型外延 =======================
eta = 0.55
nrounds = 3
strats_value <- seq(from = 20, to = 75, by = 1)
bstx <- lapply(strats_value, function(stv){
  ft1 <- df[[main_value]] >= stv
  ft2 <- df[[main_value]] <= (stv + width)
  subdf <- df %>% dplyr::filter(ft1 & ft2)
  data_train <- subdf %>% dplyr::select(-c(main_predict, main_key, "age")) %>% as.matrix()
  bstx <- xgboost(data = data_train,
                  label = subdf[[main_predict]],
                  max_depth = 2,
                  nthread = 2, 
                  eta = eta,
                  nrounds = nrounds,
                  objective = "reg:squarederror")
  ans <- list(
    model = bstx,
    start = stv,
    subdf = subdf,
    main_value = main_value,
    main_predict = main_predict,
    fratures = list(colnames(data_train))
  )
})

matrix_bstx <- lapply(bstx, function(obj){
  modst <- obj$start
  main_value <- obj$main_value
  model <- obj$model
  fratures <- obj$fratures %>% unlist()
  lapply(strats_value, function(st){
    ft1 <- df[[main_value]] >= st
    ft2 <- df[[main_value]] <= st + width
    subdf <- df %>% dplyr::filter(ft1 & ft2)
    predx <- predict(model, subdf %>% dplyr::select(all_of(fratures)) %>% as.matrix())
    subdf <- subdf %>% mutate(
      predict = predx,
      error = predx - .data[[main_predict]],
      start_point = st,
      modst = modst
    )
  }) %>% list_rbind()
})

summary_test_every_window0 <- matrix_bstx %>% list_rbind() %>% group_by(start_point, modst) %>% 
  summarise(cor = cor(predict, max_data, method = "spearman")) 

p1 <- summary_test_every_window0 %>% 
  ggplot(aes(x = start_point, y = modst, fill = cor)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_bmbdc(font_size = 16, fill = "white") +
  ylab("local model")  +
  xlab("age")
pdf(glue("{out_dir}/model_extend_matrix.pdf"))
print(
  p1 
)
dev.off()

importance_frame <- lapply(1:length(model_list), function(i){
  yn <- strats_value[i]
  tmp <- xgb.importance(model = model_list[[i]]) %>% as_tibble() %>% dplyr::select(Feature, Gain)
  colnames(tmp)[2] <- paste0(colnames(tmp)[2], "_", yn)
  tmp
}) %>% purrr::reduce(full_join, by = "Feature")
importance_frame[is.na(importance_frame)] <- 0
importance_frame_long <- importance_frame %>% 
  pivot_longer(-"Feature") %>% 
  dplyr::mutate(start_year = str_extract(name, pattern = "\\d+") %>% as.numeric(),
                importance = value)

p2 <- ggplot(importance_frame_long, aes(x = start_year, y = Feature, fill = importance)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_bmbdc(font_size = 16, fill = "white") +
  theme(axis.title.y = element_blank()) +
  xlab("age")

pdf(glue("{out_dir}/full_importance_heatmap.pdf"), width = 12)
print(
  p2
)
dev.off()

pdf(glue("{out_dir}/prework.pdf"), width = 16, height = 5)
print(p1 + p2 +  plot_layout(width = c(1,1.2)) + plot_annotation(tag_level = "A"))
dev.off()

