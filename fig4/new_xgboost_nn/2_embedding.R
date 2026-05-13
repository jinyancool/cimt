pkgs <- c("jhtools", "glue", "tidyverse", "xgboost","ggthemes")
for (pkg in pkgs){
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}") %>% checkdir()
setwd(workdir)

out_dir <- "./clinical/figures/fig4/model/model_embedding" %>% checkdir
source("/cluster/home/ztao_jh/projects/healthman/code/zhanglei/human/clinical/fig4/new_xgboost_nn/model_info.R")
# ================= 读入模型  ===================
input_model_dir <- "./clinical/figures/fig4/model/model_ans"
feature_list <- read_rds(glue("{input_model_dir}/search_ans_feature_list.rds"))
model_list <- read_rds(glue("{input_model_dir}/search_ans_model_list.rds"))
all_used_feature <- unlist(feature_list) %>% unique()
# ================= 准备预测值 ===================
input_data_dir <- "./clinical/tables" %>% checkdir
rawobj <- read_rds(glue::glue("{input_data_dir}/raw_data.rds"))

table_frame <- rawobj[all_features] %>% ungroup()
table_frame$max_data <- log10(1 + table_frame$max_data)
table_frame$gender_level <- ifelse(table_frame$gender_level == "Male", 1, 0)

df <- table_frame %>% arrange(!!sym(main_value))
df$test_index <- 1:nrow(df)
full_pddf <- main_predict(model_list,df, jx, strats_value, main_value, width)
df$index <- df$test_index
full_pddf_df <- df %>% left_join(full_pddf, by = c("age", "index"))
# ================= mask feature ===================
scale_attr <- read_rds(glue("{out_dir}/embedding_model/scale_attr.rds"))
pcaobj <- read_rds(glue("{out_dir}/embedding_model/pca_obj.rds"))

used_features_table <- tibble(
  start_age = strats_value,
  end_age = strats_value + width,
  used_features = feature_list
)
naf <- is.na(full_pddf_df[["preidct"]])
for(i in 1:length(all_used_feature)){
  naf <- naf | is.na(full_pddf_df[[all_used_feature[[i]]]])
}
naomit_embedding <- full_pddf_df[!naf,]
mask_embedding <- mask_feature(naomit_embedding, used_features_table,all_used_feature, arrow = F)
scale_embedding <- mask_embedding %>% apply_scale(scale_attr)
pca_pos <- predict(pcaobj, scale_embedding)
pca_ft <- clean_pca(pca_pos)
naomit_embedding <- naomit_embedding[pca_ft,]
raw_pca_frame <- pca_pos[pca_ft,]
# ================= pca raw point ===================
p1 <- raw_pca_frame %>% as_tibble() %>% 
  mutate(log10_max_data = naomit_embedding$max_data, 
         log10_preidct = naomit_embedding$preidct) %>% 
  arrange(log10_max_data) %>% 
  ggplot(aes(x = PC1, y = PC2, color = log10_max_data)) +
  geom_point() +
  scale_color_viridis_c()+
  theme_few() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 1)) +
  labs(title = "max_data")
p2 <- raw_pca_frame %>% as_tibble() %>% 
  mutate(log10_max_data = naomit_embedding$max_data, 
         log10_preidct = naomit_embedding$preidct) %>% 
  arrange(log10_preidct) %>% 
  ggplot(aes(x = PC1, y = PC2, color = log10_preidct)) +
  geom_point()+
  scale_color_viridis_c()+
  theme_few() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 1))+
  labs(title = "preidct")

pdf(glue("{total_pca_dir}/pca_total_patchwork.pdf"), width = 10,height = 5)
print(p1 + p2)
dev.off()
write_rds(list(p1, p2), glue("{total_pca_dir}/pca_total_patchwork.rds"))
write_rds(raw_pca_frame, glue("{total_pca_dir}/raw_pca_frame.rds"))
write_rds(naomit_embedding, glue("{total_pca_dir}/naomit_embedding.rds"))
# ================= all_used_feature features jitter ===================
arrow_out_dir <- checkdir(glue("{out_dir}/arrow"))
pca_frame_list <- lapply(all_used_feature, function(feature_i){
  print("start")
  print(feature_i)
  embedding_jittered_data <- embedding_jitter(naomit_embedding, feature_i, pct = 0.3) 
  after_frame <- embedding_jittered_data %>% dplyr::filter(jitter_flag == "after")
  
  full_after_frame <- main_predict(model_list, after_frame, jx, strats_value, main_value, width)
  
  embedding_jittered_data <- list_rbind(list(
    embedding_jittered_data %>% dplyr::filter(jitter_flag == "before"),
    after_frame %>% dplyr::select(-"preidct") %>% left_join(full_after_frame[,c("index","preidct")], by = c("index"))
  ))
  embedding_jittered_data <- embedding_jittered_data[!is.na(embedding_jittered_data[[feature_i]]),]
  
  masked_embedding_jittered <- mask_feature(embedding_jittered_data, used_features_table,all_used_feature, arrow = T)
  scale_masked_embedding_jittered <- masked_embedding_jittered %>% apply_scale(scale_attr)
  pcaobj2 <- predict(pcaobj, scale_masked_embedding_jittered) %>%
    as_tibble() %>%
    dplyr::mutate(max_data = masked_embedding_jittered$max_data,
                  group_jitter = masked_embedding_jittered$group_jitter,
                  jitter_flag = masked_embedding_jittered$jitter_flag,
                  preidct = masked_embedding_jittered$preidct,
                  "{feature_i}" :=  masked_embedding_jittered[[feature_i]])
  
  print("gaussian_rast")
  data_frame_rasted <- gaussian_rast(pcaobj2 %>%
                                       dplyr::mutate(age = embedding_jittered_data$age,
                                                     gender_level = embedding_jittered_data$gender_level) %>% 
                                       dplyr::filter(jitter_flag == "before"), 
                                     raster_col = c("max_data", "preidct","age",feature_i) %>% unique())
  arrow_frame <- pcaobj2[c("PC1","PC2","max_data","group_jitter","jitter_flag","preidct")]
  arrow_frame_wd <- pivot_wider(arrow_frame, values_from = c("PC1","PC2","preidct"), names_from = "jitter_flag") %>% 
    dplyr::mutate(predict_delta = preidct_after - preidct_before)
  
  print("start_drawing ... ")
  predict_delta <- arrow_frame_wd$predict_delta
  delta_breaks_lines <- seq(min(predict_delta),max(predict_delta),by=(max(predict_delta)-min(predict_delta))/10) %>% unique()
  arrow_frame_wd$lable <- ifelse(arrow_frame_wd$predict_delta > 0, "up", "down")
  density2d_frame <- arrow_frame_wd %>% dplyr::slice_max(order_by = abs(predict_delta), prop = 0.1) %>% arrange(predict_delta)
  minlable <- density2d_frame %>% group_by(lable) %>% summarise(n = n()) %>% dplyr::filter(n < 5) %>% pull(lable)
  density2d_frame <- density2d_frame %>% dplyr::filter(!lable %in% minlable)
  p01 <- ggplot() +
    geom_point(data = arrow_frame_wd  %>% arrange(abs(predict_delta)),
               mapping = aes(x = PC1_before , y = PC2_before, color = predict_delta, alpha = abs(predict_delta))) +
    scale_color_gradient2(high = "red", low = "blue", mid = "grey") +
    ggnewscale::new_scale_color() +
    geom_density_2d(data = density2d_frame,
                   mapping = aes(x = PC1_before , y = PC2_before, color = lable, group = lable),
                   contour = T,contour_var = "density", h = c(2.1, 2.1)) +
    scale_color_manual(values = c(up = "red", down = "blue")) +
    theme_few() +
    ggtitle(feature_i) +
    theme(plot.title = element_text(hjust = 1)) +
    xlab("PC1") +
    ylab("PC2")
  
  p02 <- ggplot() +
    geom_point(data = data_frame_rasted  %>% arrange(!!sym(feature_i)),
               mapping = aes(x = PC1 , y = PC2, color = .data[[feature_i]])) +
    scale_color_viridis_b(option = "B",n.breaks=8) +
    theme_few() +
    ggtitle(feature_i) +
    theme(plot.title = element_text(hjust = 1))
  print("draw")
  pdf(glue::glue("{arrow_out_dir}/pca_total_{feature_i}.pdf"), width = 12, height = 5)
  print(p01 + p02) 
  dev.off()
  
  list(
    figa = p01,
    figb = p02,
    embedding_jittered_data = embedding_jittered_data,
    density2d_frame = density2d_frame
  )
})
names(pca_frame_list) <- all_used_feature
write_rds(pca_frame_list, file = glue("{arrow_out_dir}/arrow_pca.rds"))




