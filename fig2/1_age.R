pkgs <- c("fs", "futile.logger", "configr", "tidyverse",
          "jhtools", "glue", "patchwork",  "ComplexHeatmap", "ggpubr")
for (pkg in pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = T))
}
project <- "healthman"
dataset <- "zhanglei"
species <- "human"
workdir <- glue("~/projects/{project}/analysis/{dataset}/{species}/") %>% checkdir()
setwd(workdir)
in_put_dir <- "./clinical/tables"
rawobj <- read_rds(glue::glue("{in_put_dir}/raw_data.rds")) %>% 
  mutate(
    NL = `NL` %>% str_remove("岁") %>% as.numeric()
  ) %>% dplyr::filter(NL <= 70)
config_fn <- glue("./clinical/configs/configs.yaml")
gender_col <- show_me_the_colors(config_fn, iterm = "gender")
level_col <- show_me_the_colors(config_fn, iterm = "level")
vertical_line_age_col <- show_me_the_colors(config_fn, iterm = "vertical_line_age")

age_out <- checkdir("./clinical/figures/fig2/total")
age_out_gender <- checkdir("./clinical/figures/fig2/gender")
make_fig <- checkdir("./clinical/figures/fig2/make_fig")

pctv <- seq(0.6, 0.95, 0.05)
getrootframe <- function(df, y = 1){
  df %>% group_by(group, PANEL) %>% 
    mutate(delta = abs(y - {{y}}))  %>% 
    slice_min(order_by = delta, n = 2) %>% 
    summarise( root = ifelse(x[1] * x[2] < 0, mean(x), x[1])) %>% 
    ungroup() 
}
getyearpctframe <- function(df, x = 1){
  df %>% group_by(group, PANEL) %>% 
    mutate(delta = abs(x - {{x}}))  %>% 
    slice_min(order_by = delta, n = 2) %>% 
    summarise( root = ifelse(y[1] * y[2] < 0, mean(y), y[1])) %>% 
    ungroup() 
}

# ================== 总体表 ====================================================
framelist <- lapply(pctv, function(qt){
  rawobj %>% group_by(NL) %>% dplyr::summarise(
    "{qt * 100}%" := quantile(IMT, {{ qt }})
  )
})
framesheet <- purrr::reduce(framelist, left_join)
maxframe <- framesheet %>% dplyr::select(1, contains("%")) %>% 
  pivot_longer(-"NL")
p2 <- ggplot() +
  geom_point(rawobj, mapping = aes(x = NL, y = IMT)) +
  geom_smooth(maxframe,
              mapping = aes(x = NL, y = value, color = name),
              linewidth = 1, se = F) + 
  theme_bmbdc(font_size = 16, fill = "white") + 
  geom_hline(yintercept = c(1,1.5), color = vertical_line_age_col, linewidth = 1) +
  geom_vline(xintercept = c(30, 40), color = vertical_line_age_col, linewidth = 1) + 
  guides(color = guide_legend(reverse = T)) +
  labs(colour = "percent") +
  xlab("age") +
  scale_color_viridis_d()

pdf(glue("{age_out}/age_total.pdf"))
print(p2)
dev.off()

bp <- ggplot_build(p2)
root_max_pct <- data.frame(
  lable = glue::glue("{pctv}%"),
  Level1 = getrootframe(bp$data[[2]], 1)$root %>% signif(digits = 5),
  Level2 = getrootframe(bp$data[[2]], 1.5)$root %>% signif(digits = 5)
) %>% arrange(desc(lable))
write_csv(root_max_pct, file = glue("{age_out}/age_total.csv"))


year_pct_maxdata <- parallel::mclapply(20:70, function(x){
  tibble(
    lable = glue::glue("{pctv}%"),
    "{x}" := getyearpctframe(bp$data[[2]], {{x}})$root
  ) %>% arrange(desc(lable))
}, mc.cores = 5L) %>% purrr::reduce(left_join, by = "lable")

new_lable <- 1 - (year_pct_maxdata$lable %>% str_remove("%") %>% as.numeric())
ha = rowAnnotation(pct = anno_numeric(new_lable,
                                      labels_format = function(x) paste0(sprintf("%.2f", x*100), "%")),
                   annotation_name_rot = 0)
mx <- year_pct_maxdata[-1]
heatmap_all <- Heatmap(mx, 
        name = "IMT",
        right_annotation = ha,
        row_split = rep("IMT percent", times = nrow(mx)),
        cell_fun = function(j, i, x, y, w, h, col) { 
          grid.text(signif(mx[i, j], digits = 2), x, y)
        },
        cluster_rows = F, cluster_columns = F, 
        show_column_names = T, show_row_names = F, heatmap_legend_param = list(
          at = c(0.5, 1, 1.5, 2, 3),
          labels = c("normal", "level1", "level2", "very high", "    "),
          title = "IMT",
          legend_height = unit(4, "cm"),
          title_position = "lefttop-rot"
        ),
        col = circlize::colorRamp2(c(0.5, 1, 1.5, 3), c("white","orange","pink","red")))

pdf(glue("{age_out}/age_total_heatmap.pdf"), width = 22, height = 3)
draw(heatmap_all)
dev.off()
write_csv(year_pct_maxdata, file = glue("{age_out}/age_total_heatmap.csv"))

IMT_level_frame0 <- rawobj %>% 
  mutate(IMT_level0 = case_when(
    IMT_level == "Normal" ~ "Normal",
    IMT_level %in% c("Level1", "Level2") ~ "Level1orLevel2",
  )) %>%  
  group_by(gender_level, age_level, IMT_level0) %>% dplyr::summarise(
    sample_size = n()
  ) %>% 
  dplyr::mutate(IMT_level_total_count = sum(sample_size), 
                IMT_level_pct = sample_size / IMT_level_total_count * 100,
                gender_level = as.character(gender_level))
IMT_level_frame0$age_bin <- str_extract(IMT_level_frame0$age_level, pattern = "\\d+") %>% as.numeric()
p5 <- ggplot(IMT_level_frame0, aes(x = age_bin, y = IMT_level_pct, color = gender_level)) + 
  geom_point(mapping = aes(size = sample_size)) +
  geom_smooth(se = F, span = 1) +
  facet_grid(~ IMT_level0) +
  theme_bmbdc(font_size = 16, fill = "white") + 
  scale_color_manual(values = gender_col) +
  xlab("age") +
  ylab("IMT percent")
pdf(glue("{age_out}/age_gender_pct.pdf"), width = 9, height = 5)
print(p5)
dev.off()

pdf(glue("{make_fig}/fig2A.pdf"), width = 20, height = 8)
print(
  (p2 + plot_spacer() + p5 + 
     plot_layout(ncol = 3, widths = c(1, 1, 2)))/grid::grid.grabExpr(draw(heatmap_all)) +
    plot_annotation(tag_levels = c('A'))
)
dev.off()

bp5 <- ggplot_build(p5)
gender_pct <- bp5$data[[1]] %>% as_tibble() %>% 
  dplyr::mutate(
    group = case_when(PANEL == 1 ~ "Level", PANEL == 2 ~ "Normal"),
    gender = case_when(colour == "#E12713" ~ "Female", colour == "#1C4999" ~ "Male"),
    age_bin = x %>% signif(digits = 2),
    pct = y %>% signif(digits = 3)) %>% 
  dplyr::select(gender, group, age_bin, pct)
write_csv(gender_pct, file = glue("{age_out}/age_gender_pct.csv"))
# ================== 性别表 ====================================================
framelist_gender <- lapply(pctv, function(qt){
  rawobj %>% group_by(gender_level, NL) %>% dplyr::summarise(
    "{qt * 100}%" := quantile(IMT, {{ qt }})
  )
})
framesheet_gender <- purrr::reduce(framelist_gender, left_join)
maxframe_gender <- framesheet_gender %>% 
  dplyr::select(1, 2, contains("%")) %>% pivot_longer(-c("NL", "gender_level"))
p3 <- ggplot() +
  geom_point(rawobj, mapping = aes(x = NL, y = IMT)) +
  geom_smooth(maxframe_gender,
              mapping = aes(x = NL, y = value, color = name),
              linewidth = 1, se = F) + 
  theme_bmbdc(font_size = 16, fill = "white") + 
  labs(colour = "percent") +
  xlab("age") +
  guides(color = guide_legend(reverse = T)) +
  geom_hline(yintercept = c(1, 1.5), color = vertical_line_age_col, size = 1) +
  geom_vline(xintercept = c(30, 40), color = vertical_line_age_col, size = 1) +
  scale_color_viridis_d() + 
  facet_wrap(~ gender_level)
pdf(glue("{age_out_gender}/age_gender.pdf"), width = 12, height = 5)
print(p3)
dev.off()



bp <- ggplot_build(p3)
df <- getrootframe(bp$data[[2]], 1)
df$Level2 <- getrootframe(bp$data[[2]], 1.5)$root
colnames(df) <- c("lable", "panel", "Level1","Level2")
lable_frame <- tibble(
  lable = 1:length(pctv),
  new_lable = glue::glue("{pctv * 100}%")
)
df <- df %>% left_join(lable_frame, by = "lable")
df$panel <- df$panel %>% as.character() %>% as.numeric()
gender_frame <- tibble(
  panel = 1:2,
  gender = c("Female", "Male")
)
df <- df %>% left_join(gender_frame, by = "panel")
df %>% arrange(desc(lable), gender) %>% dplyr::select(gender, new_lable, Level1, Level2) %>% 
  dplyr::mutate(Level1 = signif(Level1, 3), Level2 = signif(Level2, 3)) %>% 
  write_csv(file = glue("{age_out_gender}/age_gender.csv"))


year_pct_maxdata <- parallel::mclapply(20:70, function(x){
  df <- getyearpctframe(bp$data[[2]], {{x}})
  colnames(df) <- c("lable", "panel", x)
  lable_frame <- tibble(
    lable = 1:length(pctv),
    new_lable = glue::glue("{pctv}%")
  )
  df <- df %>% left_join(lable_frame, by = "lable")
  df$panel <- df$panel %>% as.character() %>% as.numeric()
  gender_frame <- tibble(
    panel = 1:2,
    gender = c("Female", "Male")
  )
  df %>% left_join(gender_frame, by = "panel") %>% 
    arrange(desc(lable), gender) %>% dplyr::select(-all_of(c("lable", "panel"))) %>% 
    dplyr::select(new_lable, gender, everything())
}, mc.cores = 5L) %>% purrr::reduce(left_join, by = c("new_lable", "gender"))

gender <- year_pct_maxdata$gender
new_lable <- 1 - (year_pct_maxdata$new_lable %>% str_remove("%") %>% as.numeric())
mx <- year_pct_maxdata %>% dplyr::select(-all_of(c("gender","new_lable")))
ha = rowAnnotation(pct = anno_numeric(new_lable,
                                          labels_format = function(x) paste0(sprintf("%.2f", x*100), "%")),
                   annotation_name_rot = 0)
heatmap_gender <- Heatmap(mx, 
                          name = "group_gender",
                          right_annotation = ha,
                          cell_fun = function(j, i, x, y, w, h, col) { 
                            grid.text(signif(mx[i, j], digits = 2), x, y)
                          },
                          row_split = gender,
                          cluster_rows = F, cluster_columns = F, 
                          show_column_names = T, show_row_names = F, heatmap_legend_param = list(
                            at = c(0.5, 1, 1.5, 2, 3),
                            labels = c("normal", "level1", "level2", "very high", "    "),
                            title = "IMT",
                            legend_height = unit(4, "cm"),
                            title_position = "lefttop-rot"
                          ),
                          col = circlize::colorRamp2(c(0.5, 1, 1.5, 3), c("white","orange","pink","red")))
pdf(glue("{age_out_gender}/age_gender_heatmap.pdf"), width = 22, height = 3)
draw(heatmap_gender)
dev.off()
write_csv(year_pct_maxdata, file = glue("{age_out_gender}/age_gender_heatmap.csv"))

dfx <- df %>% arrange(desc(lable), gender) %>% dplyr::select(gender, new_lable, Level1, Level2)
dfy <- df %>% arrange(desc(lable), gender) %>% dplyr::select(gender, new_lable, Level1, Level2) %>% 
  group_by(new_lable) %>% summarise(delta_level1 = Level1[1] - Level1[2], 
                                    delta_level2 = Level2[1] - Level2[2])
dfx %>% left_join(dfy, by = "new_lable") %>% 
  dplyr::mutate(Level1 = signif(Level1, 3), Level2 = signif(Level2, 3), delta_level1 = signif(delta_level1, 3), delta_level2 = signif(delta_level2, 3)) %>% 
  write_csv(file = glue("{age_out_gender}/age_gender_delta.csv"))




pdf(glue("{make_fig}/fig2B.pdf"), width = 20, height = 8)
print(
  (p3 + plot_spacer() + 
     plot_layout(ncol = 2, widths = c(1.5, 2)))/grid::grid.grabExpr(draw(heatmap_gender)) +
    plot_annotation(tag_levels = c('A'))
)
dev.off()






