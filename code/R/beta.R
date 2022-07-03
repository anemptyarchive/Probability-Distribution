
# ベータ分布 -------------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)
library(MCMCpack)
library(gganimate)
library(patchwork)

# チェック用
library(magrittr)
library(ggplot2)
library(patchwork)


# 確率密度の計算 -----------------------------------------------------------------

# パラメータを指定
alpha <- 5
beta  <- 2

# 確率変数の値を指定
phi <- 0.6

# ベクトルに変換
alpha_v <- c(alpha, beta)
phi_v <- c(phi, 1 - phi)


# 定義式により確率密度を計算
C <- gamma(alpha + beta) / gamma(alpha) / gamma(beta)
dens <- C * phi^(alpha - 1) * (1 - phi)^(beta - 1)
dens

# 対数をとった定義式により確率密度を計算
log_C <- lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta)
log_dens <- log_C + (alpha - 1) * log(phi) + (beta - 1) * log(1 - phi)
dens <- exp(log_dens)
dens; log_dens

# ベータ分布の関数により確率密度を計算
dens <- dbeta(x = phi, shape1 = alpha, shape2 = beta)
dens

# ベータ分布の対数をとった関数により確率密度を計算
log_dens <- dbeta(x = phi, shape1 = alpha, shape2 = beta, log = TRUE)
dens <- exp(log_dens)
dens; log_dens

# ディリクレ分布の関数により確率密度を計算
dens <- MCMCpack::ddirichlet(x = phi_v, alpha = alpha_v)
dens


# 統計量の計算 ------------------------------------------------------------------

# パラメータを指定
alpha <- 5
beta  <- 2


# 期待値を計算
E_phi <- alpha / (alpha + beta)
E_phi

# 分散を計算
V_phi <- alpha * beta / (alpha + beta)^2 / (alpha + beta + 1)
V_phi

# 最頻値を計算:(alpha > 1, beta > 1)
mode_phi <- (alpha - 1) / (alpha + beta - 2)
mode_phi


# 歪度を計算
numer <- 2 * (beta - alpha) * sqrt(alpha + beta + 1)
denom <- (alpha + beta + 2) * sqrt(alpha * beta)
skewness <- numer / denom
skewness

# 尖度を計算
numer <- (6 * (alpha - beta)^2 * (alpha + beta + 1) - alpha * beta * (alpha + beta + 2))
denom <- alpha * beta * (alpha + beta + 2) * (alpha + beta + 3)
kurtosis <- numer / denom
kurtosis


# グラフの作成 ------------------------------------------------------------------

# パラメータを指定
alpha <- 5
beta  <- 2


# phiがとり得る値を作成
phi_vals <- seq(from = 0, to = 1, length.out = 501)

# ベータ分布を計算
dens_df <- tidyr::tibble(
  phi = phi_vals, # 確率変数
  density = dbeta(x = phi_vals, shape1 = alpha, shape2 = beta) # 確率密度
)

# ベータ分布のグラフを作成
ggplot(data = dens_df, mapping = aes(x = phi, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 折れ線グラフ
  labs(title = "Beta Distribution", 
       subtitle = paste0("alpha=", alpha, ", beta=", beta), # (文字列表記用)
       #subtitle = parse(text = paste0("list(alpha==", alpha, ", beta==", beta, ")")), # (数式表記用)
       x = expression(phi), y = "density") # ラベル


# 補助線用の統計量を計算
E_phi <- alpha / (alpha + beta)
s_phi <- sqrt(alpha * beta / (alpha + beta)^2 / (alpha + beta + 1))
mode_phi <- (alpha - 1) / (alpha + beta - 2)

# 統計量を重ねたベータ分布のグラフを作成:線のみ
ggplot(data = dens_df, mapping = aes(x = phi, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 折れ線グラフ
  geom_vline(xintercept = E_phi, color = "blue", size = 1, linetype = "dashed") + # 期待値
  geom_vline(xintercept = E_phi-s_phi, color = "orange", size = 1, linetype = "dotted") + # 期待値 - 標準偏差
  geom_vline(xintercept = E_phi+s_phi, color = "orange", size = 1, linetype = "dotted") + # 期待値 + 標準偏差
  geom_vline(xintercept = mode_phi, color = "chocolate", size = 1, linetype = "dashed") + # 最頻値
  labs(title = "Beta Distribution", 
       subtitle = parse(text = paste0("list(alpha==", alpha, ", beta==", beta, ")")), 
       x = expression(phi), y = "density") # ラベル


# 統計量を格納
stat_df <- tibble::tibble(
  statistic = c(E_phi, E_phi-s_phi, E_phi+s_phi, mode_phi), # 統計量
  type = c("mean", "sd", "sd", "mode") # 色分け用ラベル
)

# 凡例用の設定を作成:(数式表示用)
color_vec <- c(mean = "blue", sd = "orange", mode = "chocolate")
linetype_vec <- c(mean = "dashed", sd = "dotted", mode = "dashed")
label_vec <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))), mode = expression(mode(x)))

# 統計量を重ねたベータ分布のグラフを作成:凡例付き
ggplot(data = dens_df, mapping = aes(x = phi, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 分布
  geom_vline(data = stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 統計量
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Beta Distribution", 
       subtitle = parse(text = paste0("list(alpha==", alpha, ", beta==", beta, ")")), 
       x = expression(phi), y = "density") # ラベル


# パラメータと分布の関係：並べて比較 ----------------------------------------------------

# phiがとり得る値を作成
phi_vals <- seq(from = 0, to = 1, length.out = 501)


### ・αの影響 -----

# パラメータとして利用する値を指定
alpha_vals <- c(0.1, 0.5, 1, 2, 4, 8)

# 固定するパラメータを指定
beta <- 2


# パラメータごとにベータ分布を計算
res_dens_df <- tidyr::expand_grid(
  phi = phi_vals, 
  alpha = alpha_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(alpha, phi) |> # パラメータごとに並べ替え
  dplyr::mutate(
    dens = dbeta(x = phi, shape1 = alpha, shape2 = beta), 
    parameter = paste0("alpha=", alpha, ", beta=", beta) |> 
      factor(levels = paste0("alpha=", sort(alpha_vals), ", beta=", beta)) # 色分け用ラベル
  ) # 確率密度を計算


### ・βの影響 -----

# 固定するパラメータを指定
alpha <- 2

# パラメータとして利用する値を指定
beta_vals <- c(0.1, 0.5, 1, 2, 4, 8)


# パラメータごとにベータ分布を計算
res_dens_df <- tidyr::expand_grid(
  phi = phi_vals, 
  beta = beta_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(beta, phi) |> # パラメータごとに並べ替え
  dplyr::mutate(
    dens = dbeta(x = phi, shape1 = alpha, shape2 = beta), 
    parameter = paste0("alpha=", alpha, ", beta=", beta) |> 
      factor(levels = paste0("alpha=", alpha, ", beta=", sort(beta_vals))) # 色分け用ラベル
  ) # 確率密度を計算


### ・αとβの影響 -----

# パラメータとして利用する値を指定
alpha_vals <- c(1, 0.5, 2, 2, 0.9, 0.8)
beta_vals  <- c(1, 0.5, 2, 4, 0.7, 1.2)

# パラメータごとにベータ分布を計算
res_dens_df <- tidyr::expand_grid(
  phi = phi_vals, 
  i = 1:length(alpha_vals) # パラメータ番号
) |> # 全ての組み合わせを作成
  dplyr::arrange(i, phi) |> # パラメータごとに並べ替え
  dplyr::mutate(
    alpha = alpha_vals[i], 
    beta = beta_vals[i]
  ) |> # パラメータ列を追加
  dplyr::mutate(
    dens = dbeta(x = phi, shape1 = alpha, shape2 = beta), 
    parameter = paste0("alpha=", alpha, ", beta=", beta) |> 
      factor() # 色分け用ラベル
  ) # 確率密度を計算


### ・作図 -----

# 凡例用のラベルを作成:(数式表示用)
label_vec <- res_dens_df[["parameter"]] |> 
  unique() |> # 重複を除去
  stringr::str_replace_all(pattern = "=", replacement = "==") %>% # 等号表示用の記法に変換
  paste0("list(", ., ")") |> # カンマ表示用の記法に変換
  parse(text = _) # expression関数化
names(label_vec) <- unique(res_dens_df[["parameter"]]) # ggplotに指定する文字列に対応する名前付きベクトルに変換

# パラメータごとにベータ分布を作図
ggplot(data = res_dens_df, mapping = aes(x = phi, y = dens, color = parameter)) + # データ
  geom_line(size = 1, show.legend = TRUE) + # 折れ線グラフ
  scale_color_hue(labels = label_vec) + # 線の色:(数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  coord_cartesian(ylim = c(0, 5)) + # 軸の表示範囲
  labs(title = "Beta Distribution", 
       x = expression(phi), y = "density") # ラベル


# パラメータと分布の関係：アニメーションによる可視化 -----------------------------------------------

# phiがとり得る値を作成
phi_vals <- seq(from = 0, to = 1, length.out = 501)


### ・αの影響 -----

# パラメータとして利用する値を指定
alpha_vals <- seq(from = 0.1, to = 15, by = 0.1)
length(alpha_vals) # フレーム数

# 固定するパラメータを指定
beta <- 2


# パラメータごとにベータ分布を計算
anime_dens_df <- tidyr::expand_grid(
  phi = phi_vals, 
  alpha = alpha_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(alpha, phi) |> # パラメータごとに並べ替え
  dplyr::mutate(
    dens = dbeta(x = phi, shape1 = alpha, shape2 = beta), 
    parameter = paste0("alpha=", alpha, ", beta=", beta) |> 
      factor(levels = paste0("alpha=", sort(alpha_vals), ", beta=", beta)) # フレーム切替用ラベル
  ) # 確率密度を計算


### ・βの影響 -----

# 固定するパラメータを指定
alpha <- 2

# パラメータとして利用する値を指定
beta_vals <- seq(from = 0.1, to = 15, by = 0.1)
length(beta_vals) # フレーム数


# パラメータごとにベータ分布を計算
anime_dens_df <- tidyr::expand_grid(
  phi = phi_vals, 
  beta = beta_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(beta, phi) |> # パラメータごとに並べ替え
  dplyr::mutate(
    dens = dbeta(x = phi, shape1 = alpha, shape2 = beta), 
    parameter = paste0("alpha=", alpha, ", beta=", beta) |> 
      factor(levels = paste0("alpha=", alpha, ", beta=", sort(beta_vals))) # フレーム切替用ラベル
  ) # 確率密度を計算


### ・作図 -----

# ベータ分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = phi, y = dens)) + # データ
  geom_line(color = "#00A968", size = 1) + # 折れ線グラフ
  gganimate::transition_manual(parameter) + # フレーム
  coord_cartesian(ylim = c(0, 5)) + # 軸の表示範囲
  labs(title = "Beta Distribution", 
       subtitle = "{current_frame}", 
       x = expression(phi), y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(alpha_vals), fps = 10, width = 800, height = 600) # (αの影響用)
gganimate::animate(anime_dens_graph, nframes = length(beta_vals), fps = 10, width = 800, height = 600) # (βの影響用)


# パラメータの比較：アニメーションによる可視化 --------------------------------------------------

# phiがとり得る値を作成
phi_vals <- seq(from = 0, to = 1, length.out = 501)


### ・αの影響 -----

# 比較する値を指定
alpha_vals <- c(0.1, 0.25, 0.5, 1, 1.5, 2, 5, 10, 15)

# 変化する値を指定
beta_vals <- seq(from = 0.5, to = 10, by = 0.5)
length(beta_vals) # フレーム数


# パラメータごとにベータ分布を計算
anime_dens_df <- tidyr::expand_grid(
  phi = phi_vals, 
  alpha = alpha_vals, 
  beta = beta_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(alpha, beta, phi) |> # パラメータごとに並べ替え
  dplyr::mutate(
    dens = dbeta(x = phi, shape1 = alpha, shape2 = beta)
  ) # 確率密度を計算


# ベータ分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = phi, y = dens, color = as.factor(beta))) + # データ
  geom_line(show.legend = FALSE) + # 折れ線グラフ
  gganimate::transition_reveal(beta) + # フレーム
  facet_wrap(. ~ alpha, labeller = label_bquote(alpha==.(alpha))) + # グラフの分割
  coord_cartesian(ylim = c(0, 6)) + # 軸の表示範囲
  labs(title = "Beta Distribution", 
       subtitle = "beta={frame_along}", 
       x = expression(phi), y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(beta_vals)+10, end_pause = 10, fps = 10, width = 1200, height = 900)


### ・βの影響 -----

# 変化する値を指定
alpha_vals <- seq(from = 0.5, to = 10, by = 0.5)
length(alpha_vals) # フレーム数

# 比較する値を指定
beta_vals <- c(0.1, 0.25, 0.5, 1, 1.5, 2, 5, 10, 15)


# パラメータごとにベータ分布を計算
anime_dens_df <- tidyr::expand_grid(
  phi = phi_vals, 
  alpha = alpha_vals, 
  beta = beta_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(beta, alpha, phi) |> # パラメータごとに並べ替え
  dplyr::mutate(
    dens = dbeta(x = phi, shape1 = alpha, shape2 = beta)
  ) # 確率密度を計算


# ベータ分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = phi, y = dens, color = as.factor(alpha))) + # データ
  geom_line(show.legend = FALSE) + # 折れ線グラフ
  gganimate::transition_reveal(alpha) + # フレーム
  facet_wrap(. ~ beta, labeller = label_bquote(beta==.(beta))) + # グラフの分割
  coord_cartesian(ylim = c(0, 6)) + # 軸の表示範囲
  labs(title = "Beta Distribution", 
       subtitle = "alpha={frame_along}", 
       x = expression(phi), y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(alpha_vals)+10, end_pause = 10, fps = 10, width = 1200, height = 900)


# 歪度と尖度の可視化 -------------------------------------------------------------------

# phiがとり得る値を作成
phi_vals <- seq(from = 0, to = 1, length.out = 501)


### ・αの影響 -----

# パラメータとして利用する値を作成
alpha_vals <- seq(from = 0.1, to = 15, by = 0.1)

# 固定するパラメータを指定
beta <- 5


# 歪度を計算
denom_vec    <- 2 * (beta - alpha_vals) * sqrt(alpha_vals + beta + 1)
numer_vec    <- (alpha_vals + beta + 2) * sqrt(alpha_vals * beta)
skewness_vec <- denom_vec / numer_vec

# 尖度を計算
denom_vec    <- 6 * ((alpha_vals - beta)^2 * (alpha_vals + beta + 1) - alpha_vals * beta * (alpha_vals + beta + 2))
numer_vec    <- alpha_vals * beta * (alpha_vals + beta + 2) * (alpha_vals + beta + 3)
kurtosis_vec <- denom_vec / numer_vec

# ラベル用のテキストを作成
label_vec <- paste0(
  "alpha=", alpha_vals, ", beta=", beta, 
  ", skewness=", round(skewness_vec, 3), ", kurtosis=", round(kurtosis_vec, 3)
)

# パラメータごとにベータ分布を計算
anime_dens_df <- tidyr::expand_grid(
  phi = phi_vals, 
  alpha = alpha_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(alpha, phi) |> # パラメータごとに並べ替え
  dplyr::mutate(
    dens = dbeta(x = phi, shape1 = alpha, shape2 = beta), 
    parameter = rep(label_vec, each = length(phi_vals)) |> 
      factor(levels = label_vec) # フレーム切替用ラベル
  ) # 確率密度を計算

# 統計量を格納
anime_stat_df <- tibble::tibble(
  mean = alpha_vals / (alpha_vals + beta), # 期待値
  sd = sqrt(alpha_vals * beta / (alpha_vals + beta)^2 / (alpha_vals + beta + 1)), # 標準偏差
  mode = (alpha_vals - 1) / (alpha_vals + beta - 2), # 最頻値
  parameter = factor(label_vec, levels = label_vec) # フレーム切替用ラベル
) |> # 統計量を計算
  dplyr::mutate(
    sd_m = mean - sd, 
    sd_p = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!sd) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = !parameter, 
    names_to = "type", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(type, pattern = "sd_.", replacement = "sd")) # 期待値±標準偏差のカテゴリを統一


### ・βの影響 -----

# 固定するパラメータを指定
alpha <- 5

# パラメータとして利用する値を作成
beta_vals <- seq(from = 0.1, to = 15, by = 0.1)


# 歪度を計算
denom_vec    <- 2 * (beta_vals - alpha) * sqrt(alpha + beta_vals + 1)
numer_vec    <- (alpha+ beta_vals  + 2) * sqrt(alpha * beta_vals)
skewness_vec <- denom_vec / numer_vec

# 尖度を計算
denom_vec    <- 6 * ((alpha - beta_vals)^2 * (alpha + beta_vals + 1) - alpha * beta_vals * (alpha + beta_vals + 2))
numer_vec    <- alpha * beta_vals * (alpha + beta_vals + 2) * (alpha + beta_vals + 3)
kurtosis_vec <- denom_vec / numer_vec

# ラベル用のテキストを作成
label_vec <- paste0(
  "alpha=", alpha, ", beta=", beta_vals, 
  ", skewness=", round(skewness_vec, 3), ", kurtosis=", round(kurtosis_vec, 3)
)

# パラメータごとにベータ分布を計算
anime_dens_df <- tidyr::expand_grid(
  phi = phi_vals, 
  beta = beta_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(beta, phi) |> # パラメータごとに並べ替え
  dplyr::mutate(
    dens = dbeta(x = phi, shape1 = alpha, shape2 = beta), 
    parameter = rep(label_vec, each = length(phi_vals)) |> 
      factor(levels = label_vec) # フレーム切替用ラベル
  ) # 確率密度を計算

# 統計量を格納
anime_stat_df <- tibble::tibble(
  mean = alpha / (alpha + beta_vals), # 期待値
  sd = sqrt(alpha * beta_vals / (alpha + beta_vals)^2 / (alpha + beta_vals + 1)), # 標準偏差
  mode = (alpha - 1) / (alpha + beta_vals - 2), # 最頻値
  parameter = factor(label_vec, levels = label_vec) # フレーム切替用ラベル
) |> # 統計量を計算
  dplyr::mutate(
    sd_m = mean - sd, 
    sd_p = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!sd) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = !parameter, 
    names_to = "type", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(type, pattern = "sd_.", replacement = "sd")) # 期待値±標準偏差のカテゴリを統一


### ・作図 -----

# 凡例用の設定を作成:(数式表示用)
color_vec <- c(mean = "blue", sd = "orange", mode = "chocolate")
linetype_vec <- c(mean = "dashed", sd = "dotted", mode = "dashed")
label_vec <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))), mode = expression(mode(x)))


# 統計量を重ねた分布のアニメーションを作図
anime_prob_graph <- ggplot() + # データ
  geom_line(data = anime_dens_df, mapping = aes(x = phi, y = dens), 
            color = "#00A968", size = 1) + # 分布
  geom_vline(data = anime_stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 統計量
  gganimate::transition_manual(parameter) + # フレーム
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 5)) + # 軸の表示範囲
  labs(title = "Beta Distribution", 
       subtitle = "{current_frame}", 
       x = expression(phi), y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(alpha_vals), fps = 10, width = 800, height = 600) # (αの影響用)
gganimate::animate(anime_prob_graph, nframes = length(beta_vals), fps = 10, width = 800, height = 600) # (βの影響用)


# 乱数の生成 -------------------------------------------------------------------

### ・サンプリング -----

# パラメータを指定
alpha <- 5
beta  <- 2

# データ数(サンプルサイズ)を指定
N <- 1000


# ベータ分布に従う乱数を生成
phi_n <- rbeta(n = N, shape1 = alpha, shape2 = beta)


### ・乱数の可視化 -----

# サンプルを格納
data_df <- tidyr::tibble(phi = phi_n)

# サンプルのヒストグラムを作成：頻度
ggplot(data = data_df, mapping = aes(x = phi)) + # データ
  geom_histogram(fill = "#00A968", bins = 30) + # 度数
  labs(title = "Beta Distribution", 
       subtitle = paste0("alpha=", alpha, ", beta=", beta, ", N=", N), # (文字列表記用)
       #subtitle = parse(text = paste0("list(alpha==", alpha, ", beta==", beta, ", N==", N, ")")), # (数式表記用)
       x = expression(phi), y = "frequency") # ラベル


# phiのとり得る値を作成
phi_vals <- seq(from = 0, to = 1, length.out = 501)

# ベータ分布を計算
dens_df <- tidyr::tibble(
  phi = phi_vals, # x軸の値
  dens = dbeta(x = phi_vals, shape1 = alpha, shape2 = beta) # 確率密度
)

# サンプルのヒストグラムを作成：密度
ggplot() + 
  geom_histogram(data = data_df, mapping = aes(x = phi, y = ..density..), 
                 fill = "#00A968", bins = 30) + # 密度
  geom_line(data = dens_df, mapping = aes(x = phi, y = dens), 
            color = "darkgreen", size = 1,linetype = "dashed") + # 元の分布
  labs(title = "Beta Distribution", 
       subtitle = parse(text = paste0("list(alpha==", alpha, ", beta==", beta, ", N==", N, ")")), 
       x = expression(phi), y = "density") # ラベル


# 乱数と分布の関係：アニメーションによる可視化 --------------------------------------------------

# パラメータを指定
alpha <- 5
beta  <- 2

# データ数(フレーム数)を指定
N <- 100


# ベータ分布に従う乱数を生成
phi_n <- rbeta(n = N, shape1 = alpha, shape2 = beta)


# サンプルを複製して格納
anime_freq_df <- tibble::tibble(
  phi = rep(phi_n, times = N), # サンプル
  n = rep(1:N, times = N), # データ番号
  frame = rep(1:N, each = N) # フレーム番号
) |> 
  dplyr::filter(n <= frame) |> # サンプリング回数以前のサンプルを抽出
  dplyr::mutate(
    parameter = paste0("alpha=", alpha, ", beta=", beta, ", N=", frame) |> 
      factor(levels = paste0("alpha=", alpha, ", beta=", beta, ", N=", 1:N))
  ) # フレーム切替用ラベルを追加

# サンプルを格納
anime_data_df <- tidyr::tibble(
  phi = phi_n, 
  n = 1:N, 
  parameter = paste0("alpha=", alpha, ", beta=", beta, ", N=", n) |> 
    factor(levels = paste0("alpha=", alpha, ", beta=", beta, ", N=", 1:N)) # フレーム切替用ラベル
)
anime_data_df <- tidyr::tibble(
  phi = phi_n, 
  parameter = paste0("alpha=", alpha, ", beta=", beta, ", N=", 1:N) |> 
    factor(levels = paste0("alpha=", alpha, ", beta=", beta, ", N=", 1:N))
)

# phiのとり得る値を作成
phi_vals <- seq(from = 0, to = 1, length.out = 501)

# ベータ分布を計算
dens_df <- tidyr::tibble(
  phi = phi_vals, # x軸の値
  density = dbeta(x = phi_vals, shape1 = alpha, shape2 = beta) # 確率密度
)


# サンプルのヒストグラムを作成：頻度
anime_freq_graph <- ggplot() + 
  geom_histogram(data = anime_freq_df, mapping = aes(x = phi), 
                 breaks = seq(from = 0, to = 1, length.out = 30), 
                 fill = "#00A968") + # 度数
  geom_point(data = anime_data_df, mapping = aes(x = phi, y = 0), 
            color = "orange", size = 6) + # 
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Beta Distribution", 
       subtitle = "{current_frame}", 
       x = expression(phi), y = "frequency") # ラベル

# サンプルのヒストグラムを作成：密度
anime_freq_graph <- ggplot() + 
  geom_histogram(data = anime_freq_df, mapping = aes(x = phi, y = ..density..), 
                 breaks = seq(from = 0, to = 1, length.out = 30), 
                 fill = "#00A968") + # 度数
  geom_line(data = dens_df, mapping = aes(x = phi, y = density), 
            color = "darkgreen", size = 1, linetype = "dashed") + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = phi, y = 0), 
             color = "orange", size = 6) + # 
  gganimate::transition_manual(parameter) + # フレーム
  coord_cartesian(ylim = c(0, 5)) + # 軸の表示範囲
  labs(title = "Beta Distribution", 
       subtitle = "{current_frame}", 
       x = expression(phi), y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 10, width = 800, height = 600)


# 分布の生成 -------------------------------------------------------------------

### ・パラメータの生成 -----

# パラメータを指定
alpha <- 5
beta  <- 2

# データ数(サンプルサイズ)を指定
N <- 10


# ベータ分布に従う乱数を生成
phi_n <- rbeta(n = N, shape1 = alpha, shape2 = beta) |> 
  sort()


# パラメータを格納
param_df <- tibble::tibble(
  phi = phi_n, 
  label = round(phi, 3) |> 
    factor() # 色分け用ラベル
)

# phiがとり得る値を作成
phi_vals <- seq(from = 0, to = 1, length.out = 501)

# ベータ分布を計算
beta_df <- tibble::tibble(
  phi = phi_vals, # 確率変数
  density = dbeta(x = phi_vals, shape1 = alpha, shape2 = beta) # 確率密度
)

# ベータ分布の期待値を計算
E_phi <- alpha / (alpha + beta)


# ベータ分布を作図
beta_graph <- ggplot() + 
  geom_line(data = beta_df, mapping = aes(x = phi, y = density), 
            color = "#00A968", size = 1) + # パラメータの生成分布
  geom_point(data = param_df, mapping = aes(x = phi, y = 0, color = label), 
             size = 6, alpha = 0.5, show.legend = FALSE) + # パラメータのサンプル
  geom_vline(mapping = aes(xintercept = E_phi), 
             color = "red", size = 1, linetype = "dashed") + # 
  labs(title = "Beta Distribution", 
       subtitle = parse(text = paste0("list(alpha==", alpha, ", beta==", beta, ")")), 
       x = expression(phi), y = "density")
beta_graph


### ・分布の作図：ベルヌーイ分布 -----

# xがとり得る値を作成
x_vals <- 0:1

# パラメータのサンプルごとにベルヌーイ分布を計算
res_bern_df <- tidyr::expand_grid(
  x = x_vals, # 確率変数
  phi = phi_n # パラメータ
) |> # 全ての組み合わせを作成
  dplyr::arrange(phi, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    probability = dbinom(x = x, size = 1, prob = phi), # 確率
    label = round(phi, 3) |> 
      factor() # 色分け用ラベル
  ) # 確率を計算

# パラメータの期待値により二項分布を計算
E_bern_df <- tibble::tibble(
  x = x_vals, 
  probability = dbinom(x = x, size = 1, prob = E_phi)
)


# サンプルごとにベルヌーイ分布を作図
bern_graph <- ggplot() + 
  geom_bar(data = res_bern_df, mapping = aes(x = x, y = probability, fill = label), 
           stat = "identity", show.legend = FALSE) + # サンプルによる分布
  geom_bar(data = E_bern_df, mapping = aes(x = x, y = probability), 
           stat = "identity", fill = NA, color = "red", linetype = "dashed") + # 期待値による分布
  facet_wrap(. ~ phi, nrow = 2, labeller = label_bquote(phi==.(round(phi, 3)))) + # グラフを分割
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  ylim(c(0, 1)) + # y軸の表示範囲
  labs(title = "Bernoulli Distribution", 
       subtitle = parse(text = paste0("E(phi)==", round(E_phi, 3))), 
       fill = expression(phi), 
       x = "x", y = "probability") # ラベル
bern_graph

# グラフを並べて描画
beta_graph / bern_graph


### ・分布の作図：二項分布 -----

# 試行回数を指定
M <- 10


# xがとり得る値を作成
x_vals <- 0:M

# パラメータのサンプルごとに二項分布を計算
res_binom_df <- tidyr::expand_grid(
  x = x_vals, # 確率変数
  phi = phi_n # パラメータ
) |> # 全ての組み合わせを作成
  dplyr::arrange(phi, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    probability = dbinom(x = x, size = M, prob = phi), 
    label = round(phi, 3) |> 
      factor() # 色分け用ラベル
  ) # 確率を計算

# パラメータの期待値により二項分布を計算
E_binom_df <- tibble::tibble(
  x = x_vals, 
  probability = dbinom(x = x, size = M, prob = E_phi)
)

# サンプルごとに二項分布を作図
binom_graph <- ggplot() + 
  geom_point(data = E_binom_df, mapping = aes(x = x, y = probability), 
             color = "red", size = 3) + # 期待値による分布
  geom_line(data = E_binom_df, mapping = aes(x = x, y = probability), 
            color = "red", size = 1, linetype = "dashed") + # 期待値による分布
  geom_point(data = res_binom_df, mapping = aes(x = x, y = probability, color = label), 
             size = 3, alpha = 0.5) + # サンプルによる分布
  geom_line(data = res_binom_df, mapping = aes(x = x, y = probability, color = label), 
            size = 1, alpha = 0.5) + # サンプルによる分布
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  theme(panel.grid.minor.x = element_blank()) + # 図の体裁
  guides(color = guide_legend(override.aes = list(alpha = 1))) + # 凡例の体裁
  labs(title = "Binomial Distribution", 
       subtitle = parse(text = paste0("list(E(phi)==", round(E_phi, 3), ", M==", M, ")")), 
       color = expression(phi), 
       x = "x", y = "probability") # タイトル
binom_graph

# グラフを並べて描画
beta_graph / binom_graph + 
  patchwork::plot_layout(guides = "collect")


