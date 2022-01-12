
# 二項分布 --------------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)


### 確率の計算 -----

# パラメータを指定
phi <- 0.3

# 試行回数を指定
M <- 10

# 確率変数の値を指定:(x <= M)
x <- 3

# ベクトルに変換
phi_v <- c(1 - phi, phi)
x_v <- c(M - x, x)


# 定義式により確率を計算
C <- gamma(M + 1) / gamma(M - x + 1) / gamma(x + 1)
prob <- C * phi^x * (1 - phi)^(M - x)
prob

# 対数をとった定義式により確率を計算
ln_C <- lgamma(M + 1) - lgamma(M - x + 1) - lgamma(x + 1)
ln_prob <- ln_C + x * log(phi) + (M - x) * log(1 - phi)
prob <- exp(ln_prob)
prob; ln_prob

# 二項分布の関数により確率を計算
prob <- dbinom(x = x, size = M, prob = phi)
prob

# 二項分布の対数をとった関数により確率を計算
ln_prob <- dbinom(x = x, size = M, prob = phi, log = TRUE)
prob <- exp(ln_prob)
prob; ln_prob

# 多項分布の関数により確率を計算
prob <- dmultinom(x = x_v, size = M, prob = phi_v)
prob

# 多項分布の対数をとった関数により確率を計算
ln_prob <- dmultinom(x = x_v, size = M, prob = phi_v, log = TRUE)
prob <- exp(ln_prob)
prob; ln_prob


### 統計量の計算 -----

# パラメータを指定
phi <- 0.3

# 試行回数を指定
M <- 10

# 平均を計算
E_x <- M * phi
E_x

# 分散を計算
V_x <- M * phi * (1 - phi)
V_x


### グラフの作成 -----

# パラメータを指定
phi <- 0.3

# 試行回数を指定
M <- 10

# 作図用のxの値を作成
x_vals <- 0:M

# 二項分布の情報を格納
prob_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  probability = dbinom(x = x_vals, size = M, prob = phi) # 確率
)

# 二項分布のグラフを作成
ggplot(data = prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # 分布
#  geom_vline(xintercept = E_x, color = "orange", size = 1, linetype = "dashed") + # 平均
#  geom_vline(xintercept = E_x - sqrt(V_x), color = "orange", size = 1, linetype = "dotted") + # 平均 - 標準偏差
#  geom_vline(xintercept = E_x + sqrt(V_x), color = "orange", size = 1, linetype = "dotted") + # 平均 + 標準偏差
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  labs(title = "Binomial Distribution", 
       subtitle = paste0("phi=", phi, ", M=", M)) # ラベル


### パラメータと分布の形状の関係:並べて比較 -----

# パラメータを指定
phi_vals <- c(0.1, 0.33, 0.5, 0.8, 0.9)

# 試行回数を指定
M <- 100

# 作図用のxの値を作成
x_vals <- 0:M

# phiの値ごとに分布を計算
probs_df <- tidyr::tibble()
for(phi in phi_vals) {
  # 二項分布の情報を格納
  tmp_prob_df <- tidyr::tibble(
    x = x_vals, 
    probability = dbinom(x = x_vals, size = M, prob = phi), 
    parameter = paste0("phi=", phi, ", M=", M) %>% 
      as.factor() # 作図用のラベル
  )
  
  # 結果を結合
  probs_df <- rbind(probs_df, tmp_prob_df)
}

# 二項分布のグラフを作成
ggplot(data = probs_df, mapping = aes(x = x, y = probability, fill = parameter, color = parameter)) + # データ
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  scale_fill_manual(values = c("#FFC0CB", "#FF0000", "#FFFF00", "#EE82EE", "#7FFF00")) + # 塗りつぶし色(不必要)
  scale_color_manual(values = c("#FFC0CB", "#FF0000", "#FFFF00", "#EE82EE", "#7FFF00")) + # 枠の色(不必要)
  theme_dark() + # 背景色(不必要)
  labs(title = "Binomial Distribution") # タイトル


### パラメータと分布の形状の関係：アニメーション -----

## phiを変更した場合

# 作図用のphiの値を作成
phi_vals <- seq(from = 0, to = 1, by = 0.01)

# phiの値ごとに分布を計算
anime_prob_df <- tidyr::tibble()
for(phi in phi_vals) {
  # 二項分布の情報を格納
  tmp_prob_df <- tidyr::tibble(
    x = x_vals, # 確率変数
    probability = dbinom(x = x_vals, size = M, prob = phi), # 確率
    parameter = paste0("phi=", phi, ", M=", M) %>% 
      as.factor() # フレーム切替用のラベル
  )
  
  # 結果を結合
  anime_prob_df <- rbind(anime_prob_df, tmp_prob_df)
}

# アニメーション用の二項分布を作図
anime_prob_graph <- ggplot(data = anime_prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # 分布
#  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Binomial Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(phi_vals), fps = 100)


## Mを変更した場合

# パラメータを指定
phi <- 0.3

# 試行回数の最大値を指定
M_max <- 100

# Mの値ごとに確率を計算
anime_prob_df <- tidyr::tibble()
for(M in 1:M_max) {
  # 作図用のxの値を作成
  x_vals <- 0:M
  
  # 二項分布の情報を格納
  tmp_prob_df <- tidyr::tibble(
    x = x_vals, # 表の回数
    prob = dbinom(x = x_vals, size = M, prob = phi), # 確率
    parameter = paste0("phi=", phi, ", M=", M) %>% 
      as.factor() # フレーム切替用ラベル
  )
  
  # 計算結果を結合
  anime_prob_df <- rbind(anime_prob_df, tmp_prob_df)
}

# アニメーション用の二項分布のグラフを作成
anime_prob_graph <- ggplot(data = anime_prob_df, mapping = aes(x = x, y = prob)) + # データ
  geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # 棒グラフ
#  scale_x_continuous(breaks = 0:M_max, labels = 0:M_max) + # x軸目盛
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Binomial Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = M_max, fps = 100)


### 乱数の生成 -----

## 乱数の可視化

# パラメータを指定
phi <- 0.3

# 試行回数を指定
M <- 10

# データ数を指定
N <- 1000

# 二項分布に従う乱数を生成
x_n <- rbinom(n = N, size = M, prob = phi)

# 乱数を集計して格納
freq_df <- tidyr::tibble(x = x_n) %>% # 乱数を格納
  dplyr::count(x, name = "frequency") %>% # 頻度を測定
  dplyr::right_join(tidyr::tibble(x = 0:M), by = "x") %>% # 確率変数列を追加
  dplyr::mutate(frequency = tidyr::replace_na(frequency, 0)) %>% # サンプルにない場合のNAを0に置換
  dplyr::mutate(proportion = frequency / N) # 構成比を計算

# サンプルのヒストグラムを作成
ggplot(data = freq_df, mapping = aes(x = x, y = frequency)) + # データ
  geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # ヒストグラム
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  labs(title = "Binomial Distribution", 
       subtitle = paste0("phi=", phi, ", M=", M, 
                         ", N=", N, "=(", paste0(freq_df[["frequency"]], collapse = ", "), ")")) # ラベル

# サンプルの構成比を作図
ggplot() + 
  geom_bar(data = freq_df, mapping = aes(x = x, y = proportion), 
           stat = "identity", position = "dodge", fill = "#00A968") + # 構成比
  geom_bar(data = prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", position = "dodge", alpha = 0, color = "darkgreen", linetype = "dashed") + # 真の分布
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  labs(title = "Binomial Distribution", 
       subtitle = paste0("phi=", phi, ", M=", M, 
                         ", N=", N, "=(", paste0(freq_df[["frequency"]], collapse = ", "), ")")) # ラベル


## アニメーションによる可視化

# データ数を指定
N <- 100

# 乱数を1つずつ生成
x_n <- rep(NA, times = N)
anime_freq_df <- tidyr::tibble()
anime_data_df <- tidyr::tibble()
anime_prob_df <- tidyr::tibble()
for(n in 1:N) {
  # 二項分布に従う乱数を生成
  x_n[n] <- rbinom(n = 1, size = M, prob = phi)
  
  # n個の乱数を集計して格納
  tmp_freq_df <- tidyr::tibble(x = x_n[1:n]) %>% # 乱数を格納
    dplyr::count(x, name = "frequency") %>% # 頻度を測定
    dplyr::right_join(tidyr::tibble(x = 0:M), by = "x") %>% # 確率変数列を追加
    dplyr::mutate(frequency = tidyr::replace_na(frequency, 0)) %>% # サンプルにない場合のNAを0に置換
    dplyr::mutate(proportion = frequency / n) # 構成比を計算
  
  # ラベル用のテキストを作成
  label_text <- paste0(
    "phi=", phi, ", N=", n, "=(", paste0(tmp_freq_df[["frequency"]], collapse = ", "), ")"
  )
  
  # フレーム切替用のラベルを付与
  tmp_freq_df <- tmp_freq_df %>% 
    dplyr::mutate(parameter = as.factor(label_text))
  
  # n番目の乱数を格納
  tmp_data_df <- tidyr::tibble(
    x = x_n[n], 
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # n回目のラベルを付与
  tmp_prob_df <- prob_df %>% 
    dplyr::mutate(parameter = as.factor(label_text))
  
  # 結果を結合
  anime_freq_df <- rbind(anime_freq_df, tmp_freq_df)
  anime_data_df <- rbind(anime_data_df, tmp_data_df)
  anime_prob_df <- rbind(anime_prob_df, tmp_prob_df)
}


# アニメーション用のサンプルのヒストグラムを作成
anime_freq_graph <- ggplot() + 
  geom_bar(data = anime_freq_df, mapping = aes(x = x, y = frequency), 
           stat = "identity", position = "dodge", fill = "#00A968") + # ヒストグラム
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size= 5) + # サンプル
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Binomial Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 100)


# アニメーション用のサンプルの構成比を作図
anime_prop_graph <- ggplot() + 
  geom_bar(data = anime_freq_df, mapping = aes(x = x, y = proportion), 
           stat = "identity", position = "dodge", fill = "#00A968") + # 構成比
  geom_bar(data = anime_prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", position = "dodge", alpha = 0, color = "darkgreen", linetype = "dashed") + # 真の分布
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size= 5) + # サンプル
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Binomial Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_prop_graph, nframes = N, fps = 100)


