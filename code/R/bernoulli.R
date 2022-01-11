
# ベルヌーイ分布 -----------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)
library(gganimate)


### 確率の計算 -----

# パラメータを指定
phi <- 0.3

# 確率変数の値を指定
x <- 1

# ベクトルに変換
phi_v <- c(1 - phi, phi)
x_v <- c(1 - x, x)
phi_v; x_v

# 定義式により確率を計算
prob <- phi^x * (1 - phi)^(1 - x)
prob

# 対数をとった定義式により確率を計算
ln_prob <- x * log(phi) + (1 - x) * log(1 - phi)
prob <- exp(ln_prob)
prob; ln_prob

# 二項分布の関数により確率を計算
prob <- dbinom(x = x, size = 1, prob = phi)
prob

# 二項分布の対数をとった関数により確率を計算
ln_prob <- dbinom(x = x, size = 1, prob = phi, log = TRUE)
prob <- exp(ln_prob)
prob; ln_prob

# 多項分布の関数により確率を計算
prob <- dmultinom(x = x_v, size = 1, prob = phi_v)
prob

# 多項分布の対数をとった関数により確率を計算
ln_prob <- dmultinom(x = x_v, size = 1, prob = phi_v, log = TRUE)
prob <- exp(ln_prob)
prob; ln_prob

# インデックスにより確率を抽出
prob <- phi_v[x+1]
prob


### 統計量の計算 -----

# パラメータを指定
phi <- 0.3

# 平均を計算
E_x <- phi
E_x

# 分散を計算
V_x <- phi * (1 - phi)
V_x


### グラフの作成 -----

# パラメータを指定
phi <- 0.3

# ベルヌーイ分布の情報を格納
prob_df <- tidyr::tibble(
  x = 0:1, # 確率変数
  probability = c(1 - phi, phi) # 確率
)

# ベルヌーイ分布を作図
ggplot(data = prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # 分布
#  geom_vline(xintercept = E_x, color = "orange", size = 1, linetype = "dashed") + # 平均
#  geom_vline(xintercept = E_x - sqrt(V_x), color = "orange", size = 1, linetype = "dotted") + # 平均 - 標準偏差
#  geom_vline(xintercept = E_x + sqrt(V_x), color = "orange", size = 1, linetype = "dotted") + # 平均 + 標準偏差
  scale_x_continuous(breaks = 0:1, labels = 0:1) + # x軸目盛
  labs(title = "Bernoulli Distribution", 
       subtitle = paste0("phi=", phi)) # ラベル


### パラメータと分布の形状の関係 -----

# 作図用のphiの値を作成
phi_vals <- seq(from = 0, to = 1, by = 0.01)

# phiの値ごとに分布を計算
anime_prob_df <- tidyr::tibble()
for(phi in phi_vals) {
  # ベルヌーイ分布の情報を格納
  tmp_prob_df <- tidyr::tibble(
    x = 0:1, # 確率変数
    probability = c(1 - phi, phi), # 確率
    parameter = paste0("phi=", phi) %>% 
      as.factor() # フレーム切替用のラベル
  )
  
  # 結果を結合
  anime_prob_df <- rbind(anime_prob_df, tmp_prob_df)
}

# アニメーション用のベルヌーイ分布を作図
anime_prob_graph <- ggplot(data = anime_prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # 分布
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = 0:1, labels = 0:1) + # x軸目盛
  labs(title = "Bernoulli Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(phi_vals), fps = 100)


### 乱数の生成 ----

## 乱数の可視化

# パラメータを指定
phi <- 0.3

# データ数を指定
N <- 1000

# ベルヌーイ分布に従う乱数を生成
x_n <- rbinom(n = N, size = 1, prob = phi)

# 乱数を集計して格納
freq_df <- tidyr::tibble(
  x = 0:1, # 確率変数
  frequency = c(sum(x_n == 0), sum(x_n == 1)), # 度数
  proportion = frequency / N # 構成比
)

# サンプルのヒストグラムを作成
ggplot(data = freq_df, mapping = aes(x = x, y = frequency)) + # データ
  geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # ヒストグラム
  scale_x_continuous(breaks = 0:1, labels = 0:1) + # x軸目盛
  labs(title = "Bernoulli Distribution", 
       subtitle = paste0("phi=", phi, ", N=", N, "=(", sum(x_n == 0), ", ", sum(x_n == 1), ")")) # ラベル

# サンプルの構成比を作図
ggplot() + 
  geom_bar(data = freq_df, mapping = aes(x = x, y = proportion), 
           stat = "identity", position = "dodge", fill = "#00A968") + # 構成比
  geom_bar(data = prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", position = "dodge", alpha = 0, color = "darkgreen", linetype = "dashed") + # 真の分布
  scale_x_continuous(breaks = 0:1, labels = 0:1) + # x軸目盛
  labs(title = "Bernoulli Distribution", 
       subtitle = paste0("phi=", phi, ", N=", N, "=(", sum(x_n == 0), ", ", sum(x_n == 1), ")")) # ラベル


## アニメーションによる可視化

# データ数を指定
N <- 100

# 乱数を1つずつ生成
x_n <- rep(NA, times = N)
anime_freq_df <- tidyr::tibble()
anime_data_df <- tidyr::tibble()
anime_prob_df <- tidyr::tibble()
for(n in 1:N) {
  # ベルヌーイ分布に従う乱数を生成
  x_n[n] <- rbinom(n = 1, size = 1, prob = phi)
  
  # ラベル用のテキストを作成
  label_text <- paste0(
    "phi=", phi, ", N=", n, "=(", sum(x_n[1:n] == 0), ", ", sum(x_n[1:n] == 1), ")"
  )
  
  # n個の乱数を集計して格納
  tmp_freq_df <- tidyr::tibble(
    x = 0:1, # 確率変数
    frequency = c(sum(x_n[1:n] == 0), sum(x_n[1:n] == 1)), # 度数
    proportion = frequency / n, # 構成比
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # n番目の乱数を格納
  tmp_data_df <- tidyr::tibble(
    x = x_n[n], # サンプル
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # n回目のラベルを付与
  tmp_prob_df <- prob_df %>% 
    mutate(parameter = as.factor(label_text))
  
  # 結果を結合
  anime_freq_df <- rbind(anime_freq_df, tmp_freq_df)
  anime_data_df <- rbind(anime_data_df, tmp_data_df)
  anime_prob_df <- rbind(anime_prob_df, tmp_prob_df)
}


# アニメーション用のサンプルのヒストグラムを作成
anime_hist_graph <- ggplot() + 
  geom_bar(data = anime_freq_df, mapping = aes(x = x, y = frequency), 
           stat = "identity", position = "dodge", fill = "#00A968") + # ヒストグラム
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size= 5) + # サンプル
  scale_x_continuous(breaks = 0:1, labels = 0:1) + # x軸目盛
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Bernoulli Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_hist_graph, nframes = N, fps = 100)


# アニメーション用のサンプルの構成比を作図
anime_prop_graph <- ggplot() + 
  geom_bar(data = anime_freq_df, mapping = aes(x = x, y = proportion), 
           stat = "identity", position = "dodge", fill = "#00A968") + # 構成比
  geom_bar(data = anime_prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", position = "dodge", alpha = 0, color = "darkgreen", linetype = "dashed") + # 真の分布
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size= 5) + # サンプル
  scale_x_continuous(breaks = 0:1, labels = 0:1) + # x軸目盛
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Bernoulli Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_prop_graph, nframes = N, fps = 100)


