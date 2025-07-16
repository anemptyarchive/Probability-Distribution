
# 確率の計算 -------------------------------------------------------------------

# パラメータを指定
phi <- 0.35

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
log_C <- lgamma(M + 1) - lgamma(M - x + 1) - lgamma(x + 1)
log_prob <- log_C + x * log(phi) + (M - x) * log(1 - phi)
prob <- exp(log_prob)
prob; log_prob

# 二項分布の関数により確率を計算
prob <- dbinom(x = x, size = M, prob = phi)
prob

# 二項分布の対数をとった関数により確率を計算
log_prob <- dbinom(x = x, size = M, prob = phi, log = TRUE)
prob <- exp(log_prob)
prob; log_prob

# 多項分布の関数により確率を計算
prob <- dmultinom(x = x_v, size = M, prob = phi_v)
prob

# 多項分布の対数をとった関数により確率を計算
log_prob <- dmultinom(x = x_v, size = M, prob = phi_v, log = TRUE)
prob <- exp(log_prob)
prob; log_prob


# 統計量の計算 -------------------------------------------------------------------

# パラメータを指定
phi <- 0.35

# 試行回数を指定
M <- 10


# 期待値を計算
E_x <- M * phi
E_x

# 分散を計算
V_x <- M * phi * (1 - phi)
V_x

# 最頻値を計算:(注:複数の場合も1つしか返さない)
mode_x <- floor(phi * (M + 1))
mode_x


# 歪度を計算
skewness <- (1 - 2 * phi) / sqrt(M * phi * (1 - phi))
skewness

# 尖度を計算
kurtosis <- (1 - 6 * phi * (1 - phi)) / (M * phi * (1 - phi))
kurtosis


# グラフの作成 ------------------------------------------------------------------

# パラメータを指定
phi <- 0.35

# 試行回数を指定
M <- 10


# xがとり得る値を作成
x_vals <- 0:M

# 二項分布を計算
prob_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  probability = dbinom(x = x_vals, size = M, prob = phi) # 確率
)


# 二項分布のグラフを作成
ggplot(data = prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
  scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  labs(title = "Binomial Distribution", 
       subtitle = paste0("phi=", phi, ", M=", M), # (文字列表記用)
       #subtitle = parse(text = paste0("list(phi==", phi, ", M==", M, ")")), # (数式表記用)
       x = "x", y = "probability") # ラベル


# 補助線用の統計量を計算
E_x <- M * phi
s_x <- sqrt(M * phi * (1 - phi))
mode_x <- floor(phi * (M + 1))

# 統計量を重ねた二項分布のグラフを作成:線のみ
ggplot(data = prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 分布
  geom_vline(xintercept = E_x, color = "blue", size = 1, linetype = "dashed") + # 期待値
  geom_vline(xintercept = c(E_x-s_x, E_x+s_x), color = "orange", size = 1, linetype = "dotted") + # 期待値 ± 標準偏差
  geom_vline(xintercept = mode_x, color = "chocolate", size = 1, linetype = "dashed") + # 最頻値
  scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  labs(title = "Binomial Distribution", 
       subtitle = parse(text = paste0("list(phi==", phi, ", M==", M, ")")), 
       x = "x", y = "probability") # ラベル


# 統計量を格納
stat_df <- tibble::tibble(
  statistic = c(E_x, E_x-s_x, E_x+s_x, mode_x), # 統計量
  type = c("mean", "sd", "sd", "mode") # 色分け用ラベル
)

# 凡例用の設定を作成:(数式表示用)
color_vec <- c(mean = "blue", sd = "orange", mode = "chocolate")
linetype_vec <- c(mean = "dashed", sd = "dotted", mode = "dashed")
label_vec <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))), mode = expression(mode(x)))

# 統計量を重ねた二項分布のグラフを作成:凡例付き
ggplot() + # データ
  geom_bar(data = prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", fill = "#00A968") + # 分布
  geom_vline(data = stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 統計量
  scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Binomial Distribution", 
       subtitle = parse(text = paste0("list(phi==", phi, ", M==", M, ")")), 
       x = "x", y = "probability") # ラベル


