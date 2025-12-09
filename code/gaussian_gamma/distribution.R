
# 確率密度の計算 -----------------------------------------------------------------

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6

# 確率変数の値を指定
mu     <- 1.5
lambda <- 2.5


# 定義式により確率密度を計算
C_N      <- sqrt(beta * lambda / 2 / pi)
dens_N   <- C_N * exp(-0.5 * beta * lambda * (mu - m)^2)
C_Gam    <- b^a / gamma(a)
dens_Gam <- C_Gam * lambda^(a - 1) * exp(-b * lambda)
dens     <- dens_N * dens_Gam
dens

# 対数をとった定義式により確率密度を計算
log_C_N      <- 0.5 * (log(beta * lambda) - log(2 * pi))
log_dens_N   <- log_C_N - 0.5 * beta * lambda * (mu - m)^2
log_C_Gam    <- a * log(b) - lgamma(a)
log_dens_Gam <- log_C_Gam + (a - 1) * log(lambda) - b * lambda
log_dens     <- log_dens_N + log_dens_Gam
dens         <- exp(log_dens)
dens; log_dens

# 関数により確率密度を計算
dens_N   <- dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda))
dens_Gam <- dgamma(x = lambda, shape = a, rate = b)
dens     <- dens_N * dens_Gam
dens

# 対数をとった関数により確率密度を計算
log_dens_N   <- dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda), log = TRUE)
log_dens_Gam <- dgamma(x = lambda, shape = a, rate = b, log = TRUE)
log_dens     <- log_dens_N + log_dens_Gam
dens         <- exp(log_dens)
dens; log_dens


# 統計量の計算 -----------------------------------------------------------------

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6


# 平均を計算
E_mu     <- m
E_lambda <- a / b
E_mu; E_lambda

# 分散を計算
V_mu     <- 1 / beta / E_lambda
V_lambda <- a / b^2
V_mu; V_lambda

# 最頻値を計算
mode_mu     <- m
mode_lambda <- (a - 1) / b
mode_mu; mode_lambda


# グラフの作成-----------------------------------------------------------------

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6


# 平均パラメータの期待値を計算
E_mu <- m

# 精度パラメータの期待値を計算
E_lambda <- a / b

# μの値を作成
mu_vals <- seq(
  from = E_mu - 1/sqrt(beta*E_lambda) * 4, 
  to = E_mu + 1/sqrt(beta*E_lambda) * 4, 
  length.out = 200
)

# λの値を作成
lambda_vals <- seq(from = 0, to = E_lambda * 3, length.out = 200)

# ガウス-ガンマ分布を計算
dens_df <- tidyr::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals # 確率変数λ
) |> # 確率変数(μとλの格子点)を作成
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens # 確率密度
  )

# ガウス-ガンマ分布を作図
ggplot() + 
  geom_contour(data = dens_df, aes(x = mu, y = lambda, z = density, color = ..level..)) + # 等高線
  #geom_contour_filled(data = dens_df, aes(x = mu, y = lambda, z = density, fill = ..level..), 
  #                    alpha = 0.8) + # 塗りつぶし等高線
  labs(
    title = "Gaussian-Gamma Distribution", 
    #subtitle = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b), # (文字列表記用)
    subtitle = parse(text = paste0("list(m==", m, ", beta==", beta, ", a==", a, ", b==", b, ")")), # (数式表記用)
    color = "density", fill = "density", 
    x = expression(mu), y = expression(lambda)
  ) # ラベル


# μに関する統計量を格納
stat_mu_df <- tibble::tibble(
  lambda = lambda_vals, # 確率変数λ
  mean = m, # 期待値
  sd = 1 / sqrt(beta * lambda_vals) # 標準偏差
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!sd) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = !lambda, 
    names_to = "group", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(group, pattern = "sd_.*", replacement = "sd")
  ) # 期待値±標準偏差のカテゴリを統一

# λに関する統計量を格納
stat_lambda_df <- tibble::tibble(
  mean = a / b, # 期待値
  sd = sqrt(a / b^2), # 標準偏差
  mode = (a - 1) / b # 最頻値
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!sd) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = dplyr::everything(), 
    names_to = "type", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(type, pattern = "sd_.*", replacement = "sd")
  ) # 期待値±標準偏差のカテゴリを統一

# 凡例用の設定を作成:(数式表示用)
color_vec    <- c(mean = "blue", sd = "orange", mode = "chocolate")
linetype_vec <- c(mean = "dashed", sd = "dotted", mode = "dashed")
label_vec    <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))), mode = expression(mode(x)))

# 統計量を重ねたガウス-ガンマ分布を作図
ggplot() + 
  geom_contour_filled(data = dens_df, aes(x = mu, y = lambda, z = density, fill = ..level..), 
                      alpha = 0.8) + # 分布
  geom_line(data = stat_mu_df, mapping = aes(x = statistic, y = lambda, color = type, linetype = type, group = group), 
            size = 1) + # μの統計量
  geom_hline(data = stat_lambda_df, mapping = aes(yintercept = statistic, color = type, linetype = type), 
             size = 1) + # λの統計量
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  guides(color = guide_legend(override.aes = list(size = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁:凡例
  coord_cartesian(xlim = c(min(mu_vals), max(mu_vals))) + # x軸の表示範囲
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = parse(text = paste0("list(m==", m, ", beta==", beta, ", a==", a, ", b==", b, ")")), 
       color = "statistic", fill = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル


