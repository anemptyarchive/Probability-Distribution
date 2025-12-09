
# 分布の生成 -------------------------------------------------------------------

### ・パラメータの生成 -----

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6

# 分布の数(サンプルサイズ)を指定
N <- 10


# ガンマ分布の精度パラメータを生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# ガウス分布の平均パラメータを生成
mu_n <- rnorm(n = N, mean = m, sd = 1/sqrt(beta*lambda_n))

# パラメータを格納
param_df <- tibble::tibble(
  mu = mu_n, 
  lambda = lambda_n, 
  parameter = paste0("mu=", round(mu_n, 2), ", lambda=", round(lambda_n, 3)) |> 
    factor() # 色分け用ラベル
)


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
gaussian_gamma_df <- tidyr::expand_grid(
  mu = mu_vals, # 確率変数mu
  lambda = lambda_vals # 確率変数λ
) |> # 確率変数(μとλの格子点)を作成
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens # 確率密度
  )


# ガウス-ガンマ分布を作図
gaussian_gamma_graph <- ggplot() + 
  geom_contour_filled(data = gaussian_gamma_df, mapping = aes(x = mu, y = lambda, z = density, fill = ..level..), 
                      alpha = 0.6) + # パラメータの生成分布
  geom_point(mapping = aes(x = E_mu, y = E_lambda), 
             color = "red", size = 6, shape = 4) + # パラメータの期待値
  geom_point(data = param_df, mapping = aes(x = mu, y = lambda, color = parameter), 
             alpha = 0.8, size = 6, show.legend = FALSE) + # パラメータのサンプル
  labs(
    title = "Gaussian-Gamma Distribution", 
    subtitle = parse(text = paste0("list(m==", m, ", beta==", beta, ", a==", a, ", b==", b, ")")), 
    fill = "density", 
    x = expression(mu), y = expression(lambda)
  ) # ラベル
gaussian_gamma_graph


### ・分布の作図：1次元ガウス分布 -----

# xの値を作成
x_vals <- mu_vals

# パラメータの期待値によるガウス分布を計算
E_gaussian_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = E_mu, sd = 1/sqrt(E_lambda)) # 確率密度
)

# パラメータのサンプルごとにガウス分布を計算
res_gaussian_df <- tidyr::expand_grid(
  x = x_vals, # 確率変数
  n = 1:N # パラメータ番号
) |> # 全ての組み合わせを作成
  dplyr::arrange(n, x) |> # パラメータのごとに並べ替え
  dplyr::mutate(
    mu = mu_n[n], # パラメータμ
    lambda = lambda_n[n], # パラメータλ
    density = dnorm(x = x, mean = mu, sd = 1/sqrt(lambda)), # 確率密度
    parameter = paste0("mu=", round(mu, 2), ", lambda=", round(lambda, 3)) |> 
      factor() # 色分け用ラベル
  )

# 凡例用のラベルを作成:(数式表示用)
label_vec <- res_gaussian_df[["parameter"]] |> 
  stringr::str_replace_all(pattern = "=", replacement = "==") |> # 等号表示用の記法に変換
  (\(.){paste0("list(", ., ")")})() |> # カンマ表示用の記法に変換
  unique() |> # 重複を除去
  parse(text = _) # expression化
names(label_vec) <- unique(res_gaussian_df[["parameter"]]) # ggplotに指定する文字列に対応する名前付きベクトルに変換

# サンプルによる1次元ガウス分布を作図
gaussian_graph <- ggplot() + 
  geom_line(data = E_gaussian_df, mapping = aes(x = x, y = density), 
            color = "red", size = 1, linetype = "dashed") + # 期待値による分布 
  geom_line(data = res_gaussian_df, mapping = aes(x = x, y = density, color = parameter), 
            alpha = 0.8, size = 1) + # サンプルによる分布
  scale_color_hue(labels = label_vec) + # 凡例テキスト:(数式表示用)
  guides(color = guide_legend(override.aes = list(alpha = 1))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁
  labs(
    title = "Gaussian Distribution", 
    subtitle = parse(text = paste0("list(E(mu)==", round(E_mu, 2), ", E(lambda)==", round(E_lambda, 3), ")")), 
    x = expression(x), y = "density"
  ) # ラベル
gaussian_graph

# グラフを並べて描画
gaussian_gamma_graph / gaussian_graph + 
  patchwork::plot_layout(guides = "collect")


