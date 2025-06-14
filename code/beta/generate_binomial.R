
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


