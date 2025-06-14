
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


