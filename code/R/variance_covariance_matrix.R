
# 分散共分散行列 -----------------------------------------------------------------


# 相関行列との関係 ----------------------------------------------------------------

### ・分散共分散行列の設定 -----

# 次元数を指定
D <- 3

# 分散共分散行列を指定
sigma_dd <- c(
  4, 1.8, -0.1, 
  1.8, 9, 2.4, 
  -0.1, 2.4, 1
) |> # 値を指定
  matrix(nrow = D, ncol = D, byrow = TRUE) # マトリクスに変換


### ・標準偏差と相関係数の計算 -----

# 次元を指定
i <- 1
j <- 2

# 分散を抽出
sigma2_i <- sigma_dd[i, i]
sigma2_j <- sigma_dd[j, j]
sigma2_i; sigma2_j

# 全ての分散を抽出
diag(sigma_dd)

# 共分散を抽出
sigma_ij <- sigma_dd[i, j]
sigma_ji <- sigma_dd[j, i]
sigma_ij; sigma_ji

# 標準偏差を計算
sigma_i <- sqrt(sigma2_i)
sigma_j <- sqrt(sigma2_j)
sigma_i; sigma_j

# 相関係数を計算
rho_ii <- sigma2_i / sigma_i / sigma_i
rho_ij <- sigma_ij / sigma_i / sigma_j
rho_ji <- sigma_ji / sigma_j / sigma_i
rho_jj <- sigma2_j / sigma_j / sigma_j
rho_ii; rho_ij; rho_ji; rho_jj


### ・分散共分散行列と相関行列の計算 -----

# 標準偏差が対角成分の行列を作成
s_dd <- sigma_dd |> 
  diag() |> # 対角成分(分散)を抽出
  (\(.){sqrt(.)})() |> # 標準偏差を計算
  diag() # 対角行列を作成
s_dd

# 標準偏差の逆数が対角成分の行列を作成
s_inv_dd <- sigma_dd |> 
  diag() |> # 対角成分(分散)を抽出
  (\(.){1 / sqrt(.)})() |> # 標準偏差の逆数を計算
  diag() # 対角行列を作成
s_inv_dd

# 標準偏差の逆数が対角成分の行列を計算
s_inv_dd <- solve(s_dd)
s_inv_dd

# 標準偏差が対角成分の行列を計算
s_dd <- solve(s_inv_dd)
s_dd

# 相関行列を計算
rho_dd <- s_inv_dd %*% sigma_dd %*% s_inv_dd
rho_dd

# 分散共分散行列を計算
sigma_dd <- s_dd %*% rho_dd %*% s_dd
sigma_dd


