# %% # 多次元スチューデントのt分布

# ライブラリを読み込み
import numpy as np
from scipy.stats import multivariate_t
from scipy.special import gamma, loggamma
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %% # 確率密度の計算

### パラメータの設定

# 次元数を指定
D = 3

# 自由度を指定
nu = 5

# 位置ベクトルを指定
mu_d = np.array([10.0, -6.0, 1.5])

# スケール行列を指定
sigma_dd = np.array(
    [[4.0, 1.8, -0.1], 
     [1.8, 9.0, 2.4], 
     [-0.1, 2.4, 1.0]]
)
print(np.linalg.det(sigma_dd)) # 確認

# 逆スケール行列を指定
lambda_dd = np.array(
    [[4.0, 1.8, -0.1], 
     [1.8, 9.0, 2.4], 
     [-0.1, 2.4, 1.0]]
)

# 逆スケール行列を計算
lambda_dd = np.linalg.inv(sigma_dd)

# 確率変数の値を指定
x_d = np.array([11.5, -5.0, 0.0])

# %%

### スケール行列を使用

# 定義式により確率密度を計算
C_St = gamma(0.5 * (nu + D)) / gamma(0.5 * nu)
C_St /= np.sqrt(np.pi * nu)**D * np.sqrt(np.linalg.det(sigma_dd))
x_tilde_d1 = (x_d - mu_d).reshape((D, 1))
tmp_term = x_tilde_d1.T.dot(np.linalg.inv(sigma_dd)).dot(x_tilde_d1).item()
dens = C_St / np.sqrt(1.0 + tmp_term / nu)**(nu + D)
print(dens)

# 対数をとった定義式により確率密度を計算
log_C_St = loggamma(0.5 * (nu + D)) - loggamma(0.5 * nu)
log_C_St -= D * 0.5 * np.log(np.pi * nu) + 0.5 * np.log(np.linalg.det(sigma_dd))
x_tilde_d1 = (x_d - mu_d).reshape((D, 1))
tmp_term = x_tilde_d1.T.dot(np.linalg.inv(sigma_dd)).dot(x_tilde_d1).item()
log_dens = log_C_St - (nu + D) * 0.5 * np.log(1.0 + tmp_term / nu)
dens = np.exp(log_dens)
print(dens, log_dens)

# 関数により確率密度を計算
dens = multivariate_t.pdf(x=x_d, loc=mu_d, shape=sigma_dd, df=nu)
print(dens)

# 対数をとった関数により確率密度を計算
log_dens = multivariate_t.logpdf(x=x_d, loc=mu_d, shape=sigma_dd, df=nu)
print(dens, log_dens)

# %%

### 逆スケール行列を使用

# 定義式により確率密度を計算
C_St = gamma(0.5 * (nu + D)) / gamma(0.5 * nu)
C_St *= np.sqrt(np.linalg.det(lambda_dd)) / np.sqrt(np.pi * nu)**D
x_tilde_d1 = (x_d - mu_d).reshape((D, 1))
tmp_term = x_tilde_d1.T.dot(lambda_dd).dot(x_tilde_d1).item()
dens = C_St / np.sqrt(1.0 + tmp_term / nu)**(nu + D)
print(dens)

# 対数をとった定義式により確率密度を計算
log_C_St = loggamma(0.5 * (nu + D)) - loggamma(0.5 * nu)
log_C_St += 0.5 * np.log(np.linalg.det(lambda_dd)) - D * 0.5 * np.log(np.pi * nu)
x_tilde_d1 = (x_d - mu_d).reshape((D, 1))
tmp_term = x_tilde_d1.T.dot(lambda_dd).dot(x_tilde_d1).item()
log_dens = log_C_St - (nu + D) * 0.5 * np.log(1.0 + tmp_term / nu)
dens = np.exp(log_dens)
print(dens, log_dens)

# 関数により確率密度を計算
dens = multivariate_t.pdf(x=x_d, loc=mu_d, shape=np.linalg.inv(lambda_dd), df=nu)
print(dens)

# 対数をとった関数により確率密度を計算
log_dens = multivariate_t.logpdf(x=x_d, loc=mu_d, shape=np.linalg.inv(lambda_dd), df=nu)
print(dens, log_dens)

# %%

### 標準化t分布による計算

# 固有値・固有ベクトルを計算
res_eigen = np.linalg.eig(sigma_dd)
lambda_d, u_dd = res_eigen[0], res_eigen[1].T
print(lambda_d, u_dd)

# 変数を変換
y_d = np.dot(u_dd, (x_d - mu_d).reshape((D, 1))).flatten() / np.sqrt(lambda_d)
print(y_d)

# Σを用いて標準化t分布により確率密度を計算
dens = multivariate_t.pdf(x=y_d, loc=np.zeros(D), shape=np.identity(D), df=nu)
dens /= np.sqrt(np.linalg.det(sigma_dd))
print(dens)

# Λを用いて標準化t分布により確率密度を計算
dens = np.sqrt(np.linalg.det(lambda_dd))
dens *= multivariate_t.pdf(x=y_d, loc=np.zeros(D), shape=np.identity(D), df=nu)
print(dens)


# %% # 統計量の計算

# 次元数を指定
D = 3

# 自由度を指定
nu = 5

# 位置ベクトルを指定
mu_d = np.array([10.0, -6.0, 1.5])

# スケール行列を指定
sigma_dd = np.array(
    [[4.0, 1.8, -0.1], 
     [1.8, 9.0, 2.4], 
     [-0.1, 2.4, 1.0]]
)

# 逆スケール行列を計算
lambda_dd = np.linalg.inv(sigma_dd)


# 期待値を計算:(ν > 1)
E_x_d = mu_d
print(E_x_d)

# Σを使って共分散を計算:(ν > 2)
cov_x_dd = nu / (nu - 2) * sigma_dd
print(cov_x_dd)

# Λを使って共分散を計算:(ν > 2)
cov_x_dd = nu / (nu - 2) * np.linalg.inv(lambda_dd)
print(cov_x_dd)

# 最頻値を計算
mode_x_d = mu_d
print(mode_x_d)


# %% # グラフの作成

### パラメータの設定

# 次元数を設定:(固定)
D = 2

# 自由度を指定
nu = 3

# 位置ベクトルを指定
mu_d = np.array([6.0, 10.0])

# スケール行列を指定
sigma_dd = np.array(
    [[1.0, 0.6], 
     [0.6, 4.0]]
)


# xの値を作成
x_1_vals = np.linspace(
    start=mu_d[0] - np.sqrt(sigma_dd[0, 0])*3.0, 
    stop=mu_d[0] + np.sqrt(sigma_dd[0, 0])*3.0, 
    num=101
)
x_2_vals = np.linspace(
    start=mu_d[1] - np.sqrt(sigma_dd[1, 1])*3.0, 
    stop=mu_d[1] + np.sqrt(sigma_dd[1, 1])*3.0, 
    num=101
)

# 作図用のxの点を作成
x_1_grid, x_2_grid = np.meshgrid(x_1_vals, x_2_vals)

# 計算用のxの点を作成
x_points = np.stack([x_1_grid.flatten(), x_2_grid.flatten()], axis=1)
x_dims = x_1_grid.shape
print(x_points)
print(x_dims)


# 多次元t分布の確率密度を計算
dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
print(dens)

# %%

### 作図

# パラメータラベル用の文字列を作成
param_text = '$\\nu=' + str(nu)
param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

# 2次元t分布を作図:(等高線図)
plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
#cnf = plt.contour(x_1_grid, x_2_grid, dens.reshape(x_dims)) # 等高線
cnf = plt.contourf(x_1_grid, x_2_grid, dens.reshape(x_dims)) # 塗りつぶし等高線
plt.xlabel('$x_1$') # x軸ラベル
plt.ylabel('$x_2$') # y軸ラベル
plt.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル
plt.title(param_text, loc='left') # タイトル
plt.colorbar(cnf, label='density') # カラーバー
plt.grid() # グリッド線
plt.show() # 描画

# 2次元t分布を作図:(ヒートマップ)
plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
pcl = plt.pcolor(x_1_grid, x_2_grid, dens.reshape(x_dims)) # ヒートマップ
plt.xlabel('$x_1$') # x軸ラベル
plt.ylabel('$x_2$') # y軸ラベル
plt.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル
plt.title(param_text, loc='left') # タイトル
plt.colorbar(pcl, label='density') # カラーバー
plt.grid() # グリッド線
plt.show() # 描画

# 2次元t分布を作図:曲面図
fig = plt.figure(figsize=(9, 9), facecolor='white') # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.plot_surface(x_1_grid, x_2_grid, dens.reshape(x_dims),
                cmap='jet', alpha=0.8) # 曲面図
ax.contour(x_1_grid, x_2_grid, dens.reshape(x_dims), 
           cmap='jet', offset=0.0) # 等高線図
ax.set_xlabel('$x_1$') # x軸ラベル
ax.set_ylabel('$x_2$') # y軸ラベル
ax.set_zlabel('density') # z軸ラベル
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル
ax.set_title(param_text, loc='left') # タイトル
plt.show() # 描画


# %% # 自由度の影響

### パラメータの設定

# 次元数を設定:(固定)
D = 2

# 自由度として利用する値を指定
nu_vals = np.arange(start=0.1, stop=15.1, step=0.1).round(decimals=1)

# フレーム数を設定
frame_num = len(nu_vals)
print(frame_num)

# 位置ベクトルを指定
mu_d = np.array([6.0, 10.0])

# スケール行列を指定
sigma_dd = np.array([[1.0, 0.6], [0.6, 4.0]])


# xの値を作成
x_1_vals = np.linspace(
    start=mu_d[0] - np.sqrt(sigma_dd[0, 0])*3.0, 
    stop=mu_d[0] + np.sqrt(sigma_dd[0, 0])*3.0, 
    num=101
)
x_2_vals = np.linspace(
    start=mu_d[1] - np.sqrt(sigma_dd[1, 1])*3.0, 
    stop=mu_d[1] + np.sqrt(sigma_dd[1, 1])*3.0, 
    num=101
)

# 作図用のxの点を作成
x_1_grid, x_2_grid = np.meshgrid(x_1_vals, x_2_vals)

# 計算用のxの点を作成
x_points = np.stack([x_1_grid.flatten(), x_2_grid.flatten()], axis=1)
x_shape = x_1_grid.shape


# z軸(確率密度)の最小値・最大値を設定
z_min = 0.0
z_max = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu_vals.mean()).max()
z_max = np.ceil(z_max * 10.0) / 10.0

# 等高線を引く値を設定
z_levels = np.linspace(start=z_min, stop=z_max, num=11)
print(z_levels)

# %%

### 等高線図のアニメーション

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# カラーバーを表示
tmp = plt.contourf(x_1_grid, x_2_grid, np.zeros(x_shape), 
                   vmin=z_min, vmax=z_max, levels=z_levels) # カラーバー用のダミー
fig.colorbar(tmp, label='density')

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の自由度を取得
    nu = nu_vals[i]
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(等高線図)
    #plt.contour(x_1_grid, x_2_grid, dens.reshape(x_dims), 
    #             vmin=z_min, vmax=z_max, levels=z_levels) # 等高線
    plt.contourf(x_1_grid, x_2_grid, dens.reshape(x_dims), 
                 vmin=z_min, vmax=z_max, levels=z_levels) # 塗りつぶし等高線
    plt.xlabel('$x_1$') # x軸ラベル
    plt.ylabel('$x_2$') # y軸ラベル
    plt.title(param_text, loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_nu_cnf.gif')

# %%

### ヒートマップのアニメーション

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# カラーバーを表示
tmp = plt.pcolor(x_1_grid, x_2_grid, np.zeros(x_shape), 
                 vmin=z_min, vmax=z_max) # カラーバー用のダミー
fig.colorbar(tmp, label='density')

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の自由度を取得
    nu = nu_vals[i]
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(ヒートマップ)
    plt.pcolor(x_1_grid, x_2_grid, dens.reshape(x_dims), 
               vmin=z_min, vmax=z_max) # 塗りつぶし等高線
    plt.xlabel('$x_1$') # x軸ラベル
    plt.ylabel('$x_2$') # y軸ラベル
    plt.title(param_text, loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_nu_pcl.gif')

# %%

### 曲面図のアニメーション

# 図を初期化
fig = plt.figure(figsize=(9, 9), facecolor='white') # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の自由度を取得
    nu = nu_vals[i]
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(曲面図)
    ax.plot_surface(x_1_grid, x_2_grid, dens.reshape(x_shape), 
                    cmap='jet', vmin=z_min, vmax=z_max, alpha=0.8) # 曲面図
    ax.contour(x_1_grid, x_2_grid, dens.reshape(x_shape), 
               cmap='jet', vmin=z_min, vmax=z_max, levels=z_levels, offset=0.0) # 等高線図
    ax.set_xlabel('$x_1$') # x軸ラベル
    ax.set_ylabel('$x_2$') # y軸ラベル
    ax.set_zlabel('density') # z軸ラベル
    ax.set_title(param_text, loc='left') # タイトル

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_nu_srf.gif')


# %% # 位置ベクトル(1軸)の影響

# 次元数を設定:(固定)
D = 2

# 自由度を指定
nu = 3

# x軸の位置パラメータとして利用する値を指定
mu_1_vals = np.linspace(start=-2.0, stop=2.0, num=101).round(decimals=2)

# フレーム数を設定
frame_num = len(mu_1_vals)
print(frame_num)

# y軸位置パラメータを指定
mu_2 = 10.0

# スケール行列を指定
sigma_dd = np.array([[1.0, 0.6], [0.6, 4.0]])


# xの値を作成
x_1_vals = np.linspace(
    start=mu_1_vals.min() - np.sqrt(sigma_dd[0, 0])*2.0, 
    stop=mu_1_vals.max() + np.sqrt(sigma_dd[0, 0])*2.0, 
    num=101
)
x_2_vals = np.linspace(
    start=mu_2 - np.sqrt(sigma_dd[1, 1])*3.0, 
    stop=mu_2 + np.sqrt(sigma_dd[1, 1])*3.0, 
    num=101
)

# 作図用のxの点を作成
x_1_grid, x_2_grid = np.meshgrid(x_1_vals, x_2_vals)

# 計算用のxの点を作成
x_points = np.stack([x_1_grid.flatten(), x_2_grid.flatten()], axis=1)
x_shape = x_1_grid.shape


# z軸(確率密度)の最小値・最大値を設定
z_min = 0.0
z_max = multivariate_t.pdf(x=x_points, loc=[mu_1_vals.mean(), mu_2], shape=sigma_dd, df=nu).max()
z_max = np.ceil(z_max * 10.0) / 10.0

# 等高線を引く値を設定
z_levels = np.linspace(start=z_min, stop=z_max, num=11)
print(z_levels)

# %%

### 等高線図のアニメーション

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# カラーバーを表示
tmp = plt.contourf(x_1_grid, x_2_grid, np.zeros(x_shape), 
                   vmin=z_min, vmax=z_max, levels=z_levels) # カラーバー用のダミー
fig.colorbar(tmp, label='density')

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の位置ベクトルを取得
    mu_1 = mu_1_vals[i]
    mu_d = np.array([mu_1, mu_2])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(等高線図)
    #plt.contour(x_1_grid, x_2_grid, dens.reshape(x_dims), 
    #             vmin=z_min, vmax=z_max, levels=z_levels) # 等高線
    plt.contourf(x_1_grid, x_2_grid, dens.reshape(x_dims), 
                 vmin=z_min, vmax=z_max, levels=z_levels) # 塗りつぶし等高線
    plt.xlabel('$x_1$') # x軸ラベル
    plt.ylabel('$x_2$') # y軸ラベル
    plt.title(param_text, loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_mu1_cnf.gif')

# %%

### ヒートマップのアニメーション

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# カラーバーを表示
tmp = plt.pcolor(x_1_grid, x_2_grid, np.zeros(x_shape), 
                 vmin=z_min, vmax=z_max) # カラーバー用のダミー
fig.colorbar(tmp, label='density')

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の位置ベクトルを取得
    mu_1 = mu_1_vals[i]
    mu_d = np.array([mu_1, mu_2])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(ヒートマップ)
    plt.pcolor(x_1_grid, x_2_grid, dens.reshape(x_dims), 
               vmin=z_min, vmax=z_max) # 塗りつぶし等高線
    plt.xlabel('$x_1$') # x軸ラベル
    plt.ylabel('$x_2$') # y軸ラベル
    plt.title(param_text, loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_mu1_pcl.gif')

# %%

### 曲面図のアニメーション

# 図を初期化
fig = plt.figure(figsize=(9, 9), facecolor='white') # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のパラメータを取得
    mu_1 = mu_1_vals[i]
    mu_d = np.array([mu_1, mu_2])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(曲面図)
    ax.plot_surface(x_1_grid, x_2_grid, dens.reshape(x_dims), 
                    cmap='jet', vmin=z_min, vmax=z_max, alpha=0.8) # 曲面図
    ax.contour(x_1_grid, x_2_grid, dens.reshape(x_dims), 
               cmap='jet', vmin=z_min, vmax=z_max, levels=z_levels, offset=0.0) # 等高線図
    ax.set_xlabel('$x_1$') # x軸ラベル
    ax.set_ylabel('$x_2$') # y軸ラベル
    ax.set_zlabel('density') # z軸ラベル
    ax.set_title(param_text, loc='left') # タイトル
    ax.set_zlim(zmin=z_min, zmax=z_max) # z軸の表示範囲

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_mu1_srf.gif')


# %% # 位置ベクトル(2軸)の影響

# 次元数を設定:(固定)
D = 2

# 自由度を指定
nu = 3

# x軸位置パラメータを指定
mu_1 = 6.0

# y軸の位置パラメータとして利用する値を指定
mu_2_vals = np.linspace(start=-2.0, stop=2.0, num=101).round(decimals=2)

# フレーム数を設定
frame_num = len(mu_2_vals)
print(frame_num)

# スケール行列を指定
sigma_dd = np.array([[1.0, 0.6], [0.6, 4.0]])


# xの値を作成
x_1_vals = np.linspace(
    start=mu_1 - np.sqrt(sigma_dd[0, 0])*3.0, 
    stop=mu_1 + np.sqrt(sigma_dd[0, 0])*3.0, 
    num=101
)
x_2_vals = np.linspace(
    start=mu_2_vals.min() - np.sqrt(sigma_dd[1, 1])*2.0, 
    stop=mu_2_vals.max() + np.sqrt(sigma_dd[1, 1])*2.0, 
    num=101
)

# 作図用のxの点を作成
x_1_grid, x_2_grid = np.meshgrid(x_1_vals, x_2_vals)

# 計算用のxの点を作成
x_points = np.stack([x_1_grid.flatten(), x_2_grid.flatten()], axis=1)
x_shape = x_1_grid.shape


# z軸(確率密度)の最小値・最大値を設定
z_min = 0.0
z_max = multivariate_t.pdf(x=x_points, loc=[mu_1, mu_2_vals.mean()], shape=sigma_dd, df=nu).max()
z_max = np.ceil(z_max * 10.0) / 10.0

# 等高線を引く値を設定
z_levels = np.linspace(start=z_min, stop=z_max, num=11)
print(z_levels)

# %%

### 等高線図のアニメーション

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# カラーバーを表示
tmp = plt.contourf(x_1_grid, x_2_grid, np.zeros(x_shape), 
                   vmin=z_min, vmax=z_max, levels=z_levels) # カラーバー用のダミー
fig.colorbar(tmp, label='density')

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の位置ベクトルを取得
    mu_2 = mu_2_vals[i]
    mu_d = np.array([mu_1, mu_2])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(等高線図)
    #plt.contour(x_1_grid, x_2_grid, dens.reshape(x_dims), 
    #             vmin=z_min, vmax=z_max, levels=z_levels) # 等高線
    plt.contourf(x_1_grid, x_2_grid, dens.reshape(x_dims), 
                 vmin=z_min, vmax=z_max, levels=z_levels) # 塗りつぶし等高線
    plt.xlabel('$x_1$') # x軸ラベル
    plt.ylabel('$x_2$') # y軸ラベル
    plt.title(param_text, loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_mu2_cnf.gif')

# %%

### ヒートマップのアニメーション

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# カラーバーを表示
tmp = plt.pcolor(x_1_grid, x_2_grid, np.zeros(x_shape), 
                 vmin=z_min, vmax=z_max) # カラーバー用のダミー
fig.colorbar(tmp, label='density')

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の位置ベクトルを取得
    mu_2 = mu_2_vals[i]
    mu_d = np.array([mu_1, mu_2])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(ヒートマップ)
    plt.pcolor(x_1_grid, x_2_grid, dens.reshape(x_dims), 
               vmin=z_min, vmax=z_max) # 塗りつぶし等高線
    plt.xlabel('$x_1$') # x軸ラベル
    plt.ylabel('$x_2$') # y軸ラベル
    plt.title(param_text, loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_mu2_pcl.gif')

# %%

### 曲面図のアニメーション

# 図を初期化
fig = plt.figure(figsize=(9, 9), facecolor='white') # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のパラメータを取得
    mu_2 = mu_2_vals[i]
    mu_d = np.array([mu_1, mu_2])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(曲面図)
    ax.plot_surface(x_1_grid, x_2_grid, dens.reshape(x_dims), 
                    cmap='jet', vmin=z_min, vmax=z_max, alpha=0.8) # 曲面図
    ax.contour(x_1_grid, x_2_grid, dens.reshape(x_dims), 
               cmap='jet', vmin=z_min, vmax=z_max, levels=z_levels, offset=0.0) # 等高線図
    ax.set_xlabel('$x_1$') # x軸ラベル
    ax.set_ylabel('$x_2$') # y軸ラベル
    ax.set_zlabel('density') # z軸ラベル
    ax.set_title(param_text, loc='left') # タイトル
    ax.set_zlim(zmin=z_min, zmax=z_max) # z軸の表示範囲

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_mu2_srf.gif')


# %% # スケール行列(1,1成分)の影響

# 次元数を設定:(固定)
D = 2

# 自由度を指定
nu = 3

# 位置ベクトルを指定
mu_1 = np.array([6.0, 10.0])

# x軸のスケールパラメータとして利用する値を指定
sigma_11_vals = np.arange(start=0.5, stop=6.0, step=0.1).round(decimals=1)

# フレーム数を設定
frame_num = len(sigma_11_vals)
print(frame_num)

# y軸のスケールパラメータを指定
sigma_22 = 4.0

# x・y軸のスケールパラメータを指定
sigma_12 = 0.6


# xの値を作成
x_1_vals = np.linspace(
    start=mu_d[0] - np.sqrt(sigma_11_vals.max())*2.0, 
    stop=mu_d[0] + np.sqrt(sigma_11_vals.max())*2.0, 
    num=101
)
x_2_vals = np.linspace(
    start=mu_d[1] - np.sqrt(sigma_22)*3.0, 
    stop=mu_d[1] + np.sqrt(sigma_22)*3.0, 
    num=101
)

# 作図用のxの点を作成
x_1_grid, x_2_grid = np.meshgrid(x_1_vals, x_2_vals)

# 計算用のxの点を作成
x_points = np.stack([x_1_grid.flatten(), x_2_grid.flatten()], axis=1)
x_shape = x_1_grid.shape


# z軸(確率密度)の最小値・最大値を設定
z_min = 0.0
z_max = multivariate_t.pdf(x=x_points, loc=mu_d, shape=np.array([[sigma_11_vals.min(), sigma_12], [sigma_12, sigma_22]]), df=nu).max()
z_max = np.ceil(z_max * 10.0) / 10.0

# 等高線を引く値を設定
z_levels = np.linspace(start=z_min, stop=z_max, num=11)
print(z_levels)

# %%

### 等高線図のアニメーション

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# カラーバーを表示
tmp = plt.contourf(x_1_grid, x_2_grid, np.zeros(x_shape), 
                   vmin=z_min, vmax=z_max, levels=z_levels) # カラーバー用のダミー
fig.colorbar(tmp, label='density')

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のスケール行列を取得
    sigma_11 = sigma_11_vals[i]
    sigma_dd = np.array([[sigma_11, sigma_12], [sigma_12, sigma_22]])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(等高線図)
    #plt.contour(x_1_grid, x_2_grid, dens.reshape(x_dims), 
    #             vmin=z_min, vmax=z_max, levels=z_levels) # 等高線
    plt.contourf(x_1_grid, x_2_grid, dens.reshape(x_dims), 
                 vmin=z_min, vmax=z_max, levels=z_levels) # 塗りつぶし等高線
    plt.xlabel('$x_1$') # x軸ラベル
    plt.ylabel('$x_2$') # y軸ラベル
    plt.title(param_text, loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_sigma11_cnf.gif')

# %%

### ヒートマップのアニメーション

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# カラーバーを表示
tmp = plt.pcolor(x_1_grid, x_2_grid, np.zeros(x_shape), 
                 vmin=z_min, vmax=z_max) # カラーバー用のダミー
fig.colorbar(tmp, label='density')

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のスケール行列を取得
    sigma_11 = sigma_11_vals[i]
    sigma_dd = np.array([[sigma_11, sigma_12], [sigma_12, sigma_22]])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(ヒートマップ)
    plt.pcolor(x_1_grid, x_2_grid, dens.reshape(x_dims), 
               vmin=z_min, vmax=z_max) # 塗りつぶし等高線
    plt.xlabel('$x_1$') # x軸ラベル
    plt.ylabel('$x_2$') # y軸ラベル
    plt.title(param_text, loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_sigma11_pcl.gif')

# %%

### 曲面図のアニメーション

# 図を初期化
fig = plt.figure(figsize=(9, 9), facecolor='white') # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のスケール行列を取得
    sigma_11 = sigma_11_vals[i]
    sigma_dd = np.array([[sigma_11, sigma_12], [sigma_12, sigma_22]])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(曲面図)
    ax.plot_surface(x_1_grid, x_2_grid, dens.reshape(x_dims), 
                    cmap='jet', vmin=z_min, vmax=z_max, alpha=0.8) # 曲面図
    ax.contour(x_1_grid, x_2_grid, dens.reshape(x_dims), 
               cmap='jet', vmin=z_min, vmax=z_max, levels=z_levels, offset=0.0) # 等高線図
    ax.set_xlabel('$x_1$') # x軸ラベル
    ax.set_ylabel('$x_2$') # y軸ラベル
    ax.set_zlabel('density') # z軸ラベル
    ax.set_title(param_text, loc='left') # タイトル
    ax.set_zlim(zmin=z_min, zmax=z_max) # z軸の表示範囲

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_sigma11_srf.gif')


# %% # スケール行列(2,2成分)の影響

# 次元数を設定:(固定)
D = 2

# 自由度を指定
nu = 3

# 位置ベクトルを指定
mu_1 = np.array([6.0, 10.0])

# x軸のスケールパラメータを指定
sigma_11 = 1.0

# y軸のスケールパラメータとして利用する値を指定
sigma_22_vals = np.arange(start=0.5, stop=6.0, step=0.1).round(decimals=1)

# フレーム数を設定
frame_num = len(sigma_22_vals)
print(frame_num)

# x・y軸のスケールパラメータを指定
sigma_12 = 0.6


# xの値を作成
x_1_vals = np.linspace(
    start=mu_d[0] - np.sqrt(sigma_11)*3.0, 
    stop=mu_d[0] + np.sqrt(sigma_11)*3.0, 
    num=101
)
x_2_vals = np.linspace(
    start=mu_d[1] - np.sqrt(sigma_22_vals.max())*2.0, 
    stop=mu_d[1] + np.sqrt(sigma_22_vals.max())*2.0, 
    num=101
)

# 作図用のxの点を作成
x_1_grid, x_2_grid = np.meshgrid(x_1_vals, x_2_vals)

# 計算用のxの点を作成
x_points = np.stack([x_1_grid.flatten(), x_2_grid.flatten()], axis=1)
x_shape = x_1_grid.shape


# z軸(確率密度)の最小値・最大値を設定
z_min = 0.0
z_max = multivariate_t.pdf(x=x_points, loc=mu_d, shape=np.array([[sigma_11, sigma_12], [sigma_12, sigma_22_vals.min()]]), df=nu).max()
z_max = np.ceil(z_max * 10.0) / 10.0

# 等高線を引く値を設定
z_levels = np.linspace(start=z_min, stop=z_max, num=11)
print(z_levels)

# %%

### 等高線図のアニメーション

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# カラーバーを表示
tmp = plt.contourf(x_1_grid, x_2_grid, np.zeros(x_shape), 
                   vmin=z_min, vmax=z_max, levels=z_levels) # カラーバー用のダミー
fig.colorbar(tmp, label='density')

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のスケール行列を取得
    sigma_22 = sigma_22_vals[i]
    sigma_dd = np.array([[sigma_11, sigma_12], [sigma_12, sigma_22]])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(等高線図)
    #plt.contour(x_1_grid, x_2_grid, dens.reshape(x_dims), 
    #             vmin=z_min, vmax=z_max, levels=z_levels) # 等高線
    plt.contourf(x_1_grid, x_2_grid, dens.reshape(x_dims), 
                 vmin=z_min, vmax=z_max, levels=z_levels) # 塗りつぶし等高線
    plt.xlabel('$x_1$') # x軸ラベル
    plt.ylabel('$x_2$') # y軸ラベル
    plt.title(param_text, loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_sigma22_cnf.gif')

# %%

### ヒートマップのアニメーション

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# カラーバーを表示
tmp = plt.pcolor(x_1_grid, x_2_grid, np.zeros(x_shape), 
                 vmin=z_min, vmax=z_max) # カラーバー用のダミー
fig.colorbar(tmp, label='density')

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のスケール行列を取得
    sigma_22 = sigma_22_vals[i]
    sigma_dd = np.array([[sigma_11, sigma_12], [sigma_12, sigma_22]])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(ヒートマップ)
    plt.pcolor(x_1_grid, x_2_grid, dens.reshape(x_dims), 
               vmin=z_min, vmax=z_max) # 塗りつぶし等高線
    plt.xlabel('$x_1$') # x軸ラベル
    plt.ylabel('$x_2$') # y軸ラベル
    plt.title(param_text, loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_sigma22_pcl.gif')

# %%

### 曲面図のアニメーション

# 図を初期化
fig = plt.figure(figsize=(9, 9), facecolor='white') # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のスケール行列を取得
    sigma_22 = sigma_22_vals[i]
    sigma_dd = np.array([[sigma_11, sigma_12], [sigma_12, sigma_22]])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(曲面図)
    ax.plot_surface(x_1_grid, x_2_grid, dens.reshape(x_dims), 
                    cmap='jet', vmin=z_min, vmax=z_max, alpha=0.8) # 曲面図
    ax.contour(x_1_grid, x_2_grid, dens.reshape(x_dims), 
               cmap='jet', vmin=z_min, vmax=z_max, levels=z_levels, offset=0.0) # 等高線図
    ax.set_xlabel('$x_1$') # x軸ラベル
    ax.set_ylabel('$x_2$') # y軸ラベル
    ax.set_zlabel('density') # z軸ラベル
    ax.set_title(param_text, loc='left') # タイトル
    ax.set_zlim(zmin=z_min, zmax=z_max) # z軸の表示範囲

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_sigma22_srf.gif')


# %% # スケール行列(1,2成分)の影響

# 次元数を設定:(固定)
D = 2

# 自由度を指定
nu = 3

# 位置ベクトルを指定
mu_1 = np.array([6.0, 10.0])

# x軸・y軸のスケールパラメータを指定
sigma_11 = 4.0
sigma_22 = 10.0

# x・y軸のスケールパラメータとして利用する値を指定
sigma_12_vals = np.linspace(start=-5.0, stop=5.0, num=101).round(decimals=1)

# フレーム数を設定
frame_num = len(sigma_12_vals)
print(frame_num)


# xの値を作成
x_1_vals = np.linspace(
    start=mu_d[0] - np.sqrt(sigma_11)*3.0, 
    stop=mu_d[0] + np.sqrt(sigma_11)*3.0, 
    num=101
)
x_2_vals = np.linspace(
    start=mu_d[1] - np.sqrt(sigma_22)*3.0, 
    stop=mu_d[1] + np.sqrt(sigma_22)*3.0, 
    num=101
)

# 作図用のxの点を作成
x_1_grid, x_2_grid = np.meshgrid(x_1_vals, x_2_vals)

# 計算用のxの点を作成
x_points = np.stack([x_1_grid.flatten(), x_2_grid.flatten()], axis=1)
x_shape = x_1_grid.shape


# z軸(確率密度)の最小値・最大値を設定
z_min = 0.0
sigma_12_max = np.max([np.abs(sigma_12_vals.min()), np.abs(sigma_12_vals.max())])
z_max = multivariate_t.pdf(x=x_points, loc=mu_d, shape=np.array([[sigma_11, sigma_12_max], [sigma_12_max, sigma_22]]), df=nu).max()
z_max = np.ceil(z_max * 10.0) / 10.0

# 等高線を引く値を設定
z_levels = np.linspace(start=z_min, stop=z_max, num=11)
print(z_levels)

# %%

### 等高線図のアニメーション

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# カラーバーを表示
tmp = plt.contourf(x_1_grid, x_2_grid, np.zeros(x_shape), 
                   vmin=z_min, vmax=z_max, levels=z_levels) # カラーバー用のダミー
fig.colorbar(tmp, label='density')

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のスケール行列を取得
    sigma_12 = sigma_12_vals[i]
    sigma_dd = np.array([[sigma_11, sigma_12], [sigma_12, sigma_22]])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(等高線図)
    #plt.contour(x_1_grid, x_2_grid, dens.reshape(x_dims), 
    #             vmin=z_min, vmax=z_max, levels=z_levels) # 等高線
    plt.contourf(x_1_grid, x_2_grid, dens.reshape(x_dims), 
                 vmin=z_min, vmax=z_max, levels=z_levels) # 塗りつぶし等高線
    plt.xlabel('$x_1$') # x軸ラベル
    plt.ylabel('$x_2$') # y軸ラベル
    plt.title(param_text, loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_sigma12_cnf.gif')

# %%

### ヒートマップのアニメーション

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# カラーバーを表示
tmp = plt.pcolor(x_1_grid, x_2_grid, np.zeros(x_shape), 
                 vmin=z_min, vmax=z_max) # カラーバー用のダミー
fig.colorbar(tmp, label='density')

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のスケール行列を取得
    sigma_12 = sigma_12_vals[i]
    sigma_dd = np.array([[sigma_11, sigma_12], [sigma_12, sigma_22]])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(ヒートマップ)
    plt.pcolor(x_1_grid, x_2_grid, dens.reshape(x_dims), 
               vmin=z_min, vmax=z_max) # 塗りつぶし等高線
    plt.xlabel('$x_1$') # x軸ラベル
    plt.ylabel('$x_2$') # y軸ラベル
    plt.title(param_text, loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_sigma12_pcl.gif')

# %%

### 曲面図のアニメーション

# 図を初期化
fig = plt.figure(figsize=(9, 9), facecolor='white') # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle("Bivariate Student's t-Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のスケール行列を取得
    sigma_12 = sigma_12_vals[i]
    sigma_dd = np.array([[sigma_11, sigma_12], [sigma_12, sigma_22]])
    
    # 多次元t分布の確率密度を計算
    dens = multivariate_t.pdf(x=x_points, loc=mu_d, shape=sigma_dd, df=nu)
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\nu=' + str(nu)
    param_text += ', \mu=(' + ', '.join([str(mu) for mu in mu_d]) + ')'
    param_text += ', \Sigma=' + str([list(sigma_d) for sigma_d in sigma_dd]) + '$'

    # 2次元t分布を作図:(曲面図)
    ax.plot_surface(x_1_grid, x_2_grid, dens.reshape(x_dims), 
                    cmap='jet', vmin=z_min, vmax=z_max, alpha=0.8) # 曲面図
    ax.contour(x_1_grid, x_2_grid, dens.reshape(x_dims), 
               cmap='jet', vmin=z_min, vmax=z_max, levels=z_levels, offset=0.0) # 等高線図
    ax.set_xlabel('$x_1$') # x軸ラベル
    ax.set_ylabel('$x_2$') # y軸ラベル
    ax.set_zlabel('density') # z軸ラベル
    ax.set_title(param_text, loc='left') # タイトル
    ax.set_zlim(zmin=z_min, zmax=z_max) # z軸の表示範囲

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('../../figure/Python/Bivariate_t_dens_sigma12_srf.gif')


# %%


