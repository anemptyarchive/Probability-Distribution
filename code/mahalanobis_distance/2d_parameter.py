
# 2次元マハラノビス距離の作図

# %%

# ライブラリを読込
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# %%

### 平均パラメータの影響 ---------------------------------------------------------

## パラメータの設定

# フレーム数を指定
frame_num = 100

# 平均ベクトルを指定
mu0_vals = np.linspace(start=-5.0, stop=5.0, num=frame_num)
mu1_vals = np.linspace(start=0.0, stop=10.0, num=frame_num)

# 分散共分散行列を指定
sigma_dd = np.diag(np.ones(2))

# 精度行列を計算
inv_sigma_dd = np.linalg.inv(sigma_dd)

# %%

## 座標の作成

# 各軸の範囲の調整値を指定
sgm_num = 3.0

# 各軸の範囲を設定
x0_min = mu0_vals.min() - sgm_num * np.sqrt(sigma_dd[0, 0])
x0_max = mu0_vals.max() + sgm_num * np.sqrt(sigma_dd[0, 0])
x1_min = mu1_vals.min() - sgm_num * np.sqrt(sigma_dd[1, 1])
x1_max = mu1_vals.max() + sgm_num * np.sqrt(sigma_dd[1, 1])
print(x0_min.round(2), x0_max.round(2))
print(x1_min.round(2), x1_max.round(2))

# 各軸の値を作成
x0 = np.linspace(start=x0_min, stop=x0_max, num=50) # 0軸方向の点の数を指定
x1 = np.linspace(start=x1_min, stop=x1_max, num=50) # 1軸方向の点の数を指定

# 格子点を作成
X0, X1 = np.meshgrid(x0, x1)

# 格子点の形状を設定
grid_shape = X0.shape
grid_size  = X0.size
print(grid_size)

# 座標を作成
X = np.stack([X0.flatten(), X1.flatten()], axis=1)

# %%

## 距離の計算

# フレームごとに処理
trace_mu_lt   = []
trace_dist_lt = []
for frame_i in range(frame_num):

    # 平均ベクトルを作成
    mu_d = np.array(
        [mu0_vals[frame_i], mu1_vals[frame_i]]
    )

    # マハラノビス距離を計算
    dist_vec = np.array(
        [np.sqrt((x_d - mu_d).T @ inv_sigma_dd @ (x_d - mu_d)) for x_d in X]
    )
    
    # 結果を格納
    trace_mu_lt.append(mu_d.copy())
    trace_dist_lt.append(dist_vec)
    
    # 途中経過を表示
    print(f'frame: {frame_i+1} / {frame_num}')

# %%

## アニメーションの作成

# グラデーションの範囲を設定
dist_min = 0.0
dist_max = np.ceil(max([arr.max() for arr in trace_dist_lt]))

# 等高線の位置を指定
dist_levels = np.linspace(start=dist_min, stop=dist_max, num=11) # 線の数を指定
print(dist_levels)

# %% 

# ラベル用の文字列を作成
def_label = '$\\Delta = \\sqrt{(x - \\mu)^{T} \\Sigma^{-1} (x - \\mu)}$'

# グラフオブジェクトを初期化
fig, ax = plt.subplots(nrows=1, ncols=1, constrained_layout=True, 
                       figsize=(8, 8), dpi=100, facecolor='white')
fig.suptitle("Mahalanobis' Distance", fontsize=20)
cs = ax.contour(X0, X1, np.linspace(dist_min, dist_max, num=grid_size).reshape(grid_shape), 
                cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # カラーバー表示用のダミー
fig.colorbar(cs, ax=ax, shrink=0.8, label=def_label)

# 作図処理を定義
def update(frame_i):
    
    # 前フレームのグラフを初期化
    ax.cla()

    # 平均ベクトルを取得
    mu_d = trace_mu_lt[frame_i]

    # ラベル用の文字列を作成
    mu_str    = '(' + ', '.join(str(val.round(2)) for val in mu_d) + ')'
    sigma_str = '(' + ', '.join('(' + ', '.join(str(val.round(2)) for val in vec) + ')' for vec in sigma_dd) + ')'
    param_label  = '$D = 2$\n'
    param_label += '$\\mu = ' + mu_str + '$\n'
    param_label += '$\\Sigma = ' + sigma_str + '$'
    
    # 2Dマハラノビス距離を作図
    ax.scatter(*mu_d, 
               color='red', s=100, marker='x', label='$\\mu$') # 中心
    ax.plot([mu_d[0], x0_min], [mu_d[1], mu_d[1]], 
            color='black', linestyle='dotted') # 中心の0軸座標
    ax.plot([mu_d[0], mu_d[0]], [mu_d[1], x1_min], 
            color='black', linestyle='dotted') # 中心の1軸座標
    ax.contour(X0, X1, trace_dist_lt[frame_i].reshape(grid_shape), 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # マハラノビス距離
    ax.set_xlim(xmin=x0_min, xmax=x0_max)
    ax.set_ylim(ymin=x1_min, ymax=x1_max)
    ax.set_xlabel('$x_0$')
    ax.set_ylabel('$x_1$')
    ax.set_title(param_label, loc='left')
    ax.grid()
    ax.legend()
    ax.set_aspect('equal', adjustable='box')

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/2d_mu.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%

### 分散パラメータの影響 ---------------------------------------------------------

## パラメータの設定

# フレーム数を指定
frame_num = 100

# 平均ベクトルを指定
mu_d = np.zeros(2)

# 分散共分散行列を指定
sigma_dd     = np.diag(np.ones(2))
sigma00_vals = np.linspace(start=0.1, stop=9.0, num=frame_num)
sigma11_vals = np.linspace(start=0.1, stop=4.0, num=frame_num)

# %%

## 座標の作成

# 各軸の範囲の調整値を指定
sgm_num = 1.0

# 各軸の範囲を設定
x0_min = mu_d[0] - sgm_num * np.sqrt(sigma00_vals.max())
x0_max = mu_d[0] + sgm_num * np.sqrt(sigma00_vals.max())
x1_min = mu_d[1] - sgm_num * np.sqrt(sigma11_vals.max())
x1_max = mu_d[1] + sgm_num * np.sqrt(sigma11_vals.max())
print(x0_min.round(2), x0_max.round(2))
print(x1_min.round(2), x1_max.round(2))

# 各軸の値を作成
x0 = np.linspace(start=x0_min, stop=x0_max, num=50) # 0軸方向の点の数を指定
x1 = np.linspace(start=x1_min, stop=x1_max, num=50) # 1軸方向の点の数を指定

# 格子点を作成
X0, X1 = np.meshgrid(x0, x1)

# 格子点の形状を設定
grid_shape = X0.shape
grid_size  = X0.size
print(grid_size)

# 座標を作成
X = np.stack([X0.flatten(), X1.flatten()], axis=1)

# %%

## 距離の計算

# フレームごとに処理
trace_sigma_lt = []
trace_dist_lt  = []
for frame_i in range(frame_num):

    # 分散共分散行列を作成
    sigma_dd[0, 0] = sigma00_vals[frame_i]
    sigma_dd[1, 1] = sigma11_vals[frame_i]
    
    # 精度行列を計算
    inv_sigma_dd = np.linalg.inv(sigma_dd)

    # マハラノビス距離を計算
    dist_vec = np.array(
        [np.sqrt((x_d - mu_d).T @ inv_sigma_dd @ (x_d - mu_d)) for x_d in X]
    )
    
    # 結果を格納
    trace_sigma_lt.append(sigma_dd.copy())
    trace_dist_lt.append(dist_vec)
    
    # 途中経過を表示
    print(f'frame: {frame_i+1} / {frame_num}')

# %%

## アニメーションの作成

# グラデーションの範囲を設定
dist_min = 0.0
dist_max = np.ceil(max([arr.max() for arr in trace_dist_lt]))

# 基準値を指定
dist_val = 1.0

# 等高線の位置を設定
dist_levels = np.array([dist_min, dist_val, dist_max])
print(dist_levels)

# %%

# ユークリッド距離(円周の座標)を計算
t = np.linspace(start=0.0, stop=2.0*np.pi, num=61) # ラジアン
euclid_x0 = mu_d[0] + dist_val * np.cos(t)
euclid_x1 = mu_d[1] + dist_val * np.sin(t)

# ラベル用の文字列を作成
def_label = '$\\Delta = \\sqrt{(x - \\mu)^{T} \\Sigma^{-1} (x - \\mu)}$'

# グラフオブジェクトを初期化
fig, ax = plt.subplots(nrows=1, ncols=1, constrained_layout=True, 
                       figsize=(8, 8), dpi=100, facecolor='white')
fig.suptitle("Mahalanobis' Distance", fontsize=20)
cs = ax.contour(X0, X1, np.linspace(dist_min, dist_max, num=grid_size).reshape(grid_shape), 
                cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # カラーバー表示用のダミー
fig.colorbar(cs, ax=ax, shrink=0.8, label=def_label)

# 作図処理を定義
def update(frame_i):
    
    # 前フレームのグラフを初期化
    ax.cla()

    # 分散共分散行列を取得
    sigma_dd = trace_sigma_lt[frame_i]

    # 標準偏差を抽出
    sigma_d = np.sqrt(np.diag(sigma_dd))
    
    # ラベル用の文字列を作成
    mu_str    = '(' + ', '.join(str(val.round(2)) for val in mu_d) + ')'
    sigma_str = '(' + ', '.join('(' + ', '.join(str(val.round(2)) for val in vec) + ')' for vec in sigma_dd) + ')'
    param_label  = '$D = 2$\n'
    param_label += '$\\mu = ' + mu_str + '$\n'
    param_label += '$\\Sigma = ' + sigma_str + '$'
    
    # 2Dマハラノビス距離を作図
    ax.plot([mu_d[0], mu_d[0]+sigma_d[0]], [mu_d[1], mu_d[1]], 
            color='C1', label=f'$\\sigma_0 = {np.sqrt(sigma_dd[0, 0]):.2f}$') # 0軸方向の標準偏差1の線分
    ax.plot([mu_d[0], mu_d[0]], [mu_d[1], mu_d[1]+sigma_d[1]], 
            color='C2', label=f'$\\sigma_1 = {np.sqrt(sigma_dd[1, 1]):.2f}$') # 1軸方向の標準偏差1の線分
    ax.plot(euclid_x0, euclid_x1, 
            color='C0') # ユークリッド距離
    ax.contour(X0, X1, trace_dist_lt[frame_i].reshape(grid_shape), 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # マハラノビス距離
    ax.set_xlim(xmin=x0_min, xmax=x0_max)
    ax.set_ylim(ymin=x1_min, ymax=x1_max)
    ax.set_xlabel('$x_0$')
    ax.set_ylabel('$x_1$')
    ax.set_title(param_label, loc='left')
    ax.grid()
    ax.legend(loc='upper left')
    ax.set_aspect('equal', adjustable='box')

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/2d_sigma2.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%

### 共分散パラメータの影響 -------------------------------------------------------

## パラメータの設定

# フレーム数を指定
frame_num = 100

# 平均ベクトルを指定
mu_d = np.zeros(2)

# 分散共分散行列を指定
sigma_dd     = np.diag(np.ones(2)) * 4.0
sigma01_vals = np.linspace(start=-3.0, stop=3.0, num=frame_num)

# %%

## 座標の作成

# 各軸の範囲の調整値を指定
sgm_num = 1.0

# 各軸の範囲を設定
x0_min = mu_d[0] - sgm_num * np.sqrt(sigma_dd[0, 0])
x0_max = mu_d[0] + sgm_num * np.sqrt(sigma_dd[0, 0])
x1_min = mu_d[1] - sgm_num * np.sqrt(sigma_dd[1, 1])
x1_max = mu_d[1] + sgm_num * np.sqrt(sigma_dd[1, 1])
print(x0_min.round(2), x0_max.round(2))
print(x1_min.round(2), x1_max.round(2))

# 各軸の値を作成
x0 = np.linspace(start=x0_min, stop=x0_max, num=50) # 0軸方向の点の数を指定
x1 = np.linspace(start=x1_min, stop=x1_max, num=50) # 1軸方向の点の数を指定

# 格子点を作成
X0, X1 = np.meshgrid(x0, x1)

# 格子点の形状を設定
grid_shape = X0.shape
grid_size  = X0.size
print(grid_size)

# 座標を作成
X = np.stack([X0.flatten(), X1.flatten()], axis=1)

# %%

## 距離の計算

# フレームごとに処理
trace_sigma_lt = []
trace_dist_lt  = []
for frame_i in range(frame_num):

    # 分散共分散行列を作成
    sigma_dd[0, 1] = sigma01_vals[frame_i]
    sigma_dd[1, 0] = sigma01_vals[frame_i]
    
    # 精度行列を計算
    inv_sigma_dd = np.linalg.inv(sigma_dd)

    # マハラノビス距離を計算
    dist_vec = np.array(
        [np.sqrt((x_d - mu_d).T @ inv_sigma_dd @ (x_d - mu_d)) for x_d in X]
    )
    
    # 結果を格納
    trace_sigma_lt.append(sigma_dd.copy())
    trace_dist_lt.append(dist_vec)
    
    # 途中経過を表示
    print(f'frame: {frame_i+1} / {frame_num}')

# %%

## アニメーションの作成

# グラデーションの範囲を設定
dist_min = 0.0
dist_max = np.ceil(max([arr.max() for arr in trace_dist_lt]))

# 基準値を指定
dist_val = 1.0

# 等高線の位置を設定
dist_levels = np.array([dist_min, dist_val, dist_max])
print(dist_levels)

# %%

# ユークリッド距離(円周の座標)を計算
t = np.linspace(start=0.0, stop=2.0*np.pi, num=61) # ラジアン
euclid_x0 = mu_d[0] + dist_val * np.cos(t)
euclid_x1 = mu_d[1] + dist_val * np.sin(t)

# ラベル用の文字列を作成
def_label = '$\\Delta = \\sqrt{(x - \\mu)^{T} \\Sigma^{-1} (x - \\mu)}$'

# グラフオブジェクトを初期化
fig, ax = plt.subplots(nrows=1, ncols=1, constrained_layout=True, 
                       figsize=(8, 8), dpi=100, facecolor='white')
fig.suptitle("Mahalanobis' Distance", fontsize=20)
cs = ax.contour(X0, X1, np.linspace(dist_min, dist_max, num=grid_size).reshape(grid_shape), 
                cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # カラーバー表示用のダミー
fig.colorbar(cs, ax=ax, shrink=0.8, label=def_label)

# 作図処理を定義
def update(frame_i):
    
    # 前フレームのグラフを初期化
    ax.cla()

    # 分散共分散行列を取得
    sigma_dd = trace_sigma_lt[frame_i]

    # 固有値・固有ベクトルを計算
    res_eigen = np.linalg.eig(sigma_dd)
    lambda_d  = res_eigen[0]
    u_dd      = res_eigen[1].T
    
    # ラベル用の文字列を作成
    mu_str    = '(' + ', '.join(str(val.round(2)) for val in mu_d) + ')'
    sigma_str = '(' + ', '.join('(' + ', '.join(str(val.round(2)) for val in vec) + ')' for vec in sigma_dd) + ')'
    param_label  = '$D = 2$\n'
    param_label += '$\\mu = ' + mu_str + '$\n'
    param_label += '$\\Sigma = ' + sigma_str + '$'
    
    # 2Dマハラノビス距離を作図
    tmp_u_str       = '(' + ', '.join(str(val.round(2)) for val in u_dd[0]) + ')'
    tmp_eigen_label = f'$\\lambda_0 = {lambda_d[0]:.2f}, u_0 = ' + tmp_u_str + '$'
    tmp_eigen_d = u_dd[0] * np.sqrt(lambda_d[0])
    ax.plot([mu_d[0]-tmp_eigen_d[0], mu_d[0]+tmp_eigen_d[0]], 
            [mu_d[1]-tmp_eigen_d[1], mu_d[1]+tmp_eigen_d[1]], 
            color='C1', label=tmp_eigen_label) # 楕円体の0軸方向の距離1の線分
    tmp_u_str       = '(' + ', '.join(str(val.round(2)) for val in u_dd[1]) + ')'
    tmp_eigen_label = f'$\\lambda_1 = {lambda_d[1]:.2f}, u_1 = ' + tmp_u_str + '$'
    tmp_eigen_d = u_dd[1] * np.sqrt(lambda_d[1])
    ax.plot([mu_d[0]-tmp_eigen_d[0], mu_d[0]+tmp_eigen_d[0]], 
            [mu_d[1]-tmp_eigen_d[1], mu_d[1]+tmp_eigen_d[1]], 
            color='C2', label=tmp_eigen_label) # 楕円体の1軸方向の距離1の線分
    ax.plot(euclid_x0, euclid_x1, 
            color='C0') # ユークリッド距離
    ax.contour(X0, X1, trace_dist_lt[frame_i].reshape(grid_shape), 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # マハラノビス距離
    ax.set_xlim(xmin=x0_min, xmax=x0_max)
    ax.set_ylim(ymin=x1_min, ymax=x1_max)
    ax.set_xlabel('$x_0$')
    ax.set_ylabel('$x_1$')
    ax.set_title(param_label, loc='left')
    ax.grid()
    ax.legend(loc='upper left')
    ax.set_aspect('equal', adjustable='box')

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/2d_cosigma.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%


