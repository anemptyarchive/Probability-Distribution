# マハラノビス距離
## 2次元変数の場合
### パラメータと形状の関係

# %%

# ライブラリを読込
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from matplotlib import cm


# %%

# 平均パラメータの影響 -----------------------------------------------------------

## パラメータの設定

# フレーム数を指定
frame_num = 101

# 次元数を設定:(固定)
D = 2

# 平均ベクトルを指定
mu1_vals = np.linspace(start=-5.0, stop=5.0, num=frame_num) # 範囲を指定
mu2_vals = np.linspace(start=0.0, stop=10.0, num=frame_num) # 範囲を指定

# 分散共分散行列を指定
sigma_dd = np.identity(D)

# 精度行列を計算
inv_sigma_dd = np.linalg.inv(sigma_dd)

# %%

## 座標の作成

# 各軸の範囲の調整値を指定
sgm_num = 3.0

# 各軸の範囲を設定
x1_min = np.floor(mu1_vals.min() - sgm_num * np.sqrt(sigma_dd[0, 0]))
x1_max =  np.ceil(mu1_vals.max() + sgm_num * np.sqrt(sigma_dd[0, 0]))
x2_min = np.floor(mu2_vals.min() - sgm_num * np.sqrt(sigma_dd[1, 1]))
x2_max =  np.ceil(mu2_vals.max() + sgm_num * np.sqrt(sigma_dd[1, 1]))
print(x1_min, x1_max)
print(x2_min, x2_max)

# 各軸の点を作成
x1 = np.linspace(start=x1_min, stop=x1_max, num=50) # 点の数を指定
x2 = np.linspace(start=x2_min, stop=x2_max, num=50) # 点の数を指定

# 格子点を作成
X1, X2 = np.meshgrid(x1, x2)
X = np.stack([X1.flatten(), X2.flatten()], axis=1)

# 格子点の形状を設定
grid_shape = X1.shape
grid_size  = X1.size
print(grid_shape)
print(grid_size)

# %%

## 距離の計算

# フレームごとに処理
trace_mu_lt   = []
trace_dist_lt = []
for frame_i in range(frame_num):

    # 平均ベクトルを作成
    mu_d = np.array(
        [mu1_vals[frame_i], mu2_vals[frame_i]]
    )

    # マハラノビス距離を計算
    dist_vec = np.array(
        [np.sqrt((x_d - mu_d) @ inv_sigma_dd @ (x_d - mu_d)) for x_d in X]
    )
    
    # 結果を格納
    trace_mu_lt.append(mu_d.copy())
    trace_dist_lt.append(dist_vec.copy())
    
    # 途中経過を表示
    print(f'frame: {frame_i+1} / {frame_num}')

# %%

## 等高線の設定

# グラデーションの範囲を設定
u = 5.0
dist_min = 0.0
dist_max = np.ceil(np.array(trace_dist_lt).max() /u)*u # u単位で切り上げ
print(dist_max)

# 等高線の間隔を設定
dist_levels = np.linspace(start=dist_min, stop=dist_max, num=11) # 線の数を指定
print(dist_levels)

# ラベル用の文字列を作成
def_label = '$\\Delta = \\sqrt{(x - \\mu)^{T} \\Sigma^{-1} (x - \\mu)}$'

# %%

## 等高線図の作成

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(10, 8), dpi=100, facecolor='white', constrained_layout=True)
fig.suptitle("Mahalanobis' Distance", fontsize=20)
cs = ax.contour(X1, X2, np.zeros(shape=grid_shape), 
                cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # カラーバー表示用のダミー
fig.colorbar(cs, ax=ax, shrink=1.0, label=def_label)

# 作図処理を定義
def update(frame_i):
    
    # 前フレームのグラフを初期化
    ax.cla()

    # 平均ベクトルを取得
    mu_d = trace_mu_lt[frame_i]

    # ラベル用の文字列を作成
    mu_str    = '(' + ', '.join([f'{val:.2f}' for val in mu_d]) + ')'
    sigma_str = '(' + ', '.join(['(' + ', '.join([f'{val:.2f}' for val in vec]) + ')' for vec in sigma_dd]) + ')'
    param_label  = '$D = 2$\n'
    param_label += '$\\mu = ' + mu_str + '$\n'
    param_label += '$\\Sigma = ' + sigma_str + '$'
    
    # 2Dマハラノビス距離を描画
    ax.plot([mu_d[0], x1_min], [mu_d[1], mu_d[1]], 
            color='black', linestyle='dotted') # 中心のx軸座標
    ax.plot([mu_d[0], mu_d[0]], [mu_d[1], x2_min], 
            color='black', linestyle='dotted') # 中心のy軸座標
    ax.scatter(x=mu_d[0], y=mu_d[1], 
               color='black', s=100, label='$\\mu$') # 中心
    ax.contour(X1, X2, trace_dist_lt[frame_i].reshape(grid_shape), 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # 等高線
    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')
    ax.set_title(param_label, loc='left')
    ax.legend(loc='upper right')
    ax.grid()
    ax.set_xlim(xmin=x1_min, xmax=x1_max)
    ax.set_ylim(ymin=x2_min, ymax=x2_max)
    ax.set_aspect('equal', adjustable='box')

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/2d_parameter/2d_mu_contour.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %% 

## 曲面図の作成

# 軸サイズを設定
x1_size = x1_max - x1_min
x2_size = x2_max - x2_min

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(9, 8), dpi=100, facecolor='white', 
                       subplot_kw={'projection': '3d'}, constrained_layout=True)
fig.suptitle("Mahalanobis' Distance", fontsize=20)

# 作図処理を定義
def update(frame_i):
    
    # 前フレームのグラフを初期化
    ax.cla()

    # 平均ベクトルを取得
    mu_d = trace_mu_lt[frame_i]

    # ラベル用の文字列を作成
    mu_str    = '(' + ', '.join([f'{val:.2f}' for val in mu_d]) + ')'
    sigma_str = '(' + ', '.join(['(' + ', '.join([f'{val:.2f}' for val in vec]) + ')' for vec in sigma_dd]) + ')'
    param_label  = '$D = 2$\n'
    param_label += '$\\mu = ' + mu_str + '$\n'
    param_label += '$\\Sigma = ' + sigma_str + '$'
    
    # 2Dマハラノビス距離を描画
    ax.plot([mu_d[0], x1_max], [mu_d[1], mu_d[1]], [dist_min, dist_min], 
            color='black', linestyle='dotted') # 中心のx軸座標
    ax.plot([mu_d[0], mu_d[0]], [mu_d[1], x2_min], [dist_min, dist_min], 
            color='black', linestyle='dotted') # 中心のy軸座標
    ax.scatter(xs=mu_d[0], ys=mu_d[1], zs=dist_min, 
               color='black', s=100, label='$\\mu$') # 中心
    ax.contour(X1, X2, trace_dist_lt[frame_i].reshape(grid_shape), offset=dist_min, 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # 等高線(座標平面上)
    ax.contour(X1, X2, trace_dist_lt[frame_i].reshape(grid_shape), 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels, 
               linestyles='dotted') # 等高線(曲面上)
    ax.plot_surface(X1, X2, trace_dist_lt[frame_i].reshape(grid_shape), 
                    cmap='viridis', vmin=dist_min, vmax=dist_max, alpha=0.8) # 曲面
    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')
    ax.set_zlabel(def_label)
    ax.set_title(param_label, loc='left')
    ax.legend(loc='upper right')
    ax.set_xlim(xmin=x1_min, xmax=x1_max)
    ax.set_ylim(ymin=x2_min, ymax=x2_max)
    ax.set_zlim(zmin=dist_min ,zmax=dist_max)
    ax.set_box_aspect([1.0, x2_size/x1_size, 1.0]) # 高さ(横サイズに対する比)を指定

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/2d_parameter/2d_mu_surface.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)


# %%

# 分散パラメータの影響 -----------------------------------------------------------

## パラメータの設定

# フレーム数を指定
frame_num = 101

# 次元数を設定:(固定)
D = 2

# 平均ベクトルを指定
mu_d = np.zeros(D)

# 分散共分散行列を指定
sigma_dd     = np.identity(D)
sigma11_vals = np.linspace(start=0.1, stop=9.0, num=frame_num) # 範囲を指定
sigma22_vals = np.linspace(start=0.1, stop=4.0, num=frame_num) # 範囲を指定

# %%

## 座標の作成

# 各軸の範囲の調整値を指定
sgm_num = 1.0

# 各軸の範囲を設定
x1_min = np.floor(mu_d[0] - sgm_num * np.sqrt(sigma11_vals.max()))
x1_max =  np.ceil(mu_d[0] + sgm_num * np.sqrt(sigma11_vals.max()))
x2_min = np.floor(mu_d[1] - sgm_num * np.sqrt(sigma22_vals.max()))
x2_max =  np.ceil(mu_d[1] + sgm_num * np.sqrt(sigma22_vals.max()))
print(x1_min, x1_max)
print(x2_min, x2_max)

# 各軸の点を作成
x1 = np.linspace(start=x1_min, stop=x1_max, num=50) # 点の数を指定
x2 = np.linspace(start=x2_min, stop=x2_max, num=50) # 点の数を指定

# 格子点を作成
X1, X2 = np.meshgrid(x1, x2)
X = np.stack([X1.flatten(), X2.flatten()], axis=1)

# 格子点の形状を設定
grid_shape = X1.shape
grid_size  = X1.size
print(grid_shape)
print(grid_size)

# %%

## 距離の計算

# フレームごとに処理
trace_sigma_lt = []
trace_dist_lt  = []
for frame_i in range(frame_num):

    # 分散共分散行列を作成
    sigma_dd[0, 0] = sigma11_vals[frame_i]
    sigma_dd[1, 1] = sigma22_vals[frame_i]
    
    # 精度行列を計算
    inv_sigma_dd = np.linalg.inv(sigma_dd)

    # マハラノビス距離を計算
    dist_vec = np.array(
        [np.sqrt((x_d - mu_d) @ inv_sigma_dd @ (x_d - mu_d)) for x_d in X]
    )
    
    # 結果を格納
    trace_sigma_lt.append(sigma_dd.copy())
    trace_dist_lt.append(dist_vec)
    
    # 途中経過を表示
    print(f'frame: {frame_i+1} / {frame_num}')

# %%

## 等高線の設定

# グラデーションの範囲を設定
u = 1.0
dist_min = 0.0
dist_max = np.ceil(np.array(trace_dist_lt).max() /u)*u # u単位で切り上げ
print(dist_max)

# 等高線の位置を設定
dist_levels = np.linspace(start=dist_min, stop=dist_max, num=13) # 線の数を指定
print(dist_levels)

# ラベル用の文字列を作成
def_label = '$\\Delta = \\sqrt{(x - \\mu)^{T} \\Sigma^{-1} (x - \\mu)}$'

# %%

## 等高線図の作成

# 矢印の装飾を指定
ls_lt = ['dashed', 'dashdot']

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white', constrained_layout=True)
fig.suptitle("Mahalanobis' Distance", fontsize=20)
cs = ax.contour(X1, X2, np.zeros(shape=grid_shape), 
                cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # カラーバー表示用のダミー
fig.colorbar(cs, ax=ax, shrink=1.0, label=def_label)

# 作図処理を定義
def update(frame_i):
    
    # 前フレームのグラフを初期化
    ax.cla()

    # 分散共分散行列を取得
    sigma_dd = trace_sigma_lt[frame_i]

    # 標準偏差を計算
    sigma_d = np.sqrt(np.diag(sigma_dd))
    
    # ラベル用の文字列を作成
    mu_str    = '(' + ', '.join([f'{val:.2f}' for val in mu_d]) + ')'
    sigma_str = '(' + ', '.join(['(' + ', '.join([f'{val:.2f}' for val in vec]) + ')' for vec in sigma_dd]) + ')'
    param_label  = '$D = 2$\n'
    param_label += '$\\mu = ' + mu_str + '$\n'
    param_label += '$\\Sigma = ' + sigma_str + '$'
    
    # 2Dマハラノビス距離を描画
    for d in range(D):
        tmp_sigma_d    = np.zeros(D)
        tmp_sigma_d[d] = sigma_d[d]
        ax.quiver(mu_d[0], mu_d[1], 
                  tmp_sigma_d[0], tmp_sigma_d[1], 
                  angles='xy', scale_units='xy', scale=1.0, 
                  units='dots', width=2.0, headwidth=5.0, headlength=5.0, headaxislength=2.5, 
                  fc='white', ec='black', linewidth=1.0, linestyle=ls_lt[d], 
                  label=f'$\\sigma_{d+1} = {sigma_d[d]:.2f}$') # d軸方向の標準偏差1つ分の線分
    ax.contour(X1, X2, trace_dist_lt[frame_i].reshape(grid_shape), 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # 等高線
    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')
    ax.set_title(param_label, loc='left')
    ax.legend(loc='upper left')
    ax.grid()
    ax.set_xlim(xmin=x1_min, xmax=x1_max)
    ax.set_ylim(ymin=x2_min, ymax=x2_max)
    ax.set_aspect('equal', adjustable='box')

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/2d_parameter/2d_sigma2_contour.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%

## 曲面図の作成

# 軸サイズを設定
x1_size = x1_max - x1_min
x2_size = x2_max - x2_min

# 矢印の装飾を指定
alr = 0.2
ls_lt = ['dashed', 'dashdot']

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(9, 8), dpi=100, facecolor='white', 
                       subplot_kw={'projection': '3d'}, constrained_layout=True)
fig.suptitle("Mahalanobis' Distance", fontsize=20)

# 作図処理を定義
def update(frame_i):
    
    # 前フレームのグラフを初期化
    ax.cla()

    # 分散共分散行列を取得
    sigma_dd = trace_sigma_lt[frame_i]

    # 標準偏差を計算
    sigma_d = np.sqrt(np.diag(sigma_dd))
    
    # ラベル用の文字列を作成
    mu_str    = '(' + ', '.join([f'{val:.2f}' for val in mu_d]) + ')'
    sigma_str = '(' + ', '.join(['(' + ', '.join([f'{val:.2f}' for val in vec]) + ')' for vec in sigma_dd]) + ')'
    param_label  = '$D = 2$\n'
    param_label += '$\\mu = ' + mu_str + '$\n'
    param_label += '$\\Sigma = ' + sigma_str + '$'
    
    # 2Dマハラノビス距離を描画
    for d in range(D):
        tmp_sigma_d    = np.zeros(D)
        tmp_sigma_d[d] = sigma_d[d]
        ax.quiver(mu_d[0], mu_d[1], dist_min, 
                  tmp_sigma_d[0], tmp_sigma_d[1], 0.0, 
                  arrow_length_ratio=alr/np.sqrt(np.sum(tmp_sigma_d**2)), 
                  color='black', linewidth=1.0, linestyle=[ls_lt[d], 'solid', 'solid'], 
                  label=f'$\\sigma_{d+1}={sigma_d[d]:.2f}$') # d軸方向の標準偏差1つ分の線分
    ax.contour(X1, X2, trace_dist_lt[frame_i].reshape(grid_shape), offset=dist_min, 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # 等高線(座標平面上)
    ax.contour(X1, X2, trace_dist_lt[frame_i].reshape(grid_shape), 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels, 
               linestyles='dotted') # 等高線(曲面上)
    ax.plot_surface(X1, X2, trace_dist_lt[frame_i].reshape(grid_shape), 
                    cmap='viridis', vmin=dist_min, vmax=dist_max, alpha=0.8) # 曲面
    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')
    ax.set_zlabel(def_label, labelpad=15.0)
    ax.set_title(param_label, loc='left')
    ax.legend(loc='upper left')
    ax.set_xlim(xmin=x1_min, xmax=x1_max)
    ax.set_ylim(ymin=x2_min, ymax=x2_max)
    ax.set_zlim(zmin=dist_min, zmax=dist_max)
    ax.set_box_aspect([1.0, x2_size/x1_size, 1.0]) # 高さ(横サイズに対する比)を指定

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/2d_parameter/2d_sigma2_surface.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)


# %%

# 共分散パラメータの影響 ---------------------------------------------------------

## パラメータの設定

# フレーム数を指定
frame_num = 101

# 次元数を設定:(固定)
D = 2

# 平均ベクトルを指定
mu_d = np.zeros(D)

# 分散共分散行列を指定
sigma_dd     = np.identity(D) * 4.0
sigma12_vals = np.linspace(start=-3.0, stop=3.0, num=frame_num) # 範囲を指定

# %%

## 座標の作成

# 各軸の範囲の調整値を指定
sgm_num = 1.0

# 各軸の範囲を設定
x1_min = np.floor(mu_d[0] - sgm_num * np.sqrt(sigma_dd[0, 0]))
x1_max =  np.ceil(mu_d[0] + sgm_num * np.sqrt(sigma_dd[0, 0]))
x2_min = np.floor(mu_d[1] - sgm_num * np.sqrt(sigma_dd[1, 1]))
x2_max =  np.ceil(mu_d[1] + sgm_num * np.sqrt(sigma_dd[1, 1]))
print(x1_min, x1_max)
print(x2_min, x2_max)

# 各軸の点を作成
x1 = np.linspace(start=x1_min, stop=x1_max, num=50) # 点の数を指定
x2 = np.linspace(start=x2_min, stop=x2_max, num=50) # 点の数を指定

# 格子点を作成
X1, X2 = np.meshgrid(x1, x2)
X = np.stack([X1.flatten(), X2.flatten()], axis=1)

# 格子点の形状を設定
grid_shape = X1.shape
grid_size  = X1.size
print(grid_shape)
print(grid_size)

# %%

## 距離の計算

# フレームごとに処理
trace_sigma_lt = []
trace_dist_lt  = []
for frame_i in range(frame_num):

    # 分散共分散行列を作成
    sigma_dd[0, 1] = sigma12_vals[frame_i]
    sigma_dd[1, 0] = sigma12_vals[frame_i]
    
    # 精度行列を計算
    inv_sigma_dd = np.linalg.inv(sigma_dd)

    # マハラノビス距離を計算
    dist_vec = np.array(
        [np.sqrt((x_d - mu_d) @ inv_sigma_dd @ (x_d - mu_d)) for x_d in X]
    )
    
    # 結果を格納
    trace_sigma_lt.append(sigma_dd.copy())
    trace_dist_lt.append(dist_vec)
    
    # 途中経過を表示
    print(f'frame: {frame_i+1} / {frame_num}')

# %%

## 等高線の設定

# グラデーションの範囲を設定
u = 1.0
dist_min = 0.0
dist_max = np.ceil(np.array(trace_dist_lt).max() /u)*u # u単位で切り上げ
print(dist_max)

# 等高線の位置を設定
dist_levels = np.linspace(start=dist_min, stop=dist_max, num=7) # 線の数を指定
print(dist_levels)

# ラベル用の文字列を作成
def_label  = '$\\Delta = \\sqrt{(x - \\mu)^{T} \\Sigma^{-1} (x - \\mu)}$'

# %%

## 等高線図の作成

# 矢印の装飾を指定
ls_lt = ['dashed', 'dashdot']

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(10, 10), dpi=100, facecolor='white', constrained_layout=True)
fig.suptitle("Mahalanobis' Distance", fontsize=20)
cs = ax.contour(X1, X2, np.zeros(shape=grid_shape), 
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
    mu_str    = '(' + ', '.join([f'{val:.2f}' for val in mu_d]) + ')'
    sigma_str = '(' + ', '.join(['(' + ', '.join([f'{val:.2f}' for val in vec]) + ')' for vec in sigma_dd]) + ')'
    param_label  = '$D = 2$\n'
    param_label += '$\\mu = ' + mu_str + '$\n'
    param_label += '$\\Sigma = ' + sigma_str + '$'
    
    # 2Dマハラノビス距離を描画
    for d in range(D):
        tmp_u_str       = '(' + ', '.join([f'{val:.2f}' for val in u_dd[d]]) + ')'
        tmp_eigen_label = f'$\\lambda_{d+1} = {lambda_d[d]:.2f}, u_{d+1} = ' + tmp_u_str + '$'
        tmp_eigen_d = u_dd[d] * np.sqrt(lambda_d[d])
        ax.quiver(mu_d[0]-tmp_eigen_d[0], mu_d[1]-tmp_eigen_d[1], 
                  2.0*tmp_eigen_d[0], 2.0*tmp_eigen_d[1], 
                  angles='xy', scale_units='xy', scale=1.0, 
                  units='dots', width=2.0, headwidth=10.0, headlength=10.0, headaxislength=2.5, 
                  fc='white', ec='black', linewidth=1.0, linestyle=ls_lt[d], 
                  label=tmp_eigen_label) # 楕円体のd軸方向の距離1の線分
    ax.contour(X1, X2, trace_dist_lt[frame_i].reshape(grid_shape), 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # 等高線
    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')
    ax.set_title(param_label, loc='left')
    ax.legend(loc='upper left')
    ax.grid()
    ax.set_xlim(xmin=x1_min, xmax=x1_max)
    ax.set_ylim(ymin=x2_min, ymax=x2_max)
    ax.set_aspect('equal', adjustable='box')

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/2d_parameter/2d_cosigma_contour.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%

## 曲面図の作成

# 軸サイズを設定
x1_size = x1_max - x1_min
x2_size = x2_max - x2_min

# 矢印の装飾を指定
alr = 0.2
ls_lt = ['dashed', 'dashdot']

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(9, 8), dpi=100, facecolor='white', 
                       subplot_kw={'projection': '3d'}, constrained_layout=True)
fig.suptitle("Mahalanobis' Distance", fontsize=20)

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
    mu_str    = '(' + ', '.join([f'{val:.2f}' for val in mu_d]) + ')'
    sigma_str = '(' + ', '.join(['(' + ', '.join([f'{val:.2f}' for val in vec]) + ')' for vec in sigma_dd]) + ')'
    param_label  = '$D = 2$\n'
    param_label += '$\\mu = ' + mu_str + '$\n'
    param_label += '$\\Sigma = ' + sigma_str + '$'
    
    # 2Dマハラノビス距離を描画
    for d in range(D):
        tmp_u_str       = '(' + ', '.join([f'{val:.2f}' for val in u_dd[d]]) + ')'
        tmp_eigen_label = f'$\\lambda_{d+1} = {lambda_d[d]:.2f}, u_{d+1} = ' + tmp_u_str + '$'
        tmp_eigen_d = u_dd[d] * np.sqrt(lambda_d[d])
        ax.quiver(mu_d[0]-tmp_eigen_d[0], mu_d[1]-tmp_eigen_d[1], dist_min, 
                  2.0*tmp_eigen_d[0], 2.0*tmp_eigen_d[1], 0.0, 
                  arrow_length_ratio=alr/np.sqrt(np.sum((2.0*tmp_eigen_d)**2)), 
                  color='black', linewidth=1.0, linestyle=[ls_lt[d], 'solid', 'solid'], 
                  label=tmp_eigen_label) # 楕円体のd軸方向の距離1の線分
    ax.contour(X1, X2, trace_dist_lt[frame_i].reshape(grid_shape), offset=dist_min, 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # 等高線(座標平面上)
    ax.contour(X1, X2, trace_dist_lt[frame_i].reshape(grid_shape), 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels, 
               linestyles='dotted') # 等高線(曲面上)
    ax.plot_surface(X1, X2, trace_dist_lt[frame_i].reshape(grid_shape), 
                    cmap='viridis', vmin=dist_min, vmax=dist_max, alpha=0.8) # 曲面
    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')
    ax.set_zlabel(def_label, labelpad=15.0)
    ax.set_title(param_label, loc='left')
    ax.legend(loc='upper left')
    ax.set_xlim(xmin=x1_min, xmax=x1_max)
    ax.set_ylim(ymin=x2_min, ymax=x2_max)
    ax.set_zlim(zmin=dist_min, zmax=dist_max)
    ax.set_box_aspect([1.0, x2_size/x1_size, 1.0]) # 高さ(横サイズに対する比)を指定

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/2d_parameter/2d_cosigma_surface.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)


# %%


