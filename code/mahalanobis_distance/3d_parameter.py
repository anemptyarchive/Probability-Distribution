
# マハラノビス距離の作図
# 3次元変数の場合
# パラメータと形状の関係

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
mu2_vals = np.linspace(start=2.5, stop=-7.5, num=frame_num)

# 分散共分散行列を指定
sigma_dd = np.diag(np.ones(3))

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
x2_min = mu2_vals.min() - sgm_num * np.sqrt(sigma_dd[2, 2])
x2_max = mu2_vals.max() + sgm_num * np.sqrt(sigma_dd[2, 2])
print(x0_min.round(2), x0_max.round(2))
print(x1_min.round(2), x1_max.round(2))
print(x2_min.round(2), x2_max.round(2))

# 各軸の値を作成
x0 = np.linspace(start=x0_min, stop=x0_max, num=50) # 0軸方向の点の数を指定
x1 = np.linspace(start=x1_min, stop=x1_max, num=50) # 1軸方向の点の数を指定
x2 = np.linspace(start=x2_min, stop=x2_max, num=100) # 2軸方向の線の数を指定

# 0・1軸の格子点を作成
X0, X1 = np.meshgrid(x0, x1)

# 0・1軸の形状を設定
grid_shape = X0.shape
grid_size  = X0.size
print(grid_size)

# %%

## 距離の計算

# フレームごとに処理
trace_mu_lt   = []
trace_dist_lt = []
for frame_i in range(frame_num):

    # 平均ベクトルを作成
    mu_d = np.array(
        [mu0_vals[frame_i], mu1_vals[frame_i], mu2_vals[frame_i]]
    )

    # マハラノビス距離を計算
    dist_arr = np.tile(np.nan, reps=(x2.size, grid_size)) # 受け皿
    for i in range(x2.size):
        X = np.stack([X0.flatten(), X1.flatten(), x2[i].repeat(grid_size)], axis=1) # 座標
        dist_arr[i] = np.array(
            [np.sqrt((x_d - mu_d).T @ inv_sigma_dd @ (x_d - mu_d)) for x_d in X]
        ) # 距離
    
    # 結果を格納
    trace_mu_lt.append(mu_d.copy())
    trace_dist_lt.append(dist_arr)
    
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

# 軸サイズを設定
x0_size = x0_max - x0_min
x1_size = x1_max - x1_min
x2_size = x2_max - x2_min
print(x0_size)
print(x1_size)
print(x2_size)

# %% 

# ラベル用の文字列を作成
def_label = '$\\Delta = \\sqrt{(x - \\mu)^{T} \\Sigma^{-1} (x - \\mu)}$'

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(8, 8), dpi=100, facecolor='white', constrained_layout=True, 
                       subplot_kw={'projection': '3d'})
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
    param_label  = '$D = 3$\n'
    param_label += '$\\mu = ' + mu_str + '$\n'
    param_label += '$\\Sigma = ' + sigma_str + '$'
    
    # 3Dマハラノビス距離を作図
    ax.scatter(*mu_d, 
               color='red', s=100, marker='x', label='$\\mu$') # 中心
    ax.plot([mu_d[0], x0_min], [mu_d[1], mu_d[1]], [mu_d[2], mu_d[2]], 
            color='black', linestyle='dotted') # 中心の0軸座標
    ax.plot([mu_d[0], mu_d[0]], [mu_d[1], x1_max], [mu_d[2], mu_d[2]], 
            color='black', linestyle='dotted') # 中心の1軸座標
    ax.plot([mu_d[0], mu_d[0]], [mu_d[1], mu_d[1]], [mu_d[2], x2_min], 
            color='black', linestyle='dotted') # 中心の2軸座標
    for i in range(x2.size):
        ax.contour(X0, X1, trace_dist_lt[frame_i][i].reshape(grid_shape), offset=x2[i], 
                   cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels, alpha=0.5, 
                   linewidths=1) # マハラノビス距離
    ax.set_xlim(xmin=x0_min, xmax=x0_max)
    ax.set_ylim(ymin=x1_min, ymax=x1_max)
    ax.set_zlim(zmin=x2_min, zmax=x2_max)
    ax.set_xlabel('$x_0$')
    ax.set_ylabel('$x_1$')
    ax.set_zlabel('$x_2$')
    ax.set_title(param_label, loc='left')
    ax.legend(loc='upper right')
    ax.set_box_aspect([1.0, x1_size/x0_size, x2_size/x0_size])
    #ax.view_init(elev=90, azim=270) # 0・1軸平面
    #ax.view_init(elev=0, azim=270) # 0・2軸平面
    #ax.view_init(elev=0, azim=0) # 1・2軸平面

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/3d_mu.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%

### 分散パラメータの影響 ---------------------------------------------------------

## パラメータの設定

# フレーム数を指定
frame_num = 100

# 平均ベクトルを指定
mu_d = np.zeros(3)

# 分散共分散行列を指定
sigma_dd     = np.diag(np.ones(3))
sigma00_vals = np.linspace(start=0.1, stop=20.0, num=frame_num)
sigma11_vals = np.linspace(start=0.1, stop=10.0, num=frame_num)
sigma22_vals = np.linspace(start=0.1, stop=5.0, num=frame_num)

# %%

## 座標の作成

# 各軸の範囲の調整値を指定
sgm_num = 1.0

# 各軸の範囲を設定
x0_min = mu_d[0] - sgm_num * np.sqrt(sigma00_vals.max())
x0_max = mu_d[0] + sgm_num * np.sqrt(sigma00_vals.max())
x1_min = mu_d[1] - sgm_num * np.sqrt(sigma11_vals.max())
x1_max = mu_d[1] + sgm_num * np.sqrt(sigma11_vals.max())
x2_min = mu_d[2] - sgm_num * np.sqrt(sigma22_vals.max())
x2_max = mu_d[2] + sgm_num * np.sqrt(sigma22_vals.max())
print(x0_min.round(2), x0_max.round(2))
print(x1_min.round(2), x1_max.round(2))
print(x2_min.round(2), x2_max.round(2))

# 各軸の値を作成
x0 = np.linspace(start=x0_min, stop=x0_max, num=50) # 0軸方向の点の数を指定
x1 = np.linspace(start=x1_min, stop=x1_max, num=50) # 1軸方向の点の数を指定
x2 = np.linspace(start=x2_min, stop=x2_max, num=100) # 2軸方向の線の数を指定

# 0・1軸の格子点を作成
X0, X1 = np.meshgrid(x0, x1)

# 0・1軸の形状を設定
grid_shape = X0.shape
grid_size  = X0.size
print(grid_size)

# %%

## 距離の計算

# フレームごとに処理
trace_sigma_lt = []
trace_dist_lt  = []
for frame_i in range(frame_num):

    # 分散共分散行列を作成
    sigma_dd[0, 0] = sigma00_vals[frame_i]
    sigma_dd[1, 1] = sigma11_vals[frame_i]
    sigma_dd[2, 2] = sigma22_vals[frame_i]
    
    # 精度行列を計算
    inv_sigma_dd = np.linalg.inv(sigma_dd)

    # マハラノビス距離を計算
    dist_arr = np.tile(np.nan, reps=(x2.size, grid_size)) # 受け皿
    for i in range(x2.size):
        X = np.stack([X0.flatten(), X1.flatten(), x2[i].repeat(grid_size)], axis=1) # 座標
        dist_arr[i] = np.array(
            [np.sqrt((x_d - mu_d).T @ inv_sigma_dd @ (x_d - mu_d)) for x_d in X]
        ) # 距離
    
    # 結果を格納
    trace_sigma_lt.append(sigma_dd.copy())
    trace_dist_lt.append(dist_arr)
    
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

# 軸サイズを設定
x0_size = x0_max - x0_min
x1_size = x1_max - x1_min
x2_size = x2_max - x2_min
print(x0_size)
print(x1_size)
print(x2_size)

# %%

# ユークリッド距離(球面の座標)を計算
T, U = np.meshgrid(
    np.linspace(start=0.0, stop=2.0*np.pi, num=61), 
    np.linspace(start=0.0, stop=2.0*np.pi, num=61)
) # ラジアン
euclid_X0 = mu_d[0] + dist_val * np.sin(T) * np.cos(U)
euclid_X1 = mu_d[1] + dist_val * np.sin(T) * np.sin(U)
euclid_X2 = mu_d[2] + dist_val * np.cos(T)

# ラベル用の文字列を作成
def_label = '$\\Delta = \\sqrt{(x - \\mu)^{T} \\Sigma^{-1} (x - \\mu)}$'

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(8, 8), dpi=100, facecolor='white', constrained_layout=True, 
                       subplot_kw={'projection': '3d'})
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
    param_label  = '$D = 3$\n'
    param_label += '$\\mu = ' + mu_str + '$\n'
    param_label += '$\\Sigma = ' + sigma_str + '$'
    
    # 3Dマハラノビス距離を作図
    ax.plot([mu_d[0], mu_d[0]+sigma_d[0]], [mu_d[1], mu_d[1]], [mu_d[2], mu_d[2]], 
            color='C1', label=f'$\\sigma_0 = {np.sqrt(sigma_dd[0, 0]):.2f}$') # 0軸方向の標準偏差1の線分
    ax.plot([mu_d[0], mu_d[0]], [mu_d[1], mu_d[1]+sigma_d[1]], [mu_d[2], mu_d[2]], 
            color='C2', label=f'$\\sigma_1 = {np.sqrt(sigma_dd[1, 1]):.2f}$') # 1軸方向の標準偏差1の線分
    ax.plot([mu_d[0], mu_d[0]], [mu_d[1], mu_d[1]], [mu_d[2], mu_d[2]+sigma_d[2]], 
            color='C3', label=f'$\\sigma_2 = {np.sqrt(sigma_dd[2, 2]):.2f}$') # 2軸方向の標準偏差1の線分
    ax.plot_wireframe(euclid_X0, euclid_X1, euclid_X2, 
                      color='C0', alpha=0.2, linewidth=1) # ユークリッド距離
    for i in range(x2.size):
        ax.contour(X0, X1, trace_dist_lt[frame_i][i].reshape(grid_shape), offset=x2[i], 
                   cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels, alpha=0.5, 
                   linewidths=1) # マハラノビス距離
    ax.set_xlim(xmin=x0_min, xmax=x0_max)
    ax.set_ylim(ymin=x1_min, ymax=x1_max)
    ax.set_zlim(zmin=x2_min, zmax=x2_max)
    ax.set_xlabel('$x_0$')
    ax.set_ylabel('$x_1$')
    ax.set_zlabel('$x_2$')
    ax.set_title(param_label, loc='left')
    ax.legend(loc='upper left')
    ax.set_box_aspect([1.0, x1_size/x0_size, x2_size/x0_size])
    #ax.view_init(elev=90, azim=270) # 0・1軸平面
    #ax.view_init(elev=0, azim=270) # 0・2軸平面
    #ax.view_init(elev=0, azim=0) # 1・2軸平面

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/3d_sigma2.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%

### 共分散パラメータの影響 -------------------------------------------------------

## パラメータの設定

# フレーム数を指定
frame_num = 100

# 平均ベクトルを指定
mu_d = np.zeros(3)

# 分散共分散行列を指定
sigma_dd     = np.diag(np.ones(3)) * 4.0
sigma01_vals = np.linspace(start=0.0, stop=3.0, num=frame_num)
sigma02_vals = np.linspace(start=-2.5, stop=2.5, num=frame_num)
sigma12_vals = np.linspace(start=0.0, stop=0.0, num=frame_num)

# %%

## 座標の作成

# 各軸の範囲の調整値を指定
sgm_num = 1.0

# 各軸の範囲を設定
x0_min = mu_d[0] - sgm_num * np.sqrt(sigma_dd[0, 0])
x0_max = mu_d[0] + sgm_num * np.sqrt(sigma_dd[0, 0])
x1_min = mu_d[1] - sgm_num * np.sqrt(sigma_dd[1, 1])
x1_max = mu_d[1] + sgm_num * np.sqrt(sigma_dd[1, 1])
x2_min = mu_d[2] - sgm_num * np.sqrt(sigma_dd[2, 2])
x2_max = mu_d[2] + sgm_num * np.sqrt(sigma_dd[2, 2])
print(x0_min.round(2), x0_max.round(2))
print(x1_min.round(2), x1_max.round(2))
print(x2_min.round(2), x2_max.round(2))

# 各軸の値を作成
x0 = np.linspace(start=x0_min, stop=x0_max, num=50) # 0軸方向の点の数を指定
x1 = np.linspace(start=x1_min, stop=x1_max, num=50) # 1軸方向の点の数を指定
x2 = np.linspace(start=x2_min, stop=x2_max, num=100) # 2軸方向の線の数を指定

# 0・1軸の格子点を作成
X0, X1 = np.meshgrid(x0, x1)

# 0・1軸の形状を設定
grid_shape = X0.shape
grid_size  = X0.size
print(grid_size)

# %%

## 距離の計算

# フレームごとに処理
trace_sigma_lt = []
trace_dist_lt  = []
for frame_i in range(frame_num):

    # 分散共分散行列を作成
    sigma_dd[0, 1] = sigma01_vals[frame_i]
    sigma_dd[1, 0] = sigma01_vals[frame_i]
    sigma_dd[0, 2] = sigma02_vals[frame_i]
    sigma_dd[2, 0] = sigma02_vals[frame_i]
    sigma_dd[1, 2] = sigma12_vals[frame_i]
    sigma_dd[2, 1] = sigma12_vals[frame_i]
    
    # 精度行列を計算
    inv_sigma_dd = np.linalg.inv(sigma_dd)

    # マハラノビス距離を計算
    dist_arr = np.tile(np.nan, reps=(x2.size, grid_size)) # 受け皿
    for i in range(x2.size):
        X = np.stack([X0.flatten(), X1.flatten(), x2[i].repeat(grid_size)], axis=1) # 座標
        dist_arr[i] = np.array(
            [np.sqrt((x_d - mu_d).T @ inv_sigma_dd @ (x_d - mu_d)) for x_d in X]
        ) # 距離
    
    # 結果を格納
    trace_sigma_lt.append(sigma_dd.copy())
    trace_dist_lt.append(dist_arr)
    
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

# 軸サイズを設定
x0_size = x0_max - x0_min
x1_size = x1_max - x1_min
x2_size = x2_max - x2_min
print(x0_size)
print(x1_size)
print(x2_size)

# %%

# ユークリッド距離(球面の座標)を計算
T, U = np.meshgrid(
    np.linspace(start=0.0, stop=2.0*np.pi, num=61), 
    np.linspace(start=0.0, stop=2.0*np.pi, num=61)
) # ラジアン
euclid_X0 = mu_d[0] + dist_val * np.sin(T) * np.cos(U)
euclid_X1 = mu_d[1] + dist_val * np.sin(T) * np.sin(U)
euclid_X2 = mu_d[2] + dist_val * np.cos(T)

# ラベル用の文字列を作成
def_label = '$\\Delta = \\sqrt{(x - \\mu)^{T} \\Sigma^{-1} (x - \\mu)}$'

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(8, 8), dpi=100, facecolor='white', constrained_layout=True, 
                       subplot_kw={'projection': '3d'})
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
    param_label  = '$D = 3$\n'
    param_label += '$\\mu = ' + mu_str + '$\n'
    param_label += '$\\Sigma = ' + sigma_str + '$'
    
    # 3Dマハラノビス距離を作図
    tmp_u_str       = '(' + ', '.join(str(val.round(2)) for val in u_dd[0]) + ')'
    tmp_eigen_label = f'$\\lambda_0 = {lambda_d[0]:.2f}, u_0 = ' + tmp_u_str + '$'
    tmp_eigen_d = u_dd[0] * np.sqrt(lambda_d[0])
    ax.plot([mu_d[0]-tmp_eigen_d[0], mu_d[0]+tmp_eigen_d[0]], 
            [mu_d[1]-tmp_eigen_d[1], mu_d[1]+tmp_eigen_d[1]], 
            [mu_d[2]-tmp_eigen_d[2], mu_d[2]+tmp_eigen_d[2]], 
            color='C1', label=tmp_eigen_label) # 楕円体の0軸方向の距離1の線分
    tmp_u_str       = '(' + ', '.join(str(val.round(2)) for val in u_dd[1]) + ')'
    tmp_eigen_label = f'$\\lambda_1 = {lambda_d[1]:.2f}, u_1 = ' + tmp_u_str + '$'
    tmp_eigen_d = u_dd[1] * np.sqrt(lambda_d[1])
    ax.plot([mu_d[0]-tmp_eigen_d[0], mu_d[0]+tmp_eigen_d[0]], 
            [mu_d[1]-tmp_eigen_d[1], mu_d[1]+tmp_eigen_d[1]], 
            [mu_d[2]-tmp_eigen_d[2], mu_d[2]+tmp_eigen_d[2]], 
            color='C2', label=tmp_eigen_label) # 楕円体の1軸方向の距離1の線分
    tmp_u_str       = '(' + ', '.join(str(val.round(2)) for val in u_dd[2]) + ')'
    tmp_eigen_label = f'$\\lambda_2 = {lambda_d[2]:.2f}, u_2 = ' + tmp_u_str + '$'
    tmp_eigen_d = u_dd[2] * np.sqrt(lambda_d[2])
    ax.plot([mu_d[0]-tmp_eigen_d[0], mu_d[0]+tmp_eigen_d[0]], 
            [mu_d[1]-tmp_eigen_d[1], mu_d[1]+tmp_eigen_d[1]], 
            [mu_d[2]-tmp_eigen_d[2], mu_d[2]+tmp_eigen_d[2]], 
            color='C3', label=tmp_eigen_label) # 楕円体の2軸方向の距離1の線分
    ax.plot_wireframe(euclid_X0, euclid_X1, euclid_X2, 
                      color='C0', alpha=0.2, linewidth=1) # ユークリッド距離
    for i in range(x2.size):
        ax.contour(X0, X1, trace_dist_lt[frame_i][i].reshape(grid_shape), offset=x2[i], 
                   cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels, alpha=0.5, 
                   linewidths=1) # マハラノビス距離
    ax.set_xlim(xmin=x0_min, xmax=x0_max)
    ax.set_ylim(ymin=x1_min, ymax=x1_max)
    ax.set_zlim(zmin=x2_min, zmax=x2_max)
    ax.set_xlabel('$x_0$')
    ax.set_ylabel('$x_1$')
    ax.set_zlabel('$x_2$')
    ax.set_title(param_label, loc='left')
    ax.legend(loc='upper left')
    ax.set_box_aspect([1.0, x1_size/x0_size, x2_size/x0_size])
    #ax.view_init(elev=90, azim=270) # 0・1軸平面
    #ax.view_init(elev=0, azim=270) # 0・2軸平面
    #ax.view_init(elev=0, azim=0) # 1・2軸平面

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/3d_cosigma.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%


