# マハラノビス距離の作図
## 2次元変数の場合
### 変数と距離の関係

# %%

# ライブラリを読込
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from matplotlib import cm


# %%

### 変数の影響 ------------------------------------------------------------------

## パラメータの設定

# フレーム数を指定
frame_num = 101

# 変数ベクトルを指定
x1_vals = np.linspace(start=0.0, stop=5.0, num=frame_num) # 範囲を指定
x2_vals = np.linspace(start=-5.0, stop=5.0, num=frame_num) # 範囲を指定

# 平均ベクトルを指定
mu_d = np.array([0.0, 0.0])

# 分散共分散行列を指定
sigma_dd = np.array(
    [[9.0, -2.3], 
     [-2.3, 4.0]]
)

# 精度行列を計算
inv_sigma_dd = np.linalg.inv(sigma_dd)

# %%

## 座標の作成

# 各軸の範囲の調整値を指定
sgm_num = 3.0

# 各軸の範囲を設定
x1_size = max(abs(x1_vals - mu_d[0]).max(), sgm_num * np.sqrt(sigma_dd[0, 0]))
x1_min  = np.floor(mu_d[0] - x1_size)
x1_max  =  np.ceil(mu_d[0] + x1_size)
x2_size = max(abs(x2_vals - mu_d[1]).max(), sgm_num * np.sqrt(sigma_dd[1, 1]))
x2_min  = np.floor(mu_d[1] - x2_size)
x2_max  =  np.ceil(mu_d[1] + x2_size)
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

# マハラノビス距離を計算
dist_vec = np.array(
    [np.sqrt((x_d - mu_d) @ inv_sigma_dd @ (x_d - mu_d)) for x_d in X]
)
trace_dist_vals = np.array(
    [np.sqrt((x_d - mu_d) @ inv_sigma_dd @ (x_d - mu_d)) for x_d in np.stack([x1_vals, x2_vals], axis=1)]
)

# %%

## 等高線の設定

# グラデーションの範囲を設定
u = 1.0
dist_min = 0.0
dist_max = np.ceil(dist_vec.max() /u)*u # u単位で切り上げ
print(dist_max)

# 等高線の位置を指定
dist_levels = np.linspace(start=dist_min, stop=dist_max, num=7) # 線の数を指定
print(dist_levels)

# %%

## 曲面図の作成

# ラベル用の文字列を作成
def_label = '$\\Delta = \\sqrt{(x - \\mu)^{T} \\Sigma^{-1} (x - \\mu)}$'
mu_str    = '(' + ', '.join([f'{val:.2f}' for val in mu_d]) + ')'
sigma_str = '(' + ', '.join(['(' + ', '.join([f'{val:.2f}' for val in vec]) + ')' for vec in sigma_dd]) + ')'
param_label  = '$D = 2$\n'
param_label += '$\\mu = ' + mu_str + '$\n'
param_label += '$\\Sigma = ' + sigma_str + '$'

# 軸サイズを設定
x1_size = x1_max - x1_min
x2_size = x2_max - x2_min

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(8, 8), dpi=100, facecolor='white', 
                       subplot_kw={'projection': '3d'}, constrained_layout=True)
fig.suptitle("Mahalanobis' Distance", fontsize=20)

# 作図処理を定義
def update(frame_i):
    
    # 前フレームのグラフを初期化
    ax.cla()

    # 変数を取得
    x_d = np.array([x1_vals[frame_i], x2_vals[frame_i]])
    x_label  = '$x = (' + ', '.join([f'{val:.2f}' for val in x_d]) + ')$'

    # 距離を取得
    dist_euclid_val = np.sqrt((x_d - mu_d) @ (x_d - mu_d))
    dist_mahal_val  = trace_dist_vals[frame_i]
   
    # 2Dマハラノビス距離を描画
    ax.plot([mu_d[0], x_d[0]], [mu_d[1], x_d[1]], [dist_min, dist_min], 
            color='black', linestyle='dashed', 
            label=f'$\\Delta_{{euclid}} = {dist_euclid_val:.3f}$') # ユークリッド距離
    ax.plot([x_d[0], x_d[0]], [x_d[1], x_d[1]], [dist_min, dist_mahal_val], 
            color='black', linestyle='dotted', 
            label=f'$\\Delta_{{mahal}} = {dist_mahal_val:.3f}$', zorder=100) # マハラノビス距離
    ax.scatter(xs=mu_d[0], ys=mu_d[1], zs=dist_min, 
               color='black', s=50, label='$\\mu = '+mu_str+'$') # 中心
    ax.scatter(xs=x_d[0], ys=x_d[1], zs=dist_min, 
               fc='none', ec='C0', s=50) # 変数点(座標平面上)
    ax.scatter(xs=x_d[0], ys=x_d[1], zs=dist_mahal_val, 
               color='C0', s=50, label=x_label, zorder=100) # 変数点(曲面上)
    ax.contour(X1, X2, dist_vec.reshape(grid_shape), offset=dist_min, 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # 等高線(座標平面上)
    ax.contour(X1, X2, dist_vec.reshape(grid_shape), 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels, 
               linestyles='dotted') # 等高線(曲面上)
    #ax.plot_surface(X1, X2, dist_vec.reshape(grid_shape), 
    #                cmap='viridis', vmin=dist_min, vmax=dist_max, alpha=0.5) # 曲面
    surf = ax.plot_surface(X1, X2, dist_vec.reshape(grid_shape), 
                           facecolors=cm.viridis(dist_vec.reshape(grid_shape)/dist_max), shade=False, 
                           linewidth=0.25) # 曲面(フレーム)
    surf.set_facecolor((0, 0, 0, 0)) # くり抜き
    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')
    ax.set_zlabel(def_label, labelpad=15.0)
    ax.set_title(param_label, loc='left')
    ax.legend(loc='upper right')
    ax.set_xlim(xmin=x1_min, xmax=x1_max)
    ax.set_ylim(ymin=x2_min, ymax=x2_max)
    ax.set_zlim(zmin=dist_min, zmax=dist_max)
    ax.set_box_aspect([1.0, x2_size/x1_size, 1.0]) # 高さ(横サイズに対する比)を指定
    #ax.view_init(elev=30, azim=-60) # 表示アングル

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/2d_shape/2d_variable.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%


