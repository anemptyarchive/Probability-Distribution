# マハラノビス距離の作図
# 2次元変数の場合
# 形状の確認

# %%

# ライブラリを読込
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

### 形状の確認 ------------------------------------------------------------------

## パラメータの設定

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
x0_min = mu_d[0] - sgm_num * np.sqrt(sigma_dd[0, 0])
x0_max = mu_d[0] + sgm_num * np.sqrt(sigma_dd[0, 0])
x1_min = mu_d[1] - sgm_num * np.sqrt(sigma_dd[1, 1])
x1_max = mu_d[1] + sgm_num * np.sqrt(sigma_dd[1, 1])
print(x0_min.round(2), x0_max.round(2))
print(x1_min.round(2), x1_max.round(2))

# 各軸の値を作成
x0 = np.linspace(start=x0_min, stop=x0_max, num=50)
x1 = np.linspace(start=x1_min, stop=x1_max, num=50)

# 格子点を作成
X0, X1 = np.meshgrid(x0, x1)

# 格子点の形状を設定
grid_shape = X0.shape
print(grid_shape)

# 座標を作成
X = np.stack([X0.flatten(), X1.flatten()], axis=1)
print(X.shape)

# %%

## 距離の計算

# マハラノビス距離を計算
dist_vec = np.array(
    [np.sqrt((x_d - mu_d).T @ inv_sigma_dd @ (x_d - mu_d)) for x_d in X]
)

# %%

## アニメーションの作成

# フレーム数を指定
frame_num = 180

# 表示角度を作成
h_vals = np.linspace(start=0.0, stop=360.0, num=frame_num+1)[:frame_num]

# グラデーションの範囲を設定
dist_min = 0.0
dist_max = np.ceil(dist_vec.max())

# 等高線の位置を指定
dist_levels = np.linspace(start=dist_min, stop=dist_max, num=7) # 線の数を指定
print(dist_levels)

# %%

# ラベル用の文字列を作成
def_label = '$\\Delta = \\sqrt{(x - \\mu)^{T} \\Sigma^{-1} (x - \\mu)}$'
mu_str    = '(' + ', '.join(str(val.round(2)) for val in mu_d) + ')'
sigma_str = '(' + ', '.join('(' + ', '.join(str(val.round(2)) for val in vec) + ')' for vec in sigma_dd) + ')'
param_label  = '$D = 2$\n'
param_label += '$\\mu = ' + mu_str + '$\n'
param_label += '$\\Sigma = ' + sigma_str + '$'

# 軸サイズを設定
x0_size = x0_max - x0_min
x1_size = x1_max - x1_min

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(8, 8), dpi=100, facecolor='white', constrained_layout=True, 
                       subplot_kw={'projection': '3d'})
fig.suptitle("Mahalanobis' Distance", fontsize=20)

# 作図処理を定義
def update(frame_i):
    
    # 前フレームのグラフを初期化
    ax.cla()
    
    # 2Dマハラノビス距離を描画
    ax.contour(X0, X1, dist_vec.reshape(grid_shape), offset=0.0, 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # 等高線(座標平面上)
    ax.contour(X0, X1, dist_vec.reshape(grid_shape), 
               cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels, 
               linestyles='dashed') # 等高線(曲面上)
    ax.plot_surface(X0, X1, dist_vec.reshape(grid_shape), 
               cmap='viridis', vmin=dist_min, vmax=dist_max, alpha=0.8) # 曲面
    ax.set_xlim(xmin=x0_min, xmax=x0_max)
    ax.set_ylim(ymin=x1_min, ymax=x1_max)
    ax.set_xlabel('$x_0$')
    ax.set_ylabel('$x_1$')
    ax.set_zlabel(def_label)
    ax.set_title(param_label, loc='left')
    ax.set_box_aspect([1.0, x1_size/x0_size, 0.6]) # 高さ(横サイズに対する比)を指定
    ax.view_init(elev=30, azim=h_vals[frame_i]) # 表示アングル

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/2d_shape/2d_angle.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%


