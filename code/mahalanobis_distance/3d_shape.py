# マハラノビス距離の作図
# 3次元変数の場合
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
mu_d = np.array([0.0, 0.0, 0.0])

# 分散共分散行列を指定
sigma_dd = np.array(
    [[16.0, -1.5, 0.0], 
     [-1.5, 9.0, 0.9], 
     [0.0, 0.9, 4.0]]
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
print(grid_shape)
print(grid_size)

# %%

## 距離の計算

# 高さごとに処理
dist_arr = np.tile(np.nan, reps=(x2.size, grid_size)) # 受け皿を初期化
for i in range(x2.size):

    # 座標を作成
    X = np.stack([X0.flatten(), X1.flatten(), x2[i].repeat(grid_size)], axis=1)
    
    # マハラノビス距離を計算
    dist_vec = np.array(
        [np.sqrt((x_d - mu_d).T @ inv_sigma_dd @ (x_d - mu_d)) for x_d in X]
    )

    # 距離を格納
    dist_arr[i] = dist_vec.copy()

# %%

## アニメーションの作成

# グラデーションの範囲を設定
dist_min = 0.0
dist_max = np.ceil(dist_arr.max())

# 等高線の位置を指定
dist_levels = np.linspace(start=dist_min, stop=dist_max, num=7) # 線の数を指定
print(dist_levels)

# 軸サイズを設定
x0_size = x0_max - x0_min
x1_size = x1_max - x1_min
x2_size = x2_max - x2_min

# ラベル用の文字列を作成
def_label = '$\\Delta = \\sqrt{(x - \\mu)^{T} \\Sigma^{-1} (x - \\mu)}$'
mu_str    = '(' + ', '.join(str(val.round(2)) for val in mu_d) + ')'
sigma_str = '(' + ', '.join('(' + ', '.join(str(val.round(2)) for val in vec) + ')' for vec in sigma_dd) + ')'
param_label  = '$D = 3$\n'
param_label += '$\\mu = ' + mu_str + '$\n'
param_label += '$\\Sigma = ' + sigma_str + '$'

# %%

# フレーム数を指定
frame_num = 180

# 表示角度を作成
h_vals = np.linspace(start=0.0, stop=360.0, num=frame_num+1)[:frame_num]

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(9, 8), dpi=100, facecolor='white', constrained_layout=True, 
                       subplot_kw={'projection': '3d'})
fig.suptitle("Mahalanobis' Distance", fontsize=20)
cs = ax.contour(X0, X1, np.linspace(dist_min, dist_max, num=grid_size).reshape(grid_shape), 
                cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels) # カラーバー表示用のダミー
fig.colorbar(cs, ax=ax, shrink=0.8, label=def_label)

# 作図処理を定義
def update(frame_i):
    
    # 前フレームのグラフを初期化
    ax.cla()
    
    # 3Dマハラノビス距離を作図
    for i in range(x2.size):
        ax.contour(X0, X1, dist_arr[i].reshape(grid_shape), offset=x2[i], 
                   cmap='viridis', vmin=dist_min, vmax=dist_max, levels=dist_levels, alpha=0.5, 
                   linewidths=1) # 等高線
    ax.set_xlim(xmin=x0_min, xmax=x0_max)
    ax.set_ylim(ymin=x1_min, ymax=x1_max)
    ax.set_zlim(zmin=x2_min, zmax=x2_max)
    ax.set_xlabel('$x_0$')
    ax.set_ylabel('$x_1$')
    ax.set_zlabel('$x_2$')
    ax.set_title(param_label, loc='left')
    ax.set_box_aspect([1.0, x1_size/x0_size, x2_size/x0_size])
    ax.view_init(elev=30, azim=h_vals[frame_i]) # 表示アングル

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/mahalanobis_distance/3d_shape/3d_angle.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%


