# %% ディリクレ分布

# 利用ライブラリ
import numpy as np
from scipy.stats import dirichlet
from scipy.special import gamma, loggamma, digamma
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %% # 確率密度の計算

# パラメータを指定
beta_v = np.array([0.4, 0.2, 0.3])
beta_v = np.array([4.0, 2.0, 3.0])

# 確率変数の値を指定
phi_v = np.array([0.5, 0.0, 0.5])
phi_v = np.array([0.5, 0.3, 0.2])


# 定義式により確率密度を計算
C = gamma(np.sum(beta_v)) / np.prod(gamma(beta_v))
dens = C * np.prod(phi_v**(beta_v - 1))
print(dens)

# 対数をとった定義式により確率密度を計算
log_C = loggamma(sum(beta_v)) - np.sum(loggamma(beta_v))
log_dens = log_C + np.sum((beta_v - 1) * np.log(phi_v))
dens = np.exp(log_dens)
print(dens)
print(log_dens)

# 関数により確率密度を計算
dens = dirichlet.pdf(x=phi_v, alpha=beta_v)
print(dens)

# 対数をとった関数により確率密度を計算
log_dens = dirichlet.logpdf(x=phi_v, alpha=beta_v)
dens = np.exp(log_dens)
print(dens)
print(log_dens)


# %% # 統計量の計算

# パラメータを指定
beta_v = np.array([4.0, 2.0, 3.0])

# 次元数を設定
V = len(beta_v)


# 期待値を計算
E_phi_v = beta_v / np.sum(beta_v)
print(E_phi_v)

# 分散を計算
V_phi_v = beta_v * (np.sum(beta_v) - beta_v)
V_phi_v /= np.sum(beta_v)**2 * (np.sum(beta_v) + 1.0)
print(V_phi_v)

# インデックスを指定:(i ≠ j)
i = 0
j = 1

# 共分散を計算
Cov_phi_ij = -beta_v[i] * beta_v[j]
Cov_phi_ij /= np.sum(beta_v)**2 * (np.sum(beta_v) + 1.0)
print(Cov_phi_ij)

# 共分散を計算
Cov_phi_vv = -np.dot(beta_v.reshape(V, 1), beta_v.reshape(1, V))
Cov_phi_vv /= np.sum(beta_v)**2 * (np.sum(beta_v) + 1.0)
Cov_phi_vv[np.arange(V), np.arange(V)] = np.nan
print(Cov_phi_vv)

# 最頻値を計算:(β_v > 1)
mode_phi_v = (beta_v - 1.0) / (np.sum(beta_v) - V)
print(mode_phi_v)

# 対数の期待値を計算
E_log_phi_v = digamma(beta_v) - digamma(np.sum(beta_v))
print(E_log_phi_v)


# %% # 座標の作成

# 軸目盛の位置を指定
axis_vals = np.arange(start=0.0, stop=1.1, step=0.1)

# 軸線用の値を作成
axis_x = np.array([0.5, 0.0, 1.0])
axis_y = np.array([0.5*np.sqrt(3.0), 0.0, 0.0])
axis_u = np.array([-0.5, 1.0, -0.5])
axis_v = np.array([-0.5*np.sqrt(3.0), 0.0, 0.5*np.sqrt(3.0)])

# グリッド線用の値を作成
grid_x = np.hstack([
    0.5 * axis_vals, 
    axis_vals, 
    0.5 * axis_vals + 0.5
])
grid_y = np.hstack([
    0.5 * axis_vals * np.sqrt(3.0), 
    np.zeros_like(axis_vals), 
    0.5 * (1.0 - axis_vals) * np.sqrt(3.0)
])
grid_u = np.hstack([
    0.5 * axis_vals, 
    0.5 * (1.0 - axis_vals), 
    -axis_vals
])
grid_v = np.hstack([
    -0.5 * axis_vals * np.sqrt(3.0), 
    0.5 * (1.0 - axis_vals) * np.sqrt(3.0), 
    np.zeros_like(axis_vals)
])


# %% # グラフの作成：散布図によるヒートマップ

## ・作図用と計算用の点の作成

# Φがとり得る値を作成
phi_vals = np.linspace(start=0.0, stop=1.0, num=51)

# 格子点を作成
phi_0_grid, phi_1_grid, phi_2_grid = np.meshgrid(phi_vals, phi_vals, phi_vals)

# Φがとり得る点を作成
phi_points = np.stack([phi_0_grid.flatten(), phi_1_grid.flatten(), phi_2_grid.flatten()], axis=1) # 配列に格納
phi_points = phi_points[1:, :] # (0, 0, 0)の行を除去
phi_points /= np.sum(phi_points, axis=1, keepdims=True) # 正規化
phi_points = np.unique(phi_points, axis=0) # 重複を除去
print(phi_points.shape)


# 三角座標に変換
y_0_vals = phi_points[:, 1] + 0.5 * phi_points[:, 2]
y_1_vals = 0.5 * phi_points[:, 2] * np.sqrt(3.0)

# %%

## ・パラメータの設定

# パラメータを指定
beta_v = np.array([0.4, 0.2, 0.3])
beta_v = np.array([4.0, 2.0, 3.0])
beta_v = np.array([0.1, 0.1, 0.1])

# %%

## ・パラメータが全ての1以上の場合

# ディリクレ分布の確率密度を計算
dens_vals = np.array(
    [dirichlet.pdf(x=phi_v, alpha=beta_v) for phi_v in phi_points]
)

#%%

## ・パラメータが全ての1未満の場合

# ディリクレ分布の確率密度を計算
dens_vals = np.array(
    [dirichlet.pdf(x=phi_v, alpha=beta_v) if all(phi_v != 0.0) else np.nan for phi_v in phi_points]
)

# %%

## ・作図

# ディリクレ分布の散布図によるヒートマップを作成
plt.figure(figsize=(12, 10), facecolor='white') # 図の設定
sct = plt.scatter(x=y_0_vals, y=y_1_vals, c=dens_vals, alpha=0.8) # 確率密度のヒートマップ
plt.quiver(grid_x, grid_y, grid_u, grid_v, 
           scale_units='xy', scale=1, units='dots', width=0.1, headwidth=0.1, 
           fc='none', ec='gray', linewidth=1.5, linestyle=':') # 三角座標のグリッド線
plt.quiver(axis_x, axis_y, axis_u, axis_v, 
           scale_units='xy', scale=1, units='dots', width=1.5, headwidth=1.5, 
           fc='gray', linestyle='-') # 三角座標の枠線
for val in axis_vals:
    plt.text(x=0.5*val, y=0.5*val*np.sqrt(3.0), s=str(np.round(1.0-val, 1))+' '*2, 
             ha='right', va='bottom', rotation=-60) # 三角座標のx軸目盛
    plt.text(x=val, y=0.0, s=str(np.round(val, 1))+' '*10, 
             ha='center', va='center', rotation=60) # 三角座標のy軸目盛
    plt.text(x=0.5*val+0.5, y=0.5*(1.0-val)*np.sqrt(3.0), s=' '*3+str(np.round(1.0-val, 1)), 
             ha='left', va='center') # 三角座標のz軸目盛
plt.text(x=0.25, y=0.25*np.sqrt(3.0), s='$\phi_1$'+' '*5, 
         ha='right', va='center', size=25) # 三角座標のx軸ラベル
plt.text(x=0.5, y=0.0, s='\n'+'$\phi_2$', 
         ha='center', va='top', size=25) # 三角座標のy軸ラベル
plt.text(x=0.75, y=0.25*np.sqrt(3.0), s=' '*4+'$\phi_3$', 
         ha='left', va='center', size=25) # 三角図のz軸ラベル
plt.xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
plt.yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
plt.grid() # 2次元座標のグリッド線
plt.axis('equal') # アスペクト比
plt.suptitle(t='Dirichlet Distribution', fontsize=20) # 全体のタイトル
plt.title(label='$\\beta=('+', '.join([str(beta) for beta in beta_v])+')$', loc='left') # パラメータラベル
plt.colorbar(sct, label='density') # カラーバー
plt.show() # 描画


# %% # グラフの作成：等高線図ほか

## ・作図と計算用の点の作成

# 2次元座標の値を作成
y_0_vals = np.linspace(start=0.0, stop=1.0, num=201)
y_1_vals = np.linspace(start=0.0, stop=0.5*np.sqrt(3.0), num=201)

# 2次元座標の格子点を作成
y_0_grid, y_1_grid = np.meshgrid(y_0_vals, y_1_vals)

# 格子点の形状を保存
y_shape = y_0_grid.shape

# 3次元座標の値に変換
phi_1_vals = y_0_grid.flatten() - y_1_grid.flatten() / np.sqrt(3.0)
phi_2_vals = 2.0 * y_1_grid.flatten() / np.sqrt(3.0)

# 範囲外の点を欠損値に置換
phi_1_vals = np.where(
    (phi_1_vals >= 0.0) & (phi_1_vals <= 1.0), 
    phi_1_vals, 
    np.nan
)
phi_2_vals = np.where(
    (phi_2_vals >= 0.0) & (phi_2_vals <= 1.0), 
    phi_2_vals, 
    np.nan
)

# 3次元座標の値に変換
phi_0_vals = 1.0 - phi_1_vals - phi_2_vals

# 範囲外の点を欠損値に置換
phi_0_vals = np.where(
    (phi_0_vals >= 0.0) & (phi_0_vals <= 1.0), 
    phi_0_vals, 
    np.nan
)

# 計算用の3次元座標の点を作成
phi_points = np.stack([phi_0_vals, phi_1_vals, phi_2_vals], axis=1)

# %%

## ・パラメータの設定

# パラメータを指定
beta_v = np.array([0.4, 0.2, 0.3])
beta_v = np.array([4.0, 2.0, 3.0])
#beta_v = np.array([0.1, 0.5, 0.9])

# %%

## ・パラメータが全ての1以上の場合

# ディリクレ分布の確率密度を計算
dens_vals = np.array(
    [dirichlet.pdf(x=phi_v, alpha=beta_v) for phi_v in phi_points]
)

#%%

## ・パラメータが全ての1未満の場合

# ディリクレ分布の確率密度を計算
dens_vals = np.array(
    [dirichlet.pdf(x=phi_v, alpha=beta_v) if all(phi_v != 0.0) else np.nan for phi_v in phi_points]
)

# %%

## ・作図

# ディリクレ分布のヒートマップを作成
plt.figure(figsize=(12, 10), facecolor='white') # 図の設定
plt.quiver(grid_x, grid_y, grid_u, grid_v, 
           scale_units='xy', scale=1, units='dots', width=0.1, headwidth=0.1, 
           fc='none', ec='gray', linewidth=1.5, linestyle=':') # 三角座標のグリッド線
plt.quiver(axis_x, axis_y, axis_u, axis_v, 
           scale_units='xy', scale=1, units='dots', width=1.5, headwidth=1.5, 
           fc='black', linestyle='-') # 三角座標の枠線
for val in axis_vals:
    plt.text(x=0.5*val, y=0.5*val*np.sqrt(3.0), s=str(np.round(1.0-val, 1))+' '*2, 
             ha='right', va='bottom', rotation=-60) # 三角座標のx軸目盛
    plt.text(x=val, y=0.0, s=str(np.round(val, 1))+' '*10, 
             ha='center', va='center', rotation=60) # 三角座標のy軸目盛
    plt.text(x=0.5*val+0.5, y=0.5*(1.0-val)*np.sqrt(3.0), s=' '*3+str(np.round(1.0-val, 1)), 
             ha='left', va='center') # 三角座標のz軸目盛
plt.text(x=0.25, y=0.25*np.sqrt(3.0), s='$\phi_1$'+' '*5, 
         ha='right', va='center', size=25) # 三角座標のx軸ラベル
plt.text(x=0.5, y=0.0, s='\n'+'$\phi_2$', 
         ha='center', va='top', size=25) # 三角座標のy軸ラベル
plt.text(x=0.75, y=0.25*np.sqrt(3.0), s=' '*4+'$\phi_3$', 
         ha='left', va='center', size=25) # 三角図のz軸ラベル
pcl = plt.pcolor(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
                 alpha = 0.8) # 確率密度のヒートマップ
plt.xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
plt.yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
plt.grid() # 2次元座標のグリッド線
plt.axis('equal') # アスペクト比
plt.colorbar(pcl, label='density') # カラーバー
plt.suptitle(t='Dirichlet Distribution', fontsize=20) # タイトル
plt.title(label='$\\beta=('+', '.join([str(val) for val in beta_v])+')$', loc='left') # パラメータラベル
plt.show() # 描画


# %%

# ディリクレ分布の等高線図を作成
plt.figure(figsize=(12, 10), facecolor='white') # 図の設定
plt.quiver(grid_x, grid_y, grid_u, grid_v, 
           scale_units='xy', scale=1, units='dots', width=0.1, headwidth=0.1, 
           fc='none', ec='gray', linewidth=1.5, linestyle=':') # 三角座標のグリッド線
plt.quiver(axis_x, axis_y, axis_u, axis_v, 
           scale_units='xy', scale=1, units='dots', width=1.5, headwidth=1.5, 
           fc='black', linestyle='-') # 三角座標の枠線
for val in axis_vals:
    plt.text(x=0.5*val, y=0.5*val*np.sqrt(3.0), s=str(np.round(1.0-val, 1))+' '*2, 
             ha='right', va='bottom', rotation=-60) # 三角座標のx軸目盛
    plt.text(x=val, y=0.0, s=str(np.round(val, 1))+' '*10, 
             ha='center', va='center', rotation=60) # 三角座標のy軸目盛
    plt.text(x=0.5*val+0.5, y=0.5*(1.0-val)*np.sqrt(3.0), s=' '*3+str(np.round(1.0-val, 1)), 
             ha='left', va='center') # 三角座標のz軸目盛
plt.text(x=0.25, y=0.25*np.sqrt(3.0), s='$\phi_1$'+' '*5, 
         ha='right', va='center', size=25) # 三角座標のx軸ラベル
plt.text(x=0.5, y=0.0, s='\n'+'$\phi_2$', 
         ha='center', va='top', size=25) # 三角座標のy軸ラベル
plt.text(x=0.75, y=0.25*np.sqrt(3.0), s=' '*4+'$\phi_3$', 
         ha='left', va='center', size=25) # 三角図のz軸ラベル
cnf = plt.contourf(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
                   alpha = 0.8) # 確率密度の等高線
plt.xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
plt.yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
plt.grid() # 2次元座標のグリッド線
plt.axis('equal') # アスペクト比
plt.colorbar(cnf, label='density') # カラーバー
plt.suptitle(t='Dirichlet Distribution', fontsize=20) # タイトル
plt.title(label='$\\beta=('+', '.join([str(val) for val in beta_v])+')$', loc='left') # パラメータラベル
plt.show() # 描画

# %%

# ディリクレ分布の曲面図を作成
fig = plt.figure(figsize=(12, 10), facecolor='white') # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.quiver(grid_x, grid_y, np.zeros_like(grid_x), grid_u, grid_v, np.zeros_like(grid_x), 
          arrow_length_ratio=0.0, ec='gray', linewidth=1.5, linestyle=':') # 三角座標のグリッド線
ax.quiver(axis_x, axis_y, np.zeros_like(axis_x), axis_u, axis_v, np.zeros_like(axis_x), 
          arrow_length_ratio=0.0, ec='black', linestyle='-') # 三角座標の枠線
for val in axis_vals:
    ax.text(x=0.5*val-0.05, y=0.5*val*np.sqrt(3.0), z=0.0, s=str(np.round(1.0-val, 1)), 
            ha='center', va='center') # 三角座標のx軸目盛
    ax.text(x=val, y=0.0-0.05, z=0.0, s=str(np.round(val, 1)), 
            ha='center', va='center') # 三角座標のy軸目盛
    ax.text(x=0.5*val+0.5+0.05, y=0.5*(1.0-val)*np.sqrt(3.0), z=0.0, s=str(np.round(1.0-val, 1)), 
            ha='center', va='center') # 三角座標のz軸目盛
ax.text(x=0.25-0.1, y=0.25*np.sqrt(3.0), z=0.0, s='$\phi_1$', 
        ha='right', va='center', size=25) # 三角座標のx軸ラベル
ax.text(x=0.5, y=0.0-0.1, z=0.0-0.1, s='$\phi_2$', 
        ha='center', va='top', size=25) # 三角座標のy軸ラベル
ax.text(x=0.75+0.1, y=0.25*np.sqrt(3.0), z=0.0, s='$\phi_3$', 
        ha='left', va='center', size=25) # 三角図のz軸ラベル
ax.contour(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
           offset=0.0) # 確率密度の等高線
ax.plot_surface(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
                cmap='viridis', alpha=0.8) # 確率密度の曲面
ax.set_xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
ax.set_yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
ax.set_zlabel(zlabel='density') # z軸ラベル
ax.set_box_aspect(aspect=(1, 1, 1)) # アスペクト比
fig.suptitle(t='Dirichlet Distribution', fontsize=20) # タイトル
ax.set_title(label='$\\beta=('+', '.join([str(beta) for beta in beta_v])+')$', loc='left') # パラメータラベル
#ax.view_init(elev=90, azim=-90) # 表示角度
plt.show() # 描画


# %% # パラメータと分布の形状の関係：アニメーションによる可視化

## ・パラメータの設定

# パラメータとして利用する値を指定
beta_1_vals = np.arange(start=1.0, stop=10.1, step=0.1).round(decimals=1)
beta_2_vals = np.arange(start=1.0, stop=10.1, step=0.1).round(decimals=1)
beta_3_vals = np.arange(start=1.0, stop=10.1, step=0.1).round(decimals=1)

# 固定するパラメータを指定
beta_1 = 4.0
beta_2 = 2.0
beta_3 = 3.0

# フレーム数を設定
frame_num = len(beta_1_vals)
print(frame_num)

# z軸の最小値と最大値を設定
dens_min = 0.0
beta_max_v = np.array([beta_1_vals.max(), beta_2_vals.max(), beta_3_vals.max()])
dens_max = np.ceil(
    dirichlet.pdf(x=(beta_max_v-1.0)/(np.sum(beta_max_v)-3.0), alpha=beta_max_v)
)

# 等高線を引く値を設定
dens_levels = np.linspace(dens_min, dens_max, num=11)
print(dens_levels)

# %%

## ・等高線図のアニメーション

# ディリクレ分布の等高線図のアニメーションを作成
fig = plt.figure(figsize=(12, 10), facecolor='white') # 図の設定
fig.suptitle(t='Dirichlet Distribution', fontsize=20) # タイトル
tmp = plt.contourf(y_0_grid, y_1_grid, np.zeros(y_shape), 
                   vmin=dens_min, vmax=dens_max, levels=dens_levels, alpha = 0.8) # カラーバー用のダミー
fig.colorbar(tmp, label='density') # カラーバー

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のパラメータを取得
    beta_1 = beta_1_vals[i]
    beta_2 = beta_2_vals[i]
    beta_3 = beta_3_vals[i]
    
    # パラメータを設定
    beta_v = np.array([beta_1, beta_2, beta_3])
    
    # ディリクレ分布の確率密度を計算
    dens_vals = np.array(
        [dirichlet.pdf(x=phi_v, alpha=beta_v) if all(phi_v != np.nan) else np.nan for phi_v in phi_points]
    )
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\beta=('+', '.join([str(beta) for beta in beta_v])+')$'
    
    # 三角座標上の等高線図を作成
    plt.quiver(grid_x, grid_y, grid_u, grid_v, 
               scale_units='xy', scale=1, units='dots', width=0.1, headwidth=0.1, 
               fc='none', ec='gray', linewidth=1.5, linestyle=':') # 三角座標のグリッド線
    plt.quiver(axis_x, axis_y, axis_u, axis_v, 
               scale_units='xy', scale=1, units='dots', width=1.5, headwidth=1.5, 
               fc='black', linestyle='-') # 三角座標の枠線
    for val in axis_vals:
        plt.text(x=0.5*val, y=0.5*val*np.sqrt(3.0), s=str(np.round(1.0-val, 1))+' '*2, 
                 ha='right', va='bottom', rotation=-60) # 三角座標のx軸目盛
        plt.text(x=val, y=0.0, s=str(np.round(val, 1))+' '*10, 
                 ha='center', va='center', rotation=60) # 三角座標のy軸目盛
        plt.text(x=0.5*val+0.5, y=0.5*(1.0-val)*np.sqrt(3.0), s=' '*3+str(np.round(1.0-val, 1)), 
                 ha='left', va='center') # 三角座標のz軸目盛
    plt.text(x=0.25, y=0.25*np.sqrt(3.0), s='$\phi_1$'+' '*5, 
             ha='right', va='center', size=25) # 三角座標のx軸ラベル
    plt.text(x=0.5, y=0.0, s='\n'+'$\phi_2$', 
             ha='center', va='top', size=25) # 三角座標のy軸ラベル
    plt.text(x=0.75, y=0.25*np.sqrt(3.0), s=' '*4+'$\phi_3$', 
             ha='left', va='center', size=25) # 三角図のz軸ラベル
    plt.contourf(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
                 vmin=dens_min, vmax=dens_max, levels=dens_levels, alpha = 0.8) # 確率密度の等高線
    plt.xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
    plt.yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
    plt.grid() # 2次元座標のグリッド線
    plt.axis('equal') # アスペクト比
    plt.title(label=param_text, loc='left') # パラメータラベル

# gif画像を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# gif画像を保存
ani.save('../../figure/Python/dirichlet_cnf.gif')

# %%

## ・曲面図のアニメーション

# ディリクレ分布の曲面図のアニメーションを作成
fig = plt.figure(figsize=(10, 10), facecolor='white') # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle(t='Dirichlet Distribution', fontsize=20) # タイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()

    # i番目のパラメータを取得
    beta_1 = beta_1_vals[i]
    beta_2 = beta_2_vals[i]
    beta_3 = beta_3_vals[i]
    
    # パラメータを設定
    beta_v = np.array([beta_1, beta_2, beta_3])
    
    # ディリクレ分布の確率密度を計算
    dens_vals = np.array(
        [dirichlet.pdf(x=phi_v, alpha=beta_v) if all(phi_v != np.nan) else np.nan for phi_v in phi_points]
    )
    
    # パラメータラベル用の文字列を作成
    param_text = '$\\beta=('+', '.join([str(beta) for beta in beta_v])+')$'
    
    # 三角座標上の曲面図を作成
    ax.quiver(grid_x, grid_y, np.zeros_like(grid_x), grid_u, grid_v, np.zeros_like(grid_x), 
              arrow_length_ratio=0.0, ec='gray', linewidth=1.5, linestyle=':') # 三角座標のグリッド線
    ax.quiver(axis_x, axis_y, np.zeros_like(axis_x), axis_u, axis_v, np.zeros_like(axis_x), 
              arrow_length_ratio=0.0, ec='black', linestyle='-') # 三角座標の枠線
    for val in axis_vals:
        ax.text(x=0.5*val-0.05, y=0.5*val*np.sqrt(3.0), z=0.0, s=str(np.round(1.0-val, 1)), 
                ha='center', va='center') # 三角座標のx軸目盛
        ax.text(x=val, y=0.0-0.05, z=0.0, s=str(np.round(val, 1)), 
                ha='center', va='center') # 三角座標のy軸目盛
        ax.text(x=0.5*val+0.5+0.05, y=0.5*(1.0-val)*np.sqrt(3.0), z=0.0, s=str(np.round(1.0-val, 1)), 
                ha='center', va='center') # 三角座標のz軸目盛
    ax.text(x=0.25-0.1, y=0.25*np.sqrt(3.0), z=0.0, s='$\phi_1$', 
            ha='right', va='center', size=25) # 三角座標のx軸ラベル
    ax.text(x=0.5, y=0.0-0.1, z=0.0-0.1, s='$\phi_2$', 
            ha='center', va='top', size=25) # 三角座標のy軸ラベル
    ax.text(x=0.75+0.1, y=0.25*np.sqrt(3.0), z=0.0, s='$\phi_3$', 
            ha='left', va='center', size=25) # 三角図のz軸ラベル
    ax.contour(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
               vmin=dens_min, vmax=dens_max, levels=dens_levels, offset=0.0) # 確率密度の等高線
    ax.plot_surface(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
                    cmap='viridis', alpha=0.8) # 確率密度の曲面
    ax.set_xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
    ax.set_yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
    ax.set_zlabel(zlabel='density') # z軸ラベル
    ax.set_zlim(bottom=dens_min, top=dens_max) # z軸の表示範囲
    ax.set_box_aspect(aspect=(1, 1, 1)) # アスペクト比
    ax.set_title(label=param_text, loc='left') # パラメータラベル
    #ax.view_init(elev=90, azim=-90) # 表示角度

# gif画像を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# gif画像を保存
ani.save('../../figure/Python/dirichlet_srf.gif')


# %%

