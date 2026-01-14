
# 1次元ガウス分布 ---------------------------------------------------------------

# パラメータの可視化


# %%

# ライブラリを読込
import numpy as np
from scipy.stats import norm
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from matplotlib import cm
from matplotlib.colors import Normalize


# %%

# パラメータの影響 ---------------------------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num = 101

# フレームごとのパラメータを指定
mu_vals     = np.linspace(start=-10.0, stop=1.0, num=frame_num)
sigma_vals  = np.linspace(start=1.0, stop=1.0, num=frame_num)
lambda_vals = np.linspace(start=1.0, stop=1.0, num=frame_num)

#sigma_vals  = np.linspace(start=0.0, stop=10.0, num=frame_num+1)[1:]
print(mu_vals[:5])

# パラメータを計算
sigma_vals  = 1.0/np.sqrt(lambda_vals)
#lambda_vals = 1.0/sigma_vals**2
print(sigma_vals[:5])
print(lambda_vals[:5])


# %%

### 変数の設定 -----

# x軸の範囲を設定
k = 2.0
u = 5.0
x_min = np.min(mu_vals - k*sigma_vals) # 基準値を指定
x_max = np.max(mu_vals + k*sigma_vals) # 基準値を指定
x_min = np.floor(x_min /u)*u # u単位で切り下げ
x_max = np.ceil(x_max /u)*u  # u単位で切り上げ
print('x size:', x_min, x_max)

# x軸の値を作成
x_vec = np.linspace(start=x_min, stop=x_max, num=1001)
print(x_vec[:5])


# %%

### 分布の計算 -----

# ガウス分布の確率密度を計算
dens_lt = [
    norm.pdf(x=x_vec, loc=mu_vals[i], scale=sigma_vals[i]) for i in range(frame_num)
]


# %%

### 分布の作図 -----

# 確率密度軸の範囲を設定
u = 0.5
dens_max = np.max(dens_lt)
dens_max = np.ceil(dens_max /u)*u # u単位で切り上げ
dens_max = 1.0
print('p(x) size:', dens_max)


# %%

#### パラメータと形状の関係 -----

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Gaussian distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()
    
    # 値を取得
    mu    = mu_vals[i]     # 平均パラメータ
    sigma = sigma_vals[i]  # 標準偏差パラメータ
    lmd   = lambda_vals[i] # 精度パラメータ
    dens_vec = dens_lt[i]  # 確率密度

    # ラベル用の文字列を作成
    param_lbl = f'$\\mu = {mu:.2f}, \\sigma = {sigma:.2f}, \\lambda = {lmd:.2f}$'
    
    # ガウス分布を描画
    ax.plot(
        x_vec, dens_vec, 
        color='#00A968', linewidth=1.0
    ) # 確率密度
    ax.set_xlabel('$x$')
    ax.set_ylabel('density')
    ax.set_title(param_lbl, loc='left')
    ax.grid()
    ax.set_ylim(ymin=0.0, ymax=dens_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/gaussian/parameter/parameter_mu.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

#### パラメータと統計量の関係 -----

# ラベルの表示用の余白を設定
y_margin = 0.05
y_min = -dens_max * y_margin
y_max = dens_max * (1.0+y_margin)

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Gaussian distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()
    
    # 値を取得
    mu    = mu_vals[i]     # 平均パラメータ
    sigma = sigma_vals[i]  # 標準偏差パラメータ
    lmd   = lambda_vals[i] # 精度パラメータ
    dens_vec = dens_lt[i]  # 確率密度

    # 統計量を計算
    mean_x = mu    # 期待値
    sd_x   = sigma # 標準偏差
    mode_x = mu    # 最頻値

    # 標準偏差の範囲を計算
    x_pm1sgm_vec = np.linspace(
        start=mean_x-sd_x if mean_x-sd_x > x_min else x_min, 
        stop =mean_x+sd_x if mean_x+sd_x < x_max else x_max, 
        num=1001
    ) # 確率変数
    dens_pm1sgm_vec = norm.pdf(x=x_pm1sgm_vec, loc=mu, scale=sigma) # 確率密度

    # ラベル用の文字列を作成
    param_lbl = f'$\\mu = {mu:.2f}, \\sigma = {sigma:.2f}, \\lambda = {lmd:.2f}$'

    # ガウス分布を描画
    ax.fill_between(
        x=x_pm1sgm_vec, y1=0.0, y2=dens_pm1sgm_vec, 
        color='gray', alpha=0.5, 
        zorder=9
    ) # 標準偏差の範囲
    ax.plot(
        x_vec, dens_vec, 
        color='#00A968', linewidth=1.0, 
        zorder=10
    ) # 確率密度
    ax.axvline(
        x=mean_x, 
        color='black', linewidth=1.0, linestyle='--', 
        label=f'$E[x] = \\mu = {mean_x:.2f}$', 
        zorder=11
    ) # 期待値の位置
    for i, coord_x in enumerate([mean_x-sd_x, mean_x+sd_x]):
        ax.axvline(
            x=coord_x, 
            color='black', linewidth=1.0, linestyle=':', 
            label=f'$\\sqrt{{V[x]}} = \\sigma = {sd_x:.2f}$' if i == 0 else None, 
            zorder=11
        ) # 標準偏差の位置
    ax.axvline(
        x=mode_x, 
        color='black', linewidth=0.0, linestyle='-.', 
        label=f'$mode[x] = \\mu = {mode_x:.2f}$', 
        zorder=11
    ) # 最頻値の位置
    ax.hlines(
        y=0.0, xmin=mean_x-sd_x, xmax=mean_x+sd_x, 
        color='black', linewidth=1.0, 
        zorder=11
    ) # 標準偏差の範囲
    for label_x, label_str in zip([mu-sigma, mu, mu+sigma], ['$-\sigma$', '$\mu$', '$+\sigma$']):
        ax.text(
            x=label_x, y=0.0, 
            s=label_str, ha='center', va='top', 
            size=10, 
            zorder=12
        ) # 統計量のラベル
    ax.set_xlabel('$x$')
    ax.set_ylabel('density')
    ax.set_title(param_lbl, loc='left')
    ax.legend(title='statistics', prop={'size': 8}, loc='upper left')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=x_min, xmax=x_max) # (垂線がはみ出すときの対策用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (ラベルの表示用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/gaussian/parameter/stats_lambda.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

# 平均パラメータの影響 -----------------------------------------------------------

### パラメータの設定 -----

# 固定するパラメータを指定
sigma_vals = np.array(
    [0.1, 0.25, 0.5, 1.0, 1.5, 3.6, 5.0, 7.1, 10.0]
)
print(sigma_vals)

# グラフ数を設定
param_num = len(sigma_vals)

# フレーム数を指定
frame_num = 20

# フレームごとのパラメータを指定
mu_vals = np.linspace(start=-10.0, stop=10.0, num=frame_num+1)[1:]
print(mu_vals[:5])


# %%

### 変数の設定 -----

# x軸の範囲を設定
k = 0.0
u = 5.0
x_min = np.min(mu_vals[:, None] - k*sigma_vals[None, :]) # 偏差の最小値
x_max = np.max(mu_vals[:, None] + k*sigma_vals[None, :]) # 偏差の最大値
x_min = np.floor(x_min /u)*u # u単位で切り下げ
x_max = np.ceil(x_max /u)*u  # u単位で切り上げ
print('x size:', x_min, x_max)

# x軸の値を作成
x_vec = np.linspace(start=x_min, stop=x_max, num=1001)
print(x_vec[:5])


# %%

### 分布の計算 -----

# ガウス分布の確率密度を計算
dens_lt = [
    [norm.pdf(x=x_vec, loc=mu_vals[i], scale=sigma_vals[j]) for j in range(param_num)] for i in range(frame_num)
]


# %%

### 分布の作図 -----

# 確率密度軸の範囲を設定
k = 0.5
u = 0.5
dens_max = np.max(dens_lt)
dens_max *= k # 定数倍
dens_max = np.ceil(dens_max /u)*u # u単位で切り上げ
print('p(x) size:', dens_max)

# パラメータ軸の範囲を設定
u = 1.0
mu_max = mu_vals.max()
mu_max = np.ceil(mu_max /u)*u # u単位で切り上げ
print('μ size:', mu_max)


# %%

#### 標準偏差パラメータの比較 -----

# サブプロット数を設定
col_num = 3 # 列数を指定
row_num = np.ceil(param_num / col_num).astype(np.int32) # 行数を計算

# カラーマップを設定
cmap = cm.viridis # カラーマップを指定
color_norm = Normalize(vmin=0.0, vmax=mu_max) # ノーマライザを指定
sm = cm.ScalarMappable(norm=color_norm, cmap=cmap) # 配色用のオブジェクトを作成
sm.set_array([]) # 範囲を初期化

# 図を初期化
fig, axes = plt.subplots(
    nrows=row_num, ncols=col_num, constrained_layout=True, 
    figsize=(12, 8), dpi=100, facecolor='white'
)
fig.suptitle('Gaussian distribution', fontsize=20)
fig.supxlabel('$x$')
fig.supylabel('density')
cbar = fig.colorbar(mappable=sm, ax=axes.ravel(), orientation='vertical') # カラーバー
cbar.set_label('$\mu$')

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes.flatten()]

    for j in range(param_num):

         # サブプロットを取得
        r = j // col_num # 行番号
        c = j % col_num  # 列番号
        ax = axes[r, c]
    
        # 値を取得
        sigma = sigma_vals[j] # 標準偏差パラメータ

        # ラベル用の文字列を作成
        param_lbl = f'$\\mu, \\sigma = {sigma:.2f}$'
        
        # ガンマ分布を描画
        for tmp_i in range(i+1):

            # 値を取得
            mu = mu_vals[tmp_i] # 平均パラメータ
            dens_vec = dens_lt[tmp_i][j] # 確率密度

            ax.plot(
                x_vec, dens_vec, 
                color=cmap(color_norm(mu)), linewidth=1.0
            ) # 確率密度
        ax.set_title(param_lbl, loc='left')
        ax.grid()
        ax.set_xlim(xmin=x_min, xmax=x_max) # 描画範囲を固定
        ax.set_ylim(ymin=0.0, ymax=dens_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/gaussian/parameter/mean_parameter.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

# 標準偏差パラメータの影響 -----------------------------------------------------------

### パラメータの設定 -----

# 固定するパラメータを指定
mu_vals = np.array(
    [-5.0, -2.5, -1.0, -0.5, 0.0, 0.5, 1.25, 3.1, 5.0]
)
print(mu_vals)

# グラフ数を設定
param_num = len(mu_vals)

# フレーム数を指定
frame_num = 20

# フレームごとのパラメータを指定
sigma_vals = np.linspace(start=0.0, stop=10.0, num=frame_num+1)[1:]
print(sigma_vals[:5])


# %%

### 変数の設定 -----

# x軸の範囲を設定
k = 0.01
u = 5.0
x_min = np.min(mu_vals[:, None] - k*sigma_vals[None, :]) # 偏差の最小値
x_max = np.max(mu_vals[:, None] + k*sigma_vals[None, :]) # 偏差の最大値
x_min = np.floor(x_min /u)*u # u単位で切り下げ
x_max = np.ceil(x_max /u)*u  # u単位で切り上げ
print('x size:', x_min, x_max)

# x軸の値を作成
x_vec = np.linspace(start=x_min, stop=x_max, num=1001)
print(x_vec[:5])


# %%

### 分布の計算 -----

# ガウス分布の確率密度を計算
dens_lt = [
    [norm.pdf(x=x_vec, loc=mu_vals[j], scale=sigma_vals[i]) for j in range(param_num)] for i in range(frame_num)
]


# %%

### 分布の作図 -----

# 確率密度軸の範囲を設定
k = 0.5
u = 0.5
dens_max = np.max(dens_lt)
dens_max *= k # 定数倍
dens_max = np.ceil(dens_max /u)*u # u単位で切り上げ
print('p(x) size:', dens_max)

# パラメータ軸の範囲を設定
u = 1.0
sigma_max = sigma_vals.max()
sigma_max = np.ceil(sigma_max /u)*u # u単位で切り上げ
print('σ size:', sigma_max)


# %%

#### 平均パラメータの比較 -----

# サブプロット数を設定
col_num = 3 # 列数を指定
row_num = np.ceil(param_num / col_num).astype(np.int32) # 行数を計算

# カラーマップを設定
cmap = cm.viridis # カラーマップを指定
color_norm = Normalize(vmin=0.0, vmax=sigma_max) # ノーマライザを指定
sm = cm.ScalarMappable(norm=color_norm, cmap=cmap) # 配色用のオブジェクトを作成
sm.set_array([]) # 範囲を初期化

# 図を初期化
fig, axes = plt.subplots(
    nrows=row_num, ncols=col_num, constrained_layout=True, 
    figsize=(12, 8), dpi=100, facecolor='white'
)
fig.suptitle('Gaussian distribution', fontsize=20)
fig.supxlabel('$x$')
fig.supylabel('density')
cbar = fig.colorbar(mappable=sm, ax=axes.ravel(), orientation='vertical') # カラーバー
cbar.set_label('$\sigma$')

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes.flatten()]

    for j in range(param_num):

         # サブプロットを取得
        r = j // col_num # 行番号
        c = j % col_num  # 列番号
        ax = axes[r, c]
    
        # 値を取得
        mu = mu_vals[j] # 平均パラメータ

        # ラベル用の文字列を作成
        param_lbl = f'$\\mu = {mu:.2f}, \\sigma$'
        
        # ガンマ分布を描画
        for tmp_i in range(i+1):

            # 値を取得
            sigma = sigma_vals[tmp_i] # 標準偏差パラメータ
            dens_vec = dens_lt[tmp_i][j] # 確率密度

            ax.plot(
                x_vec, dens_vec, 
                color=cmap(color_norm(sigma)), linewidth=1.0
            ) # 確率密度
        ax.set_title(param_lbl, loc='left')
        ax.grid()
        ax.set_xlim(xmin=x_min, xmax=x_max) # 描画範囲を固定
        ax.set_ylim(ymin=0.0, ymax=dens_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/gaussian/parameter/sd_parameter.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%


