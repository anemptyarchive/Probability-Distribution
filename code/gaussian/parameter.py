
# 1次元ガウス分布 ---------------------------------------------------------------

# パラメータの可視化

# %%

# 利用するライブラリ
import numpy as np
from scipy.stats import norm
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# パラメータの影響：平均・標準偏差 ------------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num = 101

# フレームごとのパラメータを指定
mu_vals    = np.linspace(start=-5.0, stop=5.0, num=frame_num)
sigma_vals = np.linspace(start=1.0, stop=1.0, num=frame_num+1)[1:]


# %%

### 変数の設定 -----

# x軸の範囲を設定
u = 5.0
x_size = np.max(sigma_vals)
x_size *= 2.0 # 倍率を指定
x_min = np.min(mu_vals) - x_size
x_max = np.max(mu_vals) + x_size
x_min = np.floor(x_min /u)*u # u単位で切り下げ
x_max = np.ceil(x_max /u)*u # u単位で切り上げ

# x軸の値を作成
x_vec = np.linspace(start=x_min, stop=x_max, num=1001)


# %%

### 分布の計算 -----

# ガウス分布の確率密度を計算
dens_lt = [
    norm.pdf(x=x_vec, loc=mu_vals[i], scale=sigma_vals[i]) for i in range(frame_num)
]


# %%

### パラメータと形状の関係 -----

# 確率密度軸の範囲を設定
u = 0.05
dens_max = np.max(dens_lt)
dens_max = np.ceil(dens_max /u)*u # u単位で切り上げ

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Gaussian distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()
    
    # 値を取得
    mu    = mu_vals[i]    # 平均パラメータ
    sigma = sigma_vals[i] # 標準偏差パラメータ
    dens_vec = dens_lt[i] # 確率密度
    
    # ガウス分布を描画
    ax.plot(
        x_vec, dens_vec, 
        color='#00A968', linewidth=1.0
    ) # 確率密度
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('density')
    ax.set_title(f'$\mu = {mu:.2f}, \\sigma = {sigma:.2f}$', loc='left')
    ax.set_ylim(ymin=0.0, ymax=dens_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/gaussian/parameter/parameter.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

### パラメータと統計量の関係 -----

# 確率密度軸の範囲を設定
u = 0.05
dens_max = np.max(dens_lt)
dens_max = np.ceil(dens_max /u)*u # u単位で切り上げ

# 余白を追加
y_margin = 0.05
y_min = -dens_max * y_margin
y_max = dens_max * (1.0+y_margin)

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Gaussian distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()
    
    # 値を取得
    mu    = mu_vals[i]    # 平均パラメータ
    sigma = sigma_vals[i] # 標準偏差パラメータ
    dens_vec = dens_lt[i] # 確率密度

    # 統計量を計算
    mean_x = mu    # 期待値
    sd_x   = sigma # 標準偏差
    mode_x = mu    # 最頻値

    # 標準偏差の範囲を計算
    tmp_x_vec    = np.linspace(start=mu-sigma, stop=mu+sigma, num=1001) # 確率変数
    tmp_dens_vec = norm.pdf(x=tmp_x_vec, loc=mu, scale=sigma)           # 確率密度

    # ガウス分布を描画
    ax.fill_between(
        x=tmp_x_vec, y1=0.0, y2=tmp_dens_vec, 
        color='gray', alpha=0.5
    ) # 標準偏差の範囲
    ax.plot(
        x_vec, dens_vec, 
        color='#00A968', linewidth=1.0
    ) # 確率密度
    ax.vlines(
        x=mean_x, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='--', 
        label=f'$E[x] = \mu = {mean_x:.2f}$'
    ) # 期待値の位置
    ax.vlines(
        x=[mean_x-sd_x, mean_x+sd_x], ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles=':', 
        label=f'$\\sqrt{{V[x]}} = \\sigma = {sd_x:.2f}$'
    ) # 標準偏差の位置
    ax.hlines(
        y=0.0, xmin=mean_x-sd_x, xmax=mean_x+sd_x, 
        color='black', linewidth=1.0
    ) # 標準偏差の範囲
    ax.vlines(
        x=mode_x, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='-.', 
        label=f'$mode[x] = \mu = {mode_x:.2f}$'
    ) # 最頻値の位置
    ax.text(
        x=mean_x, y=0.0, 
        s='$E[x] \pm \\sqrt{V[x]}$', ha='center', va='top', 
        size=8
    ) # 統計量のラベル
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('density')
    ax.set_title(f'$\mu = {mu:.2f}, \sigma = {sigma:.2f}$', loc='left')
    ax.legend(title='statistics', prop={'size': 8}, loc='upper left')
    ax.set_xlim(xmin=x_min-0.5, xmax=x_max+0.5) # (垂線がはみ出す回避用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (垂線との対応用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/gaussian/parameter/stats.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

# パラメータの影響：精度 ---------------------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num = 101

# フレームごとのパラメータを指定
mu_vals     = np.linspace(start=0.0, stop=0.0, num=frame_num)
lambda_vals = np.linspace(start=0.0, stop=10.0, num=frame_num+1)[1:]


# %%

### 変数の設定 -----

# x軸の範囲を設定
u = 5.0
x_size = np.max(1.0/np.sqrt(lambda_vals))
x_size *= 2.0 # 倍率を指定
x_min = np.min(mu_vals) - x_size
x_max = np.max(mu_vals) + x_size
x_min = np.floor(x_min /u)*u # u単位で切り下げ
x_max = np.ceil(x_max /u)*u # u単位で切り上げ

# x軸の値を作成
x_vec = np.linspace(start=x_min, stop=x_max, num=1001)


# %%

### 分布の計算 -----

# ガウス分布の確率密度を計算
dens_lt = [
    norm.pdf(x=x_vec, loc=mu_vals[i], scale=1.0/np.sqrt(lambda_vals[i])) for i in range(frame_num)
]


# %%

### パラメータと形状の関係 -----

# 確率密度軸の範囲を設定
u = 0.05
dens_max = np.max(dens_lt)
dens_max = np.ceil(dens_max /u)*u # u単位で切り上げ

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Gaussian distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()
    
    # 値を取得
    mu  = mu_vals[i]         # 平均パラメータ
    lmd = lambda_vals[i]     # 精度パラメータ
    sigma = 1.0/np.sqrt(lmd) # 標準偏差パラメータ
    dens_vec = dens_lt[i]    # 確率密度
    
    # ガウス分布を描画
    ax.plot(
        x_vec, dens_vec, 
        color='#00A968', linewidth=1.0
    ) # 確率密度
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('density')
    ax.set_title(f'$\mu = {mu:.2f}, \\sigma = {sigma:.2f}, \lambda = {lmd:.2f}$', loc='left')
    ax.set_ylim(ymin=0.0, ymax=dens_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/gaussian/parameter/parameter_lambda.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

### パラメータと統計量の関係 -----

# 確率密度軸の範囲を設定
u = 0.05
dens_max = np.max(dens_lt)
dens_max = np.ceil(dens_max /u)*u # u単位で切り上げ

# 余白を追加
y_margin = 0.05
y_min = -dens_max * y_margin
y_max = dens_max * (1.0+y_margin)

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Gaussian distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()
    
    # 値を取得
    mu  = mu_vals[i]         # 平均パラメータ
    lmd = lambda_vals[i]     # 精度パラメータ
    sigma = 1.0/np.sqrt(lmd) # 標準偏差パラメータ
    dens_vec = dens_lt[i]    # 確率密度

    # 統計量を計算
    mean_x = mu    # 期待値
    sd_x   = sigma # 標準偏差
    mode_x = mu    # 最頻値

    # 標準偏差の範囲を計算
    tmp_x_vec    = np.linspace(start=mu-sigma, stop=mu+sigma, num=1001) # 確率変数
    tmp_dens_vec = norm.pdf(x=tmp_x_vec, loc=mu, scale=sigma)           # 確率密度

    # ガウス分布を描画
    ax.fill_between(
        x=tmp_x_vec, y1=0.0, y2=tmp_dens_vec, 
        color='gray', alpha=0.5
    ) # 標準偏差の範囲
    ax.plot(
        x_vec, dens_vec, 
        color='#00A968', linewidth=1.0
    ) # 確率密度
    ax.vlines(
        x=mean_x, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='--', 
        label=f'$E[x] = \mu = {mean_x:.2f}$'
    ) # 期待値の位置
    ax.vlines(
        x=[mean_x-sd_x, mean_x+sd_x], ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles=':', 
        label=f'$\\sqrt{{V[x]}} = \\sigma = {sd_x:.2f}$'
    ) # 標準偏差の位置
    ax.hlines(
        y=0.0, xmin=mean_x-sd_x, xmax=mean_x+sd_x, 
        color='black', linewidth=1.0
    ) # 標準偏差の範囲
    ax.vlines(
        x=mode_x, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='-.', 
        label=f'$mode[x] = \mu = {mode_x:.2f}$'
    ) # 最頻値の位置
    ax.text(
        x=mean_x, y=0.0, 
        s='$E[x] \pm \\sqrt{V[x]}$', ha='center', va='top', 
        size=8
    ) # 統計量のラベル
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('density')
    ax.set_title(f'$\mu = {mu:.2f}, \sigma = {sigma:.2f}, \lambda = {lmd:.2f}$', loc='left')
    ax.legend(title='statistics', prop={'size': 8}, loc='upper left')
    ax.set_xlim(xmin=x_min-0.5, xmax=x_max+0.5) # (垂線がはみ出す回避用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (垂線との対応用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/gaussian/parameter/stats_lambda.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%


