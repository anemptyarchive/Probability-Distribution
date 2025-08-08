
# 二項分布 ----------------------------------------------------------------------

# パラメータの可視化


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import binom, norm
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# パラメータの影響：成功確率 ------------------------------------------------------

### パラメータの設定 -----

# 試行回数を指定
M = 9

# フレームごとのパラメータを指定
phi_vals = np.arange(start=0.0, stop=1.01, step=0.01)

# フレーム数を設定
frame_num = len(phi_vals)


# %%

### 変数の設定 -----

# x軸の範囲を設定
x_min = 0.0
x_max = M

# x軸の値を作成
x_vec = np.arange(start=x_min, stop=x_max+1, step=1)


# %%

### 分布の計算 -----

# 二項分布の確率を計算
prob_lt = [
    binom.pmf(k=x_vec, n=M, p=phi_vals[i]) for i in range(frame_num)
]


# %%

### パラメータと形状の関係 -----

# 確率軸の範囲を設定
u = 0.05
prob_max = np.max(prob_lt)
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Binomial distribution', fontsize=20)

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()
    
    # 値を取得
    phi = phi_vals[i]     # パラメータ
    prob_vec = prob_lt[i] # 確率
    
    # 二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968'
    ) # 確率
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('probability')
    ax.set_title(f'$M = {M}, \\phi = {phi:.2f}$', loc='left')
    ax.set_ylim(ymin=0.0, ymax=prob_max) # 描画範囲を固定

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/binomial/parameter/parameter_phi.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

### パラメータと統計量の関係 -----

# 確率軸の範囲を設定
u = 0.05
prob_max = np.max(prob_lt)
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ

# 余白を追加
y_margin = 0.05
y_min = -prob_max * y_margin
y_max = prob_max * (1.0+y_margin)

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Binomial distribution', fontsize=20)

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を取得
    phi = phi_vals[i]     # パラメータ
    prob_vec = prob_lt[i] # 確率

    # 統計量を計算
    mean_x = M * phi                        # 期待値
    sd_x   = np.sqrt(M * phi * (1.0 - phi)) # 標準偏差
    mode_x = np.floor((M + 1) * phi)        # 最頻値
    
    # 二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968'
    ) # 確率
    ax.vlines(
        x=mean_x, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='--', 
        label=f'$E[x] = M \\phi = {mean_x:.2f}$'
    ) # 期待値の位置
    ax.vlines(
        x=[mean_x-sd_x, mean_x+sd_x], ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles=':', 
        label=f'$\\sqrt{{V[x]}} = \\sqrt{{M \\phi (1-\\phi)}} = {sd_x:.2f}$'
    ) # 標準偏差の位置
    ax.hlines(
        y=0.0, xmin=mean_x-sd_x, xmax=mean_x+sd_x, 
        color='black', linewidth=1.0
    ) # 標準偏差の範囲
    ax.vlines(
        x=mode_x, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='-.', 
        label=f'$mode[x] = \\lfloor (M+1) \\phi \\rfloor = {mode_x:.2f}$'
    ) # 最頻値の位置
    ax.text(
        x=mean_x, y=0.0, 
        s='$E[x] \pm \\sqrt{V[x]}$', ha='center', va='top', 
        size=8
    ) # 統計量のラベル
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('probability')
    ax.set_title(f'$M = {M}, \\phi = {phi:.2f}$', loc='left')
    ax.legend(title='statistics', prop={'size': 8}, loc='upper left')
    ax.set_xlim(xmin=x_min-0.5, xmax=x_max+0.5) # (垂線がはみ出す回避用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (垂線との対応用)

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/binomial/parameter/stats_phi.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

### パラメータとモーメントの関係 -----

# 確率軸の範囲を設定
u = 0.05
prob_max = np.max(prob_lt)
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ

# 余白を追加
y_margin = 0.05
y_min = -prob_max * y_margin
y_max = prob_max * (1.0+y_margin)

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Binomial distribution', fontsize=20)

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を取得
    phi = phi_vals[i]          # パラメータ
    pois_prob_vec = prob_lt[i] # 確率

    # 統計量を計算
    mu    = M * phi                        # 期待値
    sigma = np.sqrt(M * phi * (1.0 - phi)) # 標準偏差

    # モーメントを計算
    if phi > 0.0:
        skew = (1.0 - 2.0 * phi) / np.sqrt(M * phi * (1.0 - phi))        # 歪度
        kurt = (1.0 - 6.0 * phi * (1.0 - phi)) / (M * phi * (1.0 - phi)) # 尖度
    else: # (0除算の回避用)
        skew = np.inf
        kurt = np.inf

    # ガウス分布の確率密度を計算
    norm_x_vec    = np.linspace(start=x_min, stop=x_max, num=1001)
    norm_dens_vec = norm.pdf(x=norm_x_vec, loc=mu, scale=sigma)
    
    # ラベル用の文字列を作成
    moment_str  = f'skewness: {skew:.3f} \n'
    moment_str += f'kurtosis:    {kurt:.3f}' # (スペースによる位置調整)

    # 二項分布分布を描画
    ax.bar(
        x=x_vec, height=pois_prob_vec, 
        color='#00A968', alpha=0.5
    ) # 二項分布分布の確率
    ax.scatter(
        x=x_vec, y=pois_prob_vec, 
        color='#00A968', s=30
    ) # 二項分布分布の確率
    ax.vlines(
        x=mu, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='dashed'
    ) # 期待値の位置
    ax.hlines(
        y=0.0, xmin=mu-sigma, xmax=mu+sigma, 
        color='black', linewidth=1.0
    ) # 標準偏差の範囲
    for label_x, label_str in zip([mu-sigma, mu, mu+sigma], ['$-\sigma$', '$\mu$', '$+\sigma$']):
        ax.text(
            x=label_x, y=0.0, 
            s=label_str, ha='center', va='top', 
            size=10
        ) # 統計量のラベル
    ax.plot(
        x_vec, pois_prob_vec, 
        color='#00A968', linewidth=1.0, 
        label='poisson'
    ) # 二項分布分布の確率
    ax.plot(
        norm_x_vec, norm_dens_vec, 
        color='red', linewidth=1.0, linestyle='dashed', 
        label='gaussian'
    ) # ガウス分布の確率密度
    ax.text(
        x=x_min, y=prob_max, 
        s=moment_str, ha='left', va='top', 
        bbox=dict(facecolor='white', alpha=0.8, edgecolor='black', linewidth=0.5), 
        size = 10
    ) # モーメントのラベル
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('probability, density')
    ax.set_title(f'$M = {M}, \\phi = {phi:.2f}, \mu = {mu:.2f}, \sigma = {sigma:.2f}$', loc='left')
    ax.legend(title='distribution', prop={'size': 8}, loc='upper right')
    ax.set_xlim(xmin=x_min-0.5, xmax=x_max+0.5) # (垂線がはみ出す回避用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (垂線との対応用)

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/binomial/parameter/moment_phi.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

# パラメータの影響：試行回数 ------------------------------------------------------

### パラメータの設定 -----

# 試行回数の最大値を指定
M_max = 100

# パラメータを指定
phi = 0.4

# フレーム数を設定
frame_num = M_max + 1


# %%

### パラメータと形状の関係 -----

# 確率軸の範囲を設定
prob_max = 1.0

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Binomial distribution', fontsize=20)

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # パラメータを設定
    M = i
    
    # x軸の値を作成
    x_vec = np.arange(start=0, stop=M+1, step=1)
    
    # 二項分布の確率を計算
    prob_vec = binom.pmf(k=x_vec, n=M, p=phi)
    
    # 二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968'
    ) # 確率
    #ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('probability')
    ax.set_title(f'$M = {M}, \\phi = {phi}$', loc='left')
    ax.set_ylim(ymin=0.0, ymax=prob_max) # 描画範囲を固定

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/binomial/parameter/parameter_M.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

### パラメータと統計量の関係 -----

# 確率軸の範囲を設定
prob_max = 1.0

# 余白を追加
y_margin = 0.05
y_min = -prob_max * y_margin
y_max = prob_max * (1.0+y_margin)

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Binomial distribution', fontsize=20)

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # パラメータを設定
    M = i
    
    # x軸の値を作成
    x_vec = np.arange(start=0, stop=M+1, step=1)
    
    # 二項分布の確率を計算
    prob_vec = binom.pmf(k=x_vec, n=M, p=phi)

    # 統計量を計算
    mean_x = M * phi                        # 期待値
    sd_x   = np.sqrt(M * phi * (1.0 - phi)) # 標準偏差
    mode_x = np.floor((M + 1) * phi)        # 最頻値
    
    # 二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968'
    ) # 確率
    ax.vlines(
        x=mean_x, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='--', 
        label=f'$E[x] = M \\phi = {mean_x:.2f}$'
    ) # 期待値の位置
    ax.vlines(
        x=[mean_x-sd_x, mean_x+sd_x], ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles=':', 
        label=f'$\\sqrt{{V[x]}} = \\sqrt{{M \\phi (1-\\phi)}} = {sd_x:.2f}$'
    ) # 標準偏差の位置
    ax.hlines(
        y=0.0, xmin=mean_x-sd_x, xmax=mean_x+sd_x, 
        color='black', linewidth=1.0
    ) # 標準偏差の範囲
    ax.vlines(
        x=mode_x, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='-.', 
        label=f'$mode[x] = \\lfloor (M+1) \\phi \\rfloor = {mode_x:.2f}$'
    ) # 最頻値の位置
    ax.text(
        x=mean_x, y=0.0, 
        s='$E[x] \pm \\sqrt{V[x]}$', ha='center', va='top', 
        size=8
    ) # 統計量のラベル
    #ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('probability')
    ax.set_title(f'$M = {M}, \\phi = {phi}$', loc='left')
    ax.legend(title='statistics', prop={'size': 8}, loc='upper left')
    ax.set_xlim(xmin=-0.5, xmax=M+0.5) # (垂線がはみ出す回避用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (垂線との対応用)

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/binomial/parameter/stats_M.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

### パラメータとモーメントの関係 -----

# 確率軸の範囲を設定
prob_max = 1.0

# y軸の余白を追加
y_margin = 0.05
y_min = -prob_max * y_margin
y_max = prob_max * (1.0+y_margin)

# x軸の余白を指定
x_margin = 5

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Binomial distribution', fontsize=20)

# ラベルの位置を設定
momemt_lbl = fig.text(
    x=0.02, y=0.98, 
    s='', transform=ax.transAxes, ha='left', va='top', 
    bbox=dict(facecolor='white', alpha=0.8, edgecolor='black', linewidth=0.5), 
    size = 10
) # モーメントのラベル

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # パラメータを設定
    M = i
    
    # x軸の範囲を設定
    x_min = -x_margin
    x_max = M + x_margin

    # x軸の値を作成
    bimom_x_vec = np.arange(start=x_min, stop=x_max+1, step=1)
    norm_x_vec  = np.linspace(start=x_min, stop=x_max, num=1001)
    
    # 二項分布の確率を計算
    binom_prob_vec = binom.pmf(k=bimom_x_vec, n=M, p=phi)

    # 統計量を計算
    mu    = M * phi                        # 期待値
    sigma = np.sqrt(M * phi * (1.0 - phi)) # 標準偏差

    # モーメントを計算
    if M > 0:
        skew = (1.0 - 2.0 * phi) / np.sqrt(M * phi * (1.0 - phi))        # 歪度
        kurt = (1.0 - 6.0 * phi * (1.0 - phi)) / (M * phi * (1.0 - phi)) # 尖度
    else: # (0除算の回避用)
        skew = np.inf
        kurt = np.inf
    
    # ガウス分布の確率密度を計算
    norm_dens_vec = norm.pdf(x=norm_x_vec, loc=mu, scale=sigma)
    
    # ラベルの文字列を設定
    moment_str  = f'skewness: {skew:.3f} \n'
    moment_str += f'kurtosis:    {kurt:.3f}' # (スペースによる位置調整)
    momemt_lbl.set_text(moment_str)

    # 二項分布を描画
    ax.bar(
        x=bimom_x_vec, height=binom_prob_vec, 
        color='#00A968', alpha=0.5
    ) # 二項分布の確率
    ax.scatter(
        x=bimom_x_vec, y=binom_prob_vec, 
        color='#00A968', s=30
    ) # 二項分布の確率
    ax.vlines(
        x=mu, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='dashed'
    ) # 期待値の位置
    ax.hlines(
        y=0.0, xmin=mu-sigma, xmax=mu+sigma, 
        color='black', linewidth=1.0
    ) # 標準偏差の範囲
    for label_x, label_str in zip([mu-sigma, mu, mu+sigma], ['$-\sigma$', '$\mu$', '$+\sigma$']):
        ax.text(
            x=label_x, y=0.0, 
            s=label_str, ha='center', va='top', 
            size=10
        ) # 統計量のラベル
    ax.plot(
        bimom_x_vec, binom_prob_vec, 
        color='#00A968', linewidth=1.0, 
        label='poisson'
    ) # 二項分布の確率
    ax.plot(
        norm_x_vec, norm_dens_vec, 
        color='red', linewidth=1.0, linestyle='dashed', 
        label='gaussian'
    ) # ガウス分布の確率密度
    #ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('probability, density')
    ax.set_title(f'$M = {M}, \\phi = {phi}, \mu = {mu:.1f}, \sigma = {sigma:.2f}$', loc='left')
    ax.legend(title='distribution', prop={'size': 8}, loc='upper right')
    ax.set_xlim(xmin=x_min-0.5, xmax=x_max+0.5) # (垂線がはみ出す回避用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (垂線との対応用)

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/binomial/parameter/moment_M.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%


