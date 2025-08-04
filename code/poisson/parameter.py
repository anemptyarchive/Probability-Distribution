
# ポアソン分布 ------------------------------------------------------------------

# パラメータの影響の可視化


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import poisson, norm
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# パラメータの影響 ---------------------------------------------------------------

### パラメータの設定 -----

# フレームごとのパラメータを指定
lambda_vals = np.arange(start=0.0, stop=10.1, step=0.1)

# フレーム数を取得
frame_num = len(lambda_vals)


# x軸の範囲を設定
x_min = -5.0 # (基本的に固定)
u = 10.0
x_max = np.max(lambda_vals)
x_max *= 1.5 # 倍率を指定
x_max = np.ceil(x_max /u)*u # u単位で切り上げ

# x軸の値を作成
x_vec = np.arange(start=x_min, stop=x_max+1, step=1)


# %%

### 分布の計算 -----

# ポアソン分布を計算
prob_lt = [poisson.pmf(k=x_vec, mu=lambda_vals[i]) for i in range(frame_num)]


# %%

### パラメータと形状の関係 -----

# 確率軸の範囲を設定
u = 0.05
prob_max = np.max(prob_lt)
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Poisson distribution', fontsize=20)

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を取得
    lmd = lambda_vals[i] # パラメータ
    prob_vec = prob_lt[i] # 確率
    
    # ポアソン分布を描画
    ax.bar(x=x_vec, height=prob_vec, color='#00A968') # 確率
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('probability')
    ax.set_title(f'$\\lambda = {lmd:.1f}$', loc='left')
    ax.set_ylim(ymin=0.0, ymax=prob_max) # 描画範囲を固定

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/poisson/parameter/paramter.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
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
fig.suptitle('Poisson distribution', fontsize=20)

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を取得
    lmd = lambda_vals[i] # パラメータ
    prob_vec = prob_lt[i] # 確率

    # 統計量を計算
    mean_x = lmd           # 期待値
    sd_x   = np.sqrt(lmd)  # 標準偏差
    mode_x = np.floor(lmd) # 最頻値
    
    # ポアソン分布を描画
    ax.vlines(
        x=mode_x, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='dashdot', 
        label=f'$mode[x] = \\lfloor \\lambda \\rfloor = {mode_x:.2f}$'
    ) # 最頻値の位置
    ax.vlines(
        x=mean_x, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='dashed', 
        label=f'$E[x] = \\lambda = {mean_x:.2f}$'
    ) # 期待値の位置
    ax.vlines(
        x=[mean_x-sd_x, mean_x+sd_x], ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='dotted', 
        label=f'$\\sqrt{{V[x]}} = \\sqrt{{\\lambda}} = {sd_x:.2f}$'
    ) # 標準偏差の位置
    ax.hlines(
        y=0.0, xmin=mean_x-sd_x, xmax=mean_x+sd_x, 
        color='black', linewidth=1.0
    ) # 標準偏差の範囲
    ax.text(
        x=mean_x, y=0.0, 
        s='$E[x] \pm \\sqrt{{V[x]}}$', ha='center', va='top', 
        size=8
    ) # 統計量のラベル
    ax.bar(x=x_vec, height=prob_vec, color='#00A968') # 確率
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('probability')
    ax.set_title(f'$\\lambda = {lmd:.1f}$', loc='left')
    ax.legend(title='statistics', prop={'size': 8}, loc='upper left')
    ax.set_ylim(ymin=y_min, ymax=y_max) # (垂線との対応用)

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/poisson/parameter/stats.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
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
fig.suptitle('Poisson distribution', fontsize=20)

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を取得
    lmd = lambda_vals[i] # パラメータ
    pois_prob_vec = prob_lt[i] # 確率

    # 統計量を計算
    mu    = lmd          # 期待値
    sigma = np.sqrt(lmd) # 標準偏差

    # モーメントを計算
    skew = 1.0 / np.sqrt(lmd) # 歪度
    kurt = 1.0 / lmd          # 尖度
    moment_str  = f'skewness: {skew:.3f} \n'
    moment_str += f'kurtosis:    {kurt:.3f}' # (スペースによる位置調整)

    # ガウス分布を計算
    norm_x_vec = np.linspace(start=x_min, stop=x_max, num=1001)
    norm_prob_vec = norm.pdf(x=norm_x_vec, loc=mu, scale=sigma)
    
    # ポアソン分布を描画
    ax.bar(
        x=x_vec, height=pois_prob_vec, 
        color='#00A968', alpha=0.5
    ) # ポアソン分布の確率
    ax.vlines(
        x=mu, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='dashed'
    ) # 期待値の位置
    ax.hlines(
        y=0.0, xmin=mu-sigma, xmax=mu+sigma, 
        color='black', linewidth=1.0
    ) # 標準偏差の範囲
    for label_x, label_str in zip([mu-sigma, mu, mu+sigma], ['$- \sigma$', '$\mu$', '$+ \sigma$']):
        ax.text(
            x=label_x, y=0.0, 
            s=label_str, ha='center', va='top', 
            size=8
        ) # 統計量のラベル
    ax.scatter(
        x=x_vec, y=pois_prob_vec, 
        color='#00A968', s=30
    ) # ポアソン分布の確率
    ax.plot(
        x_vec, pois_prob_vec, 
        color='#00A968', linewidth=1.0, 
        label='poisson'
    ) # ポアソン分布の確率
    ax.plot(
        norm_x_vec, norm_prob_vec, 
        color='red', linewidth=1.0, linestyle='dashed', 
        label='gaussian'
    ) # ガウス分布の確率密度
    ax.text(
        x=x_min, y=prob_max, 
        s=moment_str, ha='left', va='top', 
        bbox=dict(facecolor='none', edgecolor='black', linewidth=0.5), 
        size = 8
    ) # モーメントのラベル
    #ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('probability, density')
    ax.set_title(f'$\\lambda = {lmd:.1f}, \mu = {mu:.1f}, \sigma = {sigma:.2f}$', loc='left')
    ax.legend(title='distribution', prop={'size': 8}, loc='upper right')
    ax.set_ylim(ymin=y_min, ymax=y_max) # (垂線との対応用)

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/poisson/parameter/moment.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%


