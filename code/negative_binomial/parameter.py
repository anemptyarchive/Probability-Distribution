
# 負の二項分布 ------------------------------------------------------------------

# パラメータの可視化


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import nbinom, norm
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# パラメータの影響：成功確率 ------------------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num = 101

# フレームごとのパラメータを指定
r_vals   = np.linspace(start=5, stop=5, num=frame_num, dtype='int')
phi_vals = np.linspace(start=0.0, stop=1.0, num=frame_num)


# %%

### 変数の設定 -----

# x軸の範囲を設定
x_min = 0.0
u = 5.0
tmp_vals = r_vals * (1.0-phi_vals) / phi_vals
x_max = np.max(tmp_vals[np.isfinite(tmp_vals)]) # 基準値を指定
x_max *= 0.1 # 倍率を指定
x_max = np.ceil(x_max /u)*u # u単位で切り上げ
print(x_max)

# x軸の値を作成
x_vec = np.arange(start=x_min, stop=x_max+1, step=1)


# %%

### 分布の計算 -----

# 負の二項分布の確率を計算
prob_lt = [
    nbinom.pmf(k=x_vec, n=r_vals[i], p=phi_vals[i]) for i in range(frame_num)
]


# %%

### パラメータと形状の関係 -----

# 確率軸の範囲を設定
u = 0.05
tmp_arr = np.array(prob_lt)
prob_max = np.max(tmp_arr[np.isfinite(tmp_arr)])
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Negative Binomial distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を取得
    r   = r_vals[i]   # 成功回数
    phi = phi_vals[i] # 成功確率
    prob_vec = prob_lt[i] # 確率
    
    # 負の二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968'
    ) # 確率
    #ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('$x$')
    ax.set_ylabel('probability')
    ax.set_title(f'$r = {r}, \\phi = {phi:.2f}$', loc='left')
    ax.set_ylim(ymin=0.0, ymax=prob_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/negative_binomial/parameter/parameter.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

### パラメータと統計量の関係 -----

# 確率軸の範囲を設定
u = 0.05
tmp_arr = np.array(prob_lt)
prob_max = np.max(tmp_arr[np.isfinite(tmp_arr)])
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ
print(prob_max)

# 余白を追加
x_margin = 0.05
y_margin = 0.05
y_min = -prob_max * y_margin
y_max = prob_max * (1.0+y_margin)

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Negative Binomial distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を取得
    r   = r_vals[i]   # 成功回数
    phi = phi_vals[i] # 成功確率
    prob_vec = prob_lt[i] # 確率

    # 統計量を計算
    if phi > 0.0:
        mean_x = r * (1.0 - phi) / phi                 # 期待値
        sd_x   = np.sqrt(r * (1.0 - phi) / phi**2)     # 標準偏差
        mode_x = np.floor((r - 1) * (1.0 - phi) / phi) # 最頻値
        mode_x = 1.0 if r <= 1 else mode_x
    else: # (0除算回避用)
        mean_x = np.inf
        sd_x   = np.inf
        mode_x = np.inf
    
    # 負の二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968'
    ) # 確率
    ax.vlines(
        x=mean_x, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='--', 
        label=f'$E[x] = \\frac{{r (1 - \phi)}}{{\phi}} = {mean_x:.2f}$'
    ) # 期待値の位置
    ax.vlines(
        x=[mean_x-sd_x, mean_x+sd_x], ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles=':', 
        label=f'$\\sqrt{{V[x]}} = \\sqrt{{\\frac{{r (1 - \phi)}}{{\phi^2}}}} = {sd_x:.2f}$'
    ) # 標準偏差の位置
    ax.hlines(
        y=0.0, xmin=mean_x-sd_x, xmax=mean_x+sd_x, 
        color='black', linewidth=1.0
    ) # 標準偏差の範囲
    ax.vlines(
        x=mode_x, ymin=y_min, ymax=y_max, 
        color='black', linewidth=1.0, linestyles='-.', 
        label=f'$mode[x] = \lfloor \\frac{{(r - 1) (1 - \phi)}}{{\phi}} \\rfloor = {mode_x:.2f}$'
    ) # 最頻値の位置
    ax.text(
        x=mean_x, y=0.0, 
        s='$E[x] \pm \\sqrt{V[x]}$', ha='center', va='top', 
        size=8
    ) # 統計量のラベル
    #ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('$x$')
    ax.set_ylabel('probability')
    ax.set_title(f'$r = {r}, \\phi = {phi:.2f}$', loc='left')
    ax.legend(title='statistics', prop={'size': 8}, loc='upper left')
    ax.set_xlim(xmin=x_min-x_max*x_margin, xmax=x_max*(1.0+x_margin)) # (垂線がはみ出す回避用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (垂線との対応用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/negative_binomial/parameter/stats.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

### パラメータとモーメントの関係 -----

# 確率軸の範囲を設定
u = 0.05
tmp_arr = np.array(prob_lt)
prob_max = np.max(tmp_arr[np.isfinite(tmp_arr)])
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ

# 余白を追加
x_margin = 0.05
y_margin = 0.05
y_min = -prob_max * y_margin
y_max = prob_max * (1.0+y_margin)

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Negative Binomial distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を取得
    r   = r_vals[i]   # 成功回数
    phi = phi_vals[i] # 成功確率
    nbinom_prob_vec = prob_lt[i] # 確率

    # 統計量を計算
    if phi > 0.0:
        mu    = r * (1.0 - phi) / phi             # 期待値
        sigma = np.sqrt(r * (1.0 - phi) / phi**2) # 標準偏差
    else: # (0除算の回避用)
        mu    = np.inf
        sigma = np.inf
    
    # モーメントを計算
    if phi < 1.0 and r > 0:
        skew = (2.0 - phi) / np.sqrt((1.0 - phi) * r) # 歪度
    else: # (0除算の回避用)
        skew = np.inf
    if phi > 0.0 and phi < 1.0 and r > 0:
        kurt = 6.0 / r + phi**2 / (1.0 - phi) / r # 尖度
    else: # (0除算の回避用)
        kurt = np.inf

    # ガウス分布の確率密度を計算
    norm_x_vec    = np.linspace(start=x_min, stop=x_max, num=1001)
    norm_dens_vec = norm.pdf(x=norm_x_vec, loc=mu, scale=sigma)
    
    # ラベル用の文字列を作成
    moment_str  = f'skewness: {skew:.3f} \n'
    moment_str += f'kurtosis:    {kurt:.3f}' # (スペースによる位置調整)

    # 負の二項分布分布を描画
    ax.bar(
        x=x_vec, height=nbinom_prob_vec, 
        color='#00A968', alpha=0.5
    ) # 負の二項分布分布の確率
    ax.scatter(
        x=x_vec, y=nbinom_prob_vec, 
        color='#00A968', s=30
    ) # 負の二項分布分布の確率
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
        x_vec, nbinom_prob_vec, 
        color='#00A968', linewidth=1.0, 
        label='negative binomial'
    ) # 負の二項分布分布の確率
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
    #ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('$x$')
    ax.set_ylabel('probability, density')
    ax.set_title(f'$r = {r}, \\phi = {phi:.2f}, \mu = {mu:.2f}, \sigma = {sigma:.2f}$', loc='left')
    ax.legend(title='distribution', prop={'size': 8}, loc='upper right')
    ax.set_xlim(xmin=x_min-x_max*x_margin, xmax=x_max*(1.0+x_margin)) # (垂線がはみ出す回避用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (垂線との対応用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/negative_binomial/parameter/moment.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%


