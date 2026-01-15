
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

# パラメータの影響 ---------------------------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num = 101

# フレームごとのパラメータを指定
r_vals   = np.linspace(start=0, stop=100, num=frame_num, dtype='int')
phi_vals = np.linspace(start=0.5, stop=0.5, num=frame_num)


# %%

### 変数の設定 -----

# x軸の範囲を設定
k = 1
u = 5.0
x_min = 0
tmp_vals = r_vals * (1.0-phi_vals) / phi_vals # 基準値を指定
x_max = np.max(tmp_vals[np.isfinite(tmp_vals)])
x_max *= k # 定数倍
x_max = np.ceil(x_max /u)*u # u単位で切り上げ
x_max = x_max.astype(np.int64) # 整数型に変換
print('x size:', x_min, x_max)

# x軸の余白を指定:(「モーメントとの関係」用)
x_margin = 0
x_min -= x_margin
x_max += x_margin

# x軸の値を作成
x_vec = np.arange(start=x_min, stop=x_max+1, step=1)


# %%

### 分布の計算 -----

# 負の二項分布の確率を計算
prob_lt = [
    nbinom.pmf(k=x_vec, n=r_vals[i], p=phi_vals[i]) for i in range(frame_num)
]


# %%

### 分布の作図 -----

# 確率軸の範囲を設定
u = 0.05
tmp_arr  = np.array(prob_lt)
prob_max = np.max(tmp_arr[np.isfinite(tmp_arr)])
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ
print('p(x) size:', prob_max)


# %%

#### パラメータと形状の関係 -----

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
    r   = r_vals[i]   # 成功回数パラメータ
    phi = phi_vals[i] # 成功確率パラメータ
    prob_vec = prob_lt[i] # 確率

    # ラベル用の文字列を作成
    param_lbl = f'$r = {r}, \\phi = {phi:.2f}$'
    
    # 負の二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968'
    ) # 確率
    #ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.set_xlabel('$x$')
    ax.set_ylabel('probability')
    ax.set_title(param_lbl, loc='left')
    ax.grid()
    ax.set_ylim(ymin=0.0, ymax=prob_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/negative_binomial/parameter/parameter.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

#### パラメータと統計量の関係 -----

# ラベルの表示用の余白を設定
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
    r   = r_vals[i]   # 成功回数パラメータ
    phi = phi_vals[i] # 成功確率パラメータ
    prob_vec = prob_lt[i] # 確率

    # 統計量を計算
    if phi > 0.0:
        mean_x = r * (1.0-phi) / phi               # 期待値
        sd_x   = np.sqrt(r * (1.0-phi) / phi**2)   # 標準偏差
        mode_x = np.floor((r-1) * (1.0-phi) / phi) # 最頻値
        mode_x = 1.0 if r <= 1 else mode_x
    else: # 0除算を回避
        mean_x = np.inf
        sd_x   = np.inf
        mode_x = np.inf

    # ラベル用の文字列を作成
    param_lbl = f'$r = {r}, \\phi = {phi:.2f}$'
    
    # 負の二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968', alpha=0.5, 
        zorder=10
    ) # 確率
    ax.axvline(
        x=mean_x, 
        color='black', linewidth=1.0, linestyle='--', 
        label=f'$E[x] = \\frac{{r (1-\phi)}}{{\phi}} = {mean_x:.2f}$', 
        zorder=11
    ) # 期待値の位置
    for i, coord_x in enumerate([mean_x-sd_x, mean_x+sd_x]):
        ax.axvline(
            x=coord_x, 
            color='black', linewidth=1.0, linestyle=':', 
            label=f'$\\sqrt{{V[x]}} = \\sqrt{{\\frac{{r (1-\phi)}}{{\phi^2}}}} = {sd_x:.2f}$' if i == 0 else None, 
            zorder=11
        ) # 標準偏差の位置
    ax.hlines(
        y=0.0, xmin=mean_x-sd_x, xmax=mean_x+sd_x, 
        color='black', linewidth=1.0, 
        zorder=11
    ) # 標準偏差の範囲
    ax.axvline(
        x=mode_x, 
        color='black', linewidth=1.0, linestyle='-.', 
        label=f'$mode[x] = \lfloor \\frac{{(r-1) (1-\phi)}}{{\phi}} \\rfloor = {mode_x:.2f}$', 
        zorder=11
    ) # 最頻値の位置
    ax.text(
        x=mean_x, y=0.0, 
        s='$E[x] \pm \\sqrt{V[x]}$', ha='center', va='top', 
        size=8, 
        zorder=12
    ) # 統計量のラベル
    #ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.set_xlabel('$x$')
    ax.set_ylabel('probability')
    ax.set_title(param_lbl, loc='left')
    ax.legend(title='statistics', prop={'size': 8}, loc='upper left')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=x_min-0.5, xmax=x_max+0.5) # (垂直線がはみ出すときの対策用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (ラベルの表示用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/negative_binomial/parameter/stats.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

#### パラメータとモーメントの関係 -----

# ラベルの表示用の余白を設定
y_margin = 0.05
y_min = -prob_max * y_margin
y_max = prob_max * (1.0+y_margin)

# ラベルの表示位置を設定
moment_loc_x = 0.02
moment_loc_y = 0.97

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
    r   = r_vals[i]   # 成功回数パラメータ
    phi = phi_vals[i] # 成功確率パラメータ
    prob_vec = prob_lt[i] # 確率

    # 統計量を計算
    if phi > 0.0:
        mu    = r * (1.0-phi) / phi             # 期待値
        sigma = np.sqrt(r * (1.0-phi) / phi**2) # 標準偏差
    else: # 0除算を回避
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
    if sigma > 0.0: # パラメータの条件
        norm_dens_vec = norm.pdf(x=norm_x_vec, loc=mu, scale=sigma)
    else:
        norm_dens_vec = np.tile(np.nan, reps=len(norm_x_vec))
    
    # ラベル用の文字列を作成
    param_lbl   = f'$r = {r}, \\phi = {phi:.2f}, '
    param_lbl  += f'\\mu = {mu:.2f}, \\sigma = {sigma:.2f}$'
    moment_lbl  = f'skewness: {skew:.3f} \n'
    moment_lbl += f'kurtosis:    {kurt:.3f}' # (スペースによる位置調整)

    # 負の二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968', alpha=0.5, 
        zorder=10
    ) # 負の二項分布の確率
    ax.axvline(
        x=mu, 
        color='black', linewidth=1.0, linestyle='dashed', 
        zorder=11
    ) # 期待値の位置
    ax.hlines(
        y=0.0, xmin=mu-sigma, xmax=mu+sigma, 
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
    ax.plot(
        x_vec, prob_vec, 
        color='#00A968', linewidth=1.0, 
        label='negative binomial', 
        zorder=13
    ) # 負の二項分布の確率
    ax.scatter(
        x=x_vec, y=prob_vec, 
        color='#00A968', s=30, 
        zorder=13
    ) # 負の二項分布の確率
    ax.plot(
        norm_x_vec, norm_dens_vec, 
        color='red', linewidth=1.0, linestyle='dashed', 
        label='gaussian', 
        zorder=14
    ) # ガウス分布の確率密度
    ax.text(
        x=moment_loc_x, y=moment_loc_y, 
        s=moment_lbl, transform=ax.transAxes, ha='left', va='top', 
        bbox=dict(facecolor='white', alpha=0.8, edgecolor='black', linewidth=0.5), 
        size = 10, 
        zorder=100
    ) # モーメントのラベル
    #ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.set_xlabel('$x$')
    ax.set_ylabel('probability, density')
    ax.set_title(param_lbl, loc='left')
    ax.legend(title='distribution', prop={'size': 8}, loc='upper right')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=x_min-0.5, xmax=x_max+0.5) # (垂直線がはみ出すときの対策用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (ラベルの表示用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/negative_binomial/parameter/moment.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%


