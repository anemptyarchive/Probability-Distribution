
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
M = 10

# フレームごとのパラメータを指定
phi_vals = np.arange(start=0.0, stop=1.01, step=0.01)

# フレーム数を設定
frame_num = len(phi_vals)


# %%

### 変数の設定 -----

# x軸の範囲を設定
x_min = 0
x_max = M
print('x size:', x_min, x_max)

# x軸の余白を指定:(「モーメントとの関係」用)
x_margin = 0
x_min -= x_margin
x_max += x_margin

# x軸の値を作成
x_vec = np.arange(start=x_min, stop=x_max+1, step=1)


# %%

### 分布の計算 -----

# 二項分布の確率を計算
prob_lt = [
    binom.pmf(k=x_vec, n=M, p=phi_vals[i]) for i in range(frame_num)
]


# %%

### 分布の作図 -----

#### パラメータと形状の関係 -----

# 確率軸の範囲を設定
u = 0.05
prob_max = np.max(prob_lt)
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ
print('p(x) size:', prob_max)

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Binomial distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()
    
    # 値を取得
    phi = phi_vals[i] # 成功確率パラメータ
    prob_vec = prob_lt[i] # 確率

    # ラベル用の文字列を作成
    param_lbl = f'$M = {M}, \\phi = {phi:.2f}$'
    
    # 二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968'
    ) # 確率
    ax.set_xticks(ticks=x_vec) # x軸目盛
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
    filename='../../figure/binomial/parameter/parameter_phi.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

#### パラメータと統計量の関係 -----

# 確率軸の範囲を設定
u = 0.05
prob_max = np.max(prob_lt)
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ
print('p(x) size:', prob_max)

# ラベルの表示用の余白を設定
y_margin = 0.05
y_min = -prob_max * y_margin
y_max = prob_max * (1.0+y_margin)

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Binomial distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を取得
    phi = phi_vals[i] # 成功確率パラメータ
    prob_vec = prob_lt[i] # 確率

    # 統計量を計算
    mean_x = M * phi                      # 期待値
    sd_x   = np.sqrt(M * phi * (1.0-phi)) # 標準偏差
    mode_x = np.floor((M+1) * phi)        # 最頻値

    # ラベル用の文字列を作成
    param_lbl = f'$M = {M}, \\phi = {phi:.2f}$'
    
    # 二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968', alpha=0.5, 
        zorder=10
    ) # 確率
    ax.axvline(
        x=mean_x, 
        color='black', linewidth=1.0, linestyle='--', 
        label=f'$E[x] = M \\phi = {mean_x:.2f}$', 
        zorder=11
    ) # 期待値の位置
    for i, coord_x in enumerate([mean_x-sd_x, mean_x+sd_x]):
        ax.axvline(
            x=coord_x, 
            color='black', linewidth=1.0, linestyle=':', 
            label=f'$\\sqrt{{V[x]}} = \\sqrt{{M \\phi (1-\\phi)}} = {sd_x:.2f}$' if i == 0 else None, 
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
        label=f'$mode[x] = \\lfloor (M+1) \\phi \\rfloor = {mode_x:.2f}$', 
        zorder=11
    ) # 最頻値の位置
    ax.text(
        x=mean_x, y=0.0, 
        s='$E[x] \pm \\sqrt{V[x]}$', ha='center', va='top', 
        size=8, 
        zorder=12
    ) # 統計量のラベル
    ax.set_xticks(ticks=x_vec) # x軸目盛
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
    filename='../../figure/binomial/parameter/stats_phi.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

#### パラメータとモーメントの関係 -----

# 確率軸の範囲を設定
u = 0.05
prob_max = np.max(prob_lt)
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ
print('p(x) size:', prob_max)

# ラベルの表示用の余白を設定
y_margin = 0.05
y_min = -prob_max * y_margin
y_max = prob_max * (1.0+y_margin)

# ラベルの表示位置を設定
moment_loc_x = 0.02
moment_loc_y = 0.97

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Binomial distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を取得
    phi = phi_vals[i] # 成功確率パラメータ
    prob_vec = prob_lt[i] # 確率

    # 統計量を計算
    mu    = M * phi                      # 期待値
    sigma = np.sqrt(M * phi * (1.0-phi)) # 標準偏差

    # モーメントを計算
    if phi > 0.0 and phi < 1.0: # パラメータの条件
        skew  = 1.0 - 2.0 * phi             # 歪度
        skew /= np.sqrt(M * phi * (1.0-phi))
        kurt  = 1.0 - 6.0 * phi * (1.0-phi) # 尖度
        kurt /= M * phi * (1.0-phi)
    else:
        skew = np.inf
        kurt = np.inf

    # ガウス分布の確率密度を計算
    norm_x_vec    = np.linspace(start=x_min, stop=x_max, num=1001)
    if sigma > 0.0: # パラメータの条件
        norm_dens_vec = norm.pdf(x=norm_x_vec, loc=mu, scale=sigma)
    else:
        norm_dens_vec = np.tile(np.nan, reps=len(norm_x_vec))
    
    # ラベル用の文字列を作成
    param_lbl   = f'$M = {M}, \\phi = {phi:.2f}, '
    param_lbl  += f'\\mu = {mu:.2f}, \\sigma = {sigma:.2f}$'
    moment_lbl  = f'skewness: {skew:.3f}\n'
    moment_lbl += f'kurtosis:    {kurt:.3f}' # (スペースによる位置調整)

    # 二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968', alpha=0.5, 
        zorder=10
    ) # 二項分布の確率
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
        label='poisson', 
        zorder=13
    ) # 二項分布の確率
    ax.scatter(
        x=x_vec, y=prob_vec, 
        color='#00A968', s=30, 
        zorder=13
    ) # 二項分布の確率
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
    ax.set_xticks(ticks=x_vec) # x軸目盛
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
    filename='../../figure/binomial/parameter/moment_phi.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

# パラメータの影響：試行回数 ------------------------------------------------------

### パラメータの設定 -----

# 試行回数の最大値を指定
M_max = 100

# 固定のパラメータを指定
phi = 0.5

# フレーム数を設定
frame_num = M_max + 1


# %%

### 分布の作図 -----

#### パラメータと形状の関係 -----

# 確率軸の範囲を設定
u = 0.05
prob_max = max(
    [binom.pmf(k=np.floor((M+1) * phi), n=M, p=phi).max() for M in range(M_max)] # 最頻値における確率
)
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ
print('p(x) size:', prob_max)

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Binomial distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # パラメータを設定
    M = i # 試行回数パラメータ
    
    # x軸の値を作成
    x_vec = np.arange(start=0, stop=M+1, step=1)
    
    # 二項分布の確率を計算
    prob_vec = binom.pmf(k=x_vec, n=M, p=phi)

    # ラベル用の文字列を作成
    param_lbl = f'$M = {M}, \\phi = {phi}$'
    
    # 二項分布を描画
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
    filename='../../figure/binomial/parameter/parameter_M.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

#### パラメータと統計量の関係 -----

# 確率軸の範囲を設定
u = 0.05
prob_max = max(
    [binom.pmf(k=np.floor((M+1) * phi), n=M, p=phi).max() for M in range(M_max)] # 最頻値における確率
)
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ
print('p(x) size:', prob_max)

# ラベルの表示用の余白を設定
y_margin = 0.05
y_min = -prob_max * y_margin
y_max = prob_max * (1.0+y_margin)

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Binomial distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # パラメータを設定
    M = i # 試行回数パラメータ
    
    # x軸の値を作成
    x_vec = np.arange(start=0, stop=M+1, step=1)
    
    # 二項分布の確率を計算
    prob_vec = binom.pmf(k=x_vec, n=M, p=phi)

    # 統計量を計算
    mean_x = M * phi                      # 期待値
    sd_x   = np.sqrt(M * phi * (1.0-phi)) # 標準偏差
    mode_x = np.floor((M+1) * phi)        # 最頻値

    # ラベル用の文字列を作成
    param_lbl = f'$M = {M}, \\phi = {phi}$'
    
    # 二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968', alpha=0.5, 
        zorder=10
    ) # 確率
    ax.axvline(
        x=mean_x, 
        color='black', linewidth=1.0, linestyle='--', 
        label=f'$E[x] = M \\phi = {mean_x:.2f}$', 
        zorder=11
    ) # 期待値の位置
    for i, coord_x in enumerate([mean_x-sd_x, mean_x+sd_x]):
        ax.axvline(
            x=coord_x, 
            color='black', linewidth=1.0, linestyle=':', 
            label=f'$\\sqrt{{V[x]}} = \\sqrt{{M \\phi (1-\\phi)}} = {sd_x:.2f}$' if i == 0 else None, 
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
        label=f'$mode[x] = \\lfloor (M+1) \\phi \\rfloor = {mode_x:.2f}$', 
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
    ax.set_xlim(xmin=-0.5, xmax=M+0.5) # (垂直線がはみ出すときの対策用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (垂直線との対応用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/binomial/parameter/stats_M.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

#### パラメータとモーメントの関係 -----

# 確率軸の範囲を設定
u = 0.05
prob_max = max(
    [binom.pmf(k=np.floor((M+1) * phi), n=M, p=phi).max() for M in range(M_max)] # 最頻値における確率
)
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ
print('p(x) size:', prob_max)

# x軸の余白を指定
x_margin = 5

# ラベルの表示用の余白を設定
y_margin = 0.05
y_min = -prob_max * y_margin
y_max = prob_max * (1.0+y_margin)

# ラベルの表示位置を設定
moment_loc_x = 0.02
moment_loc_y = 0.97

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Binomial distribution', fontsize=20)

# ラベルの表示位置を設定:(x軸の範囲が変わる対策用)
momemt_lbl = fig.text(
    x=moment_loc_x, y=moment_loc_y, 
    s='', transform=ax.transAxes, ha='left', va='top', 
    bbox=dict(facecolor='white', alpha=0.8, edgecolor='black', linewidth=0.5), 
    size = 10, 
    zorder=100
) # モーメントのラベル

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # パラメータを設定
    M = i # 試行回数パラメータ
    
    # x軸の範囲を設定
    x_min = -x_margin
    x_max = M + x_margin

    # x軸の値を作成
    x_vec       = np.arange(start=x_min, stop=x_max+1, step=1)
    norm_x_vec  = np.linspace(start=x_min, stop=x_max, num=1001)
    
    # 二項分布の確率を計算
    prob_vec = binom.pmf(k=x_vec, n=M, p=phi)

    # 統計量を計算
    mu    = M * phi                      # 期待値
    sigma = np.sqrt(M * phi * (1.0-phi)) # 標準偏差

    # モーメントを計算
    if M > 0: # パラメータの条件
        skew  = 1.0 - 2.0 * phi             # 歪度
        skew /= np.sqrt(M * phi * (1.0-phi))
        kurt  = 1.0 - 6.0 * phi * (1.0-phi) # 尖度
        kurt /= M * phi * (1.0-phi)
    else:
        skew = np.inf
        kurt = np.inf
    
    # ガウス分布の確率密度を計算
    norm_x_vec = np.linspace(start=x_min, stop=x_max, num=1001)
    if sigma > 0.0: # パラメータの条件
        norm_dens_vec = norm.pdf(x=norm_x_vec, loc=mu, scale=sigma)
    else:
        norm_dens_vec = np.tile(np.nan, reps=len(norm_x_vec))
    
    # ラベルの文字列を設定
    param_lbl   = f'$M = {M}, \\phi = {phi}, '
    param_lbl  += f'\\mu = {mu:.2f}, \\sigma = {sigma:.2f}$'
    moment_str  = f'skewness: {skew:.3f}\n'
    moment_str += f'kurtosis:    {kurt:.3f}' # (スペースによる位置調整)

    # ラベルの文字列を更新:(x軸の範囲が変わる対策用)
    momemt_lbl.set_text(moment_str)

    # 二項分布を描画
    ax.bar(
        x=x_vec, height=prob_vec, 
        color='#00A968', alpha=0.5, 
        zorder=10
    ) # 二項分布の確率
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
        label='binomial', 
        zorder=13
    ) # 二項分布の確率
    ax.scatter(
        x=x_vec, y=prob_vec, 
        color='#00A968', s=30, 
        zorder=13
    ) # 二項分布の確率
    ax.plot(
        norm_x_vec, norm_dens_vec, 
        color='red', linewidth=1.0, linestyle='dashed', 
        label='gaussian', 
        zorder=14
    ) # ガウス分布の確率密度
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
    filename='../../figure/binomial/parameter/moment_M.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%


