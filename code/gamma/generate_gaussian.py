
# ガンマ分布 --------------------------------------------------------------------

# 1次元ガウス分布の生成


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import gamma, norm
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# ハイパラとサンプル分布の関係 ----------------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num = 100

# 生成分布のパラメータを指定
a_vals = np.tile(1.0, reps=frame_num)
b_vals = np.tile(1.0, reps=frame_num)

# 変化させるパラメータ用
a_vals = np.linspace(start=0, stop=10, num=frame_num+1)[1:] # (0を除去)
print(a_vals[:5])
print(b_vals[:5])


# サンプル分布のパラメータを指定
mu = 0.0


# %%

### 変数の設定 -----

# λ軸の範囲を設定
k = 1.0
u = 1.0
lambda_min  = 0.0
lambda_max  = np.max(a_vals / b_vals) # 期待値の最大値
lambda_max *= k # 定数倍
lambda_max  = np.ceil(lambda_max /u)*u  # u単位で切り上げ
print('λ-axis size:', lambda_min, lambda_max)

# λ軸の値を作成
lambda_vec = np.linspace(start=lambda_min, stop=lambda_max, num=1001)
lambda_vec[lambda_vec == 0] = 1e-10 # 値を調整:(infの回避用)
print('λ values:   ', lambda_vec[:5])


# x軸の範囲を設定
k = 2.0
u = 2.0
x_size  = 1.0/np.sqrt(lambda_max) # 標準偏差の最小値
x_size *= k # 定数倍
x_size  = np.ceil(x_size /u)*u # u単位で切り上げ
x_min   = mu - x_size # 期待値 - 定数倍の標準偏差
x_max   = mu + x_size # 期待値 - 定数倍の標準偏差
print('x-axis size:', x_min, x_max)

# x軸の値を作成
x_vec = np.linspace(start=x_min, stop=x_max, num=1001)
print('x values:   ', x_vec[:5])


# %%

### 分布の生成と作図 -----

# サンプルサイズを指定
N = 6

# カラーマップを作成:(配色の共通化用)
cmap = plt.get_cmap('tab10') # カラーマップを指定
color_num = 10               # カラーマップの色数を設定


# p(λ)軸の範囲を設定
u = 0.05
gen_dens_max = gamma.pdf(
  x     = (a_vals-1.0) / b_vals, # 最頻値
  a     = a_vals, 
  scale = 1.0/b_vals
).max()
gen_dens_max = np.ceil(gen_dens_max /u)*u # u単位で切り上げ
print('p(λ) size:', gen_dens_max)

# p(x)軸の範囲を設定
u = 0.05
smp_dens_max = norm.pdf(
  x     = mu, # 最頻値
  loc   = mu, 
  scale = 1.0/np.sqrt(lambda_max) # 最大値による標準偏差
)
smp_dens_max = np.ceil(smp_dens_max /u)*u # u単位で切り上げ
print('p(x) size:', smp_dens_max)


# %%

#### 精度パラメータとの関係 -----

# ラベル位置を設定
gen_loc_x = 0.02
gen_loc_y = 0.96
smp_loc_x = 0.02
smp_loc_y = 0.96

# 変換曲線の座標を計算
adapt_coord_vec = norm.pdf(x=mu, loc=mu, scale=1.0/np.sqrt(lambda_vec)) # 最頻値における確率密度

# 図を初期化
fig, axes = plt.subplots(
    nrows=2, ncols=2, constrained_layout=True, 
    figsize=(12, 8), dpi=100, facecolor='white'
)
axes2 = [axes[0, 0].twiny(), axes[1, 1].twinx()] # 第2軸の設定用

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes.flatten()]
    [ax.cla() for ax in axes2]

    ##### パラメータの生成 -----

    # 生成分布のパラメータを取得
    a = a_vals[i] # 形状パラメータ
    b = b_vals[i] # 尺度パラメータ

    # 統計量を計算
    E_lambda = a / b
    s_lambda = np.sqrt(a) / b
    E_x      = mu
    s_x      = np.sqrt(b / a)

    # サンプル分布のパラメータを作成
    pctl_min = gamma.cdf(x=lambda_min, a=a, scale=1.0/b)         # 描画範囲の最小値を取得
    pctl_max = gamma.cdf(x=lambda_max, a=a, scale=1.0/b)         # 描画範囲の最大値を取得
    pctl_n   = np.linspace(start=pctl_min, stop=pctl_max, num=N) # 等間隔の累積確率に設定
    lambda_n = gamma.ppf(q=pctl_n, a=a, scale=1.0/b)             # 分布の形状に応じて設定

    # 値を調整:(infの回避用)
    lambda_n[lambda_n <= 0.0]       = 1e-10
    lambda_n[lambda_n > lambda_max] = lambda_max

    # 期待値・サンプルに対応する座標を計算
    E_dens_max     = norm.pdf(x=mu, loc=mu, scale=1.0/np.sqrt(E_lambda))
    smp_dens_max_n = norm.pdf(x=mu, loc=mu, scale=1.0/np.sqrt(lambda_n))


    ##### 生成分布の作図 -----

    # 生成分布の確率密度を計算
    gen_dens_vec = gamma.pdf(x=lambda_vec, a=a, scale=1.0/b)

    # 生成分布のラベルを作成
    gen_param_lbl  = f'$a = {a:.2f}, b = {b:.2f}$'
    gen_stats_lbl  = f'$E[\\lambda] = \\frac{{a}}{{b}} = {E_lambda:.2f}$\n'
    gen_stats_lbl += f'$s[\\lambda] = \\frac{{\\sqrt{{a}}}}{{b}} = {s_lambda:.2f}$'

    # 期待値のラベルを作成
    E_data_lbl = '$E[\lambda]$'

    # サンプルのラベルを作成
    smp_data_lbl_lt = [f'$\\lambda_{{{n+1}}}$' for n in range(N)]

    # 生成分布を描画
    ax   = axes[0, 0]
    ax2x = axes2[0]
    ax.axvline(
        x=E_lambda, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=10
    ) # 期待値の位置
    for n in range(N):
        ax.axvline(
            x=lambda_n[n], 
            color=cmap(n%color_num), linewidth=1.0, linestyle=':', 
            zorder=11
        ) # サンプルの位置
    ax.plot(
        lambda_vec, gen_dens_vec, 
        color='black', linewidth=1.0, 
        label=gen_param_lbl, 
        zorder=12
    ) # 生成分布
    for n in range(N):
        ax.scatter(
            x=lambda_n[n], y=0.0, 
            color=cmap(n%color_num), s=50, clip_on=False, 
            zorder=13
        ) # サンプル
    ax.text(
        x=gen_loc_x, y=gen_loc_y, 
        s=gen_stats_lbl, transform=ax.transAxes, ha='left', va='top', 
        bbox=dict(facecolor='white', alpha=0.8, edgecolor='black', linewidth=0.5), 
        size = 10, 
        zorder=100
    ) # 統計量のラベル
    ax2x.set_xticks(
        ticks =[E_lambda, *lambda_n], 
        labels=[E_data_lbl]+smp_data_lbl_lt
    ) # サンプルのラベル
    ax.set_xlabel('$\lambda$')
    ax.set_ylabel('$p(\lambda \\mid a, b)$')
    ax.set_title('Gamma distribution')
    #ax.legend(title='generator', prop={'size': 8}, bbox_to_anchor=(1, 1), loc='upper left')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=lambda_min, xmax=lambda_max)   # (目盛の共通化用)
    ax2x.set_xlim(xmin=lambda_min, xmax=lambda_max) # (目盛の共通化用)
    ax.set_ylim(ymin=0.0, ymax=gen_dens_max) # 描画範囲を固定


    ##### 軸変換の作図:(λ -> p(x)) -----

    # 軸変換を作図
    ax = axes[1, 0]
    ax.plot(
        lambda_vec, adapt_coord_vec, 
        color='black', linewidth=1.0, 
        zorder=10
    ) # 変換曲線
    ax.vlines(
        x=E_lambda, ymin=E_dens_max, ymax=smp_dens_max, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=11
    ) # 生成分布との対応
    ax.hlines(
        y=E_dens_max, xmin=E_lambda, xmax=lambda_max, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=11
    ) # サンプル分布との対応
    for n in range(N):
        ax.vlines(
            x=lambda_n[n], ymin=smp_dens_max_n[n], ymax=smp_dens_max, 
            color=cmap(n%color_num), linewidth=1.0, linestyle=':', 
            zorder=12
        ) # 生成分布との対応
        ax.hlines(
            y=smp_dens_max_n[n], xmin=lambda_n[n], xmax=lambda_max, 
            color=cmap(n%color_num), linewidth=1.0, linestyle=':', 
            zorder=12
        ) # サンプル分布との対応
    for n in range(N):
        ax.scatter(
            x=lambda_n[n], y=smp_dens_max_n[n], 
            color=cmap(n%color_num), s=50, clip_on=False, 
            zorder=13
        ) # サンプル
    ax.set_xlabel('$\lambda$')
    ax.set_ylabel('$p(x = \mu \mid \mu, \lambda^{-1})$')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=lambda_min, xmax=lambda_max) # (目盛の共通化用)
    ax.set_ylim(ymin=0.0, ymax=smp_dens_max) # (目盛の共通化用)


    ##### サンプル分布の作図 -----

    # 期待値による分布の確率密度を計算
    E_dens_vec = norm.pdf(x=x_vec, loc=mu, scale=s_x)

    # サンプル分布の確率密度を計算
    smp_dens_lt = [norm.pdf(x=x_vec, loc=mu, scale=1.0/np.sqrt(lambda_n[n])) for n in range(N)]

    # 期待値のラベルを作成
    E_data_lbl   = '$p(\mu \mid \mu, E[\lambda]^{-1})$'
    E_param_lbl  = f'$\\mu = {mu:.2f}, E[\\lambda] = {E_lambda:.2f}$'
    E_stats_lbl  = f'$E[x] = \\mu = {E_x:.2f}$\n'
    E_stats_lbl += f'$s[x] = \\sqrt{{\\frac{{b}}{{a}}}} = {s_x:.2f}$'

    # サンプルのラベルを作成
    smp_data_lbl_lt  = [f'$p(\\mu \\mid \\mu, \\lambda_{{{n+1}}}^{{-1}})$' for n in range(N)]
    smp_param_lbl_lt = [f'$\\mu = {mu:.2f}, \\lambda_{{{n+1}}} = {lambda_n[n]:.2f}$' for n in range(N)]

    # サンプル分布を描画
    ax   = axes[1, 1]
    ax2y = axes2[1]
    ax.axhline(
        y=E_x, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=10
    ) # 期待値との対応
    for n in range(N):
        ax.axhline(
            y=smp_dens_max_n[n], 
            color=cmap(n%color_num), linewidth=1.0, linestyle=':', 
            zorder=11
        ) # サンプルとの対応
    ax.plot(
        x_vec, E_dens_vec, 
        color='red', linewidth=1.0, linestyle='-.', 
        label=E_param_lbl, 
        zorder=12
    ) # 期待値による分布
    for n in range(N):
        ax.plot(
            x_vec, smp_dens_lt[n], 
            color=cmap(n%color_num), linewidth=1.0, 
            label=smp_param_lbl_lt[n], 
            zorder=13
        ) # サンプル分布
    ax.text(
        x=smp_loc_x, y=smp_loc_y, 
        s=E_stats_lbl, transform=ax.transAxes, ha='left', va='top', 
        bbox=dict(facecolor='white', alpha=0.8, edgecolor='black', linewidth=0.5), 
        size = 10, 
        zorder=100
    ) # 統計量のラベル
    ax2y.set_yticks(
        ticks =[E_dens_max, *smp_dens_max_n], 
        labels=[E_data_lbl]+smp_data_lbl_lt
    ) # サンプルのラベル
    ax.set_xlabel('$x$')
    ax.set_ylabel('$p(x \mid \mu, \lambda^{-1})$')
    ax.set_title('Gaussian distribution')
    #ax.legend(title='sample', prop={'size': 8}, bbox_to_anchor=(1, 1), loc='upper left')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=x_min, xmax=x_max)
    ax.set_ylim(ymin=0.0, ymax=smp_dens_max)   # (目盛の共通化用)
    ax2y.set_ylim(ymin=0.0, ymax=smp_dens_max) # (目盛の共通化用)


    ##### 凡例の作図 -----

    # グラフオブジェクトを取得
    ax = axes[0, 1]
    ax.axis('off')

    # 凡例情報を取得
    gen_handles, gen_labels = axes[0, 0].get_legend_handles_labels()
    smp_handles, smp_labels = axes[1, 1].get_legend_handles_labels()

    # 凡例の体裁を設定
    gen_legend = ax.legend(
        handles=gen_handles, labels=gen_labels, 
        loc='center', bbox_to_anchor=(0.25, 0.5), 
        title='generator', prop={'size': 8}
    )
    smp_legend = ax.legend(
        handles=smp_handles, labels=smp_labels, 
        loc='center', bbox_to_anchor=(0.75, 0.5), 
        title='sample', prop={'size': 8}
    )

    # 凡例を並べて描画
    ax.add_artist(gen_legend)
    ax.add_artist(smp_legend)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/gamma/generate_gaussian/gam_to_gauss_dens.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

#### 標準偏差パラメータとの関係 -----

# ラベル位置を設定
gen_loc_x = 0.02
gen_loc_y = 0.96
smp_loc_x = 0.02
smp_loc_y = 0.96

# 変換曲線の座標を計算
sigma_vec = 1.0/np.sqrt(lambda_vec) # 標準偏差

# 図を初期化
fig, axes = plt.subplots(
    nrows=2, ncols=2, constrained_layout=True, 
    figsize=(12, 8), dpi=100, facecolor='white'
)
axes2 = [ax.twiny() for ax in [axes[0, 0], axes[0, 1], axes[1, 1]]] # 第2軸の設定用

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes.flatten()]
    [ax.cla() for ax in axes2]

    ##### パラメータの生成 -----

    # 生成分布のパラメータを取得
    a = a_vals[i] # 形状パラメータ
    b = b_vals[i] # 尺度パラメータ

    # 統計量を計算
    E_lambda = a / b
    s_lambda = np.sqrt(a) / b
    E_x      = mu
    s_x      = np.sqrt(b / a)

    # パラメータを生成
    pctl_min = gamma.cdf(x=lambda_min, a=a, scale=1.0/b)         # 描画範囲の最小値を取得
    pctl_max = gamma.cdf(x=lambda_max, a=a, scale=1.0/b)         # 描画範囲の最大値を取得
    pctl_n   = np.linspace(start=pctl_min, stop=pctl_max, num=N) # 等間隔の累積確率に設定
    lambda_n = gamma.ppf(q=pctl_n, a=a, scale=1.0/b)             # 分布の形状に応じて設定

    # 値を調整:(infの回避用)
    lambda_n[lambda_n <= 0.0]       = 1e-10
    lambda_n[lambda_n > lambda_max] = lambda_max

    # 標準偏差パラメータに変換
    E_sigma = 1.0/np.sqrt(E_lambda)
    sigma_n = 1.0/np.sqrt(lambda_n)

    # 期待値・サンプルに対応する座標を計算
    E_dens_sgm     = norm.pdf(x=mu+E_sigma, loc=mu, scale=E_sigma)
    smp_dens_sgm_n = norm.pdf(x=mu+sigma_n, loc=mu, scale=sigma_n)


    ##### 生成分布の作図 -----

    # 生成分布の確率密度を計算
    gen_dens_vec = gamma.pdf(x=lambda_vec, a=a, scale=1.0/b)

    # 生成分布のラベルを作成
    gen_param_lbl  = f'$a = {a:.2f}, b = {b:.2f}$'
    gen_stats_lbl  = f'$E[\\lambda] = \\frac{{a}}{{b}} = {E_lambda:.2f}$\n'
    gen_stats_lbl += f'$s[\\lambda] = \\frac{{\\sqrt{{a}}}}{{b}} = {s_lambda:.2f}$'

    # 期待値のラベルを作成
    E_data_lbl = '$E[\lambda]$'

    # サンプルのラベルを作成
    smp_data_lbl_lt = [f'$\\lambda_{{{n+1}}}$' for n in range(N)]

    # 生成分布を描画
    ax   = axes[0, 0]
    ax2x = axes2[0]
    ax.axvline(
        x=E_lambda, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=10
    ) # 期待値の位置
    for n in range(N):
        ax.axvline(
            x=lambda_n[n], 
            color=cmap(n%color_num), linewidth=1.0, linestyle=':', 
            zorder=11
        ) # サンプルの位置
    ax.plot(
        lambda_vec, gen_dens_vec, 
        color='black', linewidth=1.0, 
        label=gen_param_lbl, 
        zorder=12
    ) # 生成分布
    for n in range(N):
        ax.scatter(
            x=lambda_n[n], y=0.0, 
            color=cmap(n%color_num), s=50, clip_on=False, 
            zorder=13
        ) # サンプル
    ax.text(
        x=gen_loc_x, y=gen_loc_y, 
        s=gen_stats_lbl, transform=ax.transAxes, ha='left', va='top', 
        bbox=dict(facecolor='white', alpha=0.8, edgecolor='black', linewidth=0.5), 
        size = 10, 
        zorder=100
    ) # 統計量のラベル
    ax2x.set_xticks(
        ticks =[E_lambda, *lambda_n], 
        labels=[E_data_lbl]+smp_data_lbl_lt
    ) # サンプルのラベル
    ax.set_xlabel('$\lambda$')
    ax.set_ylabel('$p(\lambda \\mid a, b)$')
    ax.set_title('Gamma distribution')
    ax.legend(title='generator', prop={'size': 8}, loc='upper right')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=lambda_min, xmax=lambda_max)   # (目盛の共通化用)
    ax2x.set_xlim(xmin=lambda_min, xmax=lambda_max) # (目盛の共通化用)
    ax.set_ylim(ymin=0.0, ymax=gen_dens_max) # 描画範囲を固定


    ##### サンプル分布の作図 -----

    # 期待値による分布の確率密度を計算
    E_dens_vec = norm.pdf(x=x_vec, loc=mu, scale=E_sigma)

    # サンプル分布の確率密度を計算
    smp_dens_lt = [norm.pdf(x=x_vec, loc=mu, scale=1.0/np.sqrt(lambda_n[n])) for n in range(N)]

    # 期待値のラベルを作成
    E_param_lbl  = f'$\\mu = {mu:.2f}, E[\\lambda] = {E_lambda:.2f}$'
    E_stats_lbl  = f'$E[x] = \\mu = {E_x:.2f}$\n'
    E_stats_lbl += f'$s[x] = \\sqrt{{\\frac{{b}}{{a}}}} = {s_x:.2f}$'

    # サンプルのラベルを作成
    smp_param_lbl_lt = [f'$\\mu = {mu:.2f}, \\lambda_{{{n+1}}} = {lambda_n[n]:.2f}$' for n in range(N)]

    # サンプル分布を描画
    ax   = axes[0, 1]
    ax2x = axes2[1]
    ax.quiver(
        mu, 0.0, 0.0, smp_dens_max, 
        units='dots', width=2.0, headwidth=6.0, headlength=10.0, headaxislength=5.0, 
        angles='xy', scale_units='xy', scale=1.0, 
        zorder=5
    ) # y軸線
    ax.vlines(
        x=mu+E_sigma, ymin=0.0, ymax=E_dens_sgm, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=10
    ) # 期待値との対応
    ax.hlines(
        y=E_dens_sgm, xmin=mu, xmax=mu+E_sigma, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=10
    ) # 標準偏差の範囲
    for n in range(N):
        ax.vlines(
            x=mu+sigma_n[n], ymin=0.0, ymax=smp_dens_sgm_n[n], 
            color=cmap(n%color_num), linewidth=1.0, linestyle=':', 
            zorder=11
        ) # サンプルとの対応
        ax.hlines(
            y=smp_dens_sgm_n[n], xmin=mu, xmax=mu+sigma_n[n], 
            color=cmap(n%color_num), linewidth=1.0, linestyle=(0, (6, 2, 2, 2)), 
            zorder=11
        ) # 標準偏差の範囲
    ax.plot(
        x_vec, E_dens_vec, 
        color='red', linewidth=1.0, linestyle='-.', 
        label=E_param_lbl, 
        zorder=12
    ) # 期待値による分布
    for n in range(N):
        ax.plot(
            x_vec, smp_dens_lt[n], 
            color=cmap(n%color_num), linewidth=1.0, 
            label=smp_param_lbl_lt[n], 
            zorder=13
        ) # サンプル分布
    ax.text(
        x=smp_loc_x, y=smp_loc_y, 
        s=E_stats_lbl, transform=ax.transAxes, ha='left', va='top', 
        bbox=dict(facecolor='white', alpha=0.8, edgecolor='black', linewidth=0.5), 
        size = 10, 
        zorder=100
    ) # 統計量のラベル
    ax2x.set_xticks(
        ticks =[mu], 
        labels=['$\mu$']
    ) # パラメータのラベル
    ax.set_xlabel('$x$')
    ax.set_ylabel('$p(x \mid \mu, \lambda^{-1})$')
    ax.set_title('Gaussian distribution')
    ax.legend(title='sample', prop={'size': 8}, bbox_to_anchor=(1, 1), loc='upper left')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=x_min, xmax=x_max)   # (目盛の共通化用)
    ax2x.set_xlim(xmin=x_min, xmax=x_max) # (目盛の共通化用)
    ax.set_ylim(ymin=0.0, ymax=smp_dens_max) # (目盛の共通化用)


    ##### 軸変換の作図:(λ -> λ) -----

    # 軸変換を作図
    ax = axes[1, 0]
    ax.plot(
        lambda_vec, lambda_vec, 
        color='black', linewidth=1.0, 
        zorder=10
    ) # 変換曲線
    ax.vlines(
        x=E_lambda, ymin=E_lambda, ymax=lambda_max, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=11
    ) # 生成分布との対応
    ax.hlines(
        y=E_lambda, xmin=E_lambda, xmax=lambda_max, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=11
    ) # 変換曲線との対応
    for n in range(N):
        ax.vlines(
            x=lambda_n[n], ymin=lambda_n[n], ymax=lambda_max, 
            color=cmap(n%color_num), linewidth=1.0, linestyle=':', 
            zorder=12
        ) # 生成分布との対応
        ax.hlines(
            y=lambda_n[n], xmin=lambda_n[n], xmax=lambda_max, 
            color=cmap(n%color_num), linewidth=1.0, linestyle=':', 
            zorder=12
        ) # 変換曲線との対応
    for n in range(N):
        ax.scatter(
            x=lambda_n[n], y=lambda_n[n], 
            color=cmap(n%color_num), s=50, clip_on=False, 
            zorder=13
        ) # サンプル
    ax.set_xlabel('$\lambda$')
    ax.set_ylabel('$\lambda$')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=lambda_min, xmax=lambda_max) # (目盛の共通化用)
    ax.set_ylim(ymin=lambda_min, ymax=lambda_max) # (目盛の共通化用)


    ##### 軸変換の作図:(λ -> σ) -----

    # 期待値のラベルを作成
    E_data_lbl  = '$E[\sigma]$'
    E_param_lbl = f'$E[\\sigma] = {E_sigma:.2f}$'

    # サンプルのラベルを作成
    smp_data_lbl_lt  = [f'$\\sigma_{{{n+1}}}$' for n in range(N)]
    smp_param_lbl_lt = [f'$\\sigma_{{{n+1}}} = {sigma_n[n]:.2f}$' for n in range(N)]

    # 軸変換を作図
    ax   = axes[1, 1]
    ax2x = axes2[2]
    ax.quiver(
        0.0, 0.0, 0.0, lambda_max, 
        units='dots', width=2.0, headwidth=6.0, headlength=10.0, headaxislength=5.0, 
        angles='xy', scale_units='xy', scale=1.0, 
        zorder=5
    ) # y軸線
    ax.plot(
        sigma_vec, lambda_vec, 
        color='black', linewidth=1.0, 
        zorder=10
    ) # 変換曲線
    ax.hlines(
        y=E_lambda, xmin=x_min, xmax=0.0, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=11
    ) # 恒等関数との対応
    ax.hlines(
        y=E_lambda, xmin=0.0, xmax=E_sigma, 
        color='red', linewidth=1.0, linestyle='--', 
        label=E_param_lbl, 
        zorder=11
    ) # 標準偏差の範囲
    ax.vlines(
        x=E_sigma, ymin=E_lambda, ymax=lambda_max, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=11
    ) # サンプル分布との対応
    for n in range(N):
        ax.hlines(
            y=lambda_n[n], xmin=x_min, xmax=0.0, 
            color=cmap(n%color_num), linewidth=1.0, linestyle=':', 
            zorder=12
        ) # 恒等関数との対応
        ax.hlines(
            y=lambda_n[n], xmin=0.0, xmax=sigma_n[n], 
            color=cmap(n%color_num), linewidth=1.0, linestyle=(0, (6, 2, 2, 2)), 
            label=smp_param_lbl_lt[n], 
            zorder=12
        ) # 標準偏差の範囲
        ax.vlines(
            x=sigma_n[n], ymin=lambda_n[n], ymax=lambda_max, 
            color=cmap(n%color_num), linewidth=1.0, linestyle=':', 
            zorder=12
        ) # サンプル分布との対応
    for n in range(N):
        ax.scatter(
            x=sigma_n[n] if sigma_n[n] <= x_max else np.nan, y=lambda_n[n], 
            color=cmap(n%color_num), s=50, clip_on=False, 
            zorder=13
        ) # サンプル
    ax2x.set_xticks(
        ticks =[mu+E_sigma, *mu+sigma_n], 
        labels=[E_data_lbl]+smp_data_lbl_lt
    ) # サンプルのラベル
    ax.set_xlabel('$\sigma = \\frac{1}{\sqrt{\lambda}}$')
    ax.set_ylabel('$\lambda = \\frac{1}{\sigma^2}$')
    ax.legend(title='sample', prop={'size': 8}, bbox_to_anchor=(1, 1), loc='upper left')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=x_min, xmax=x_max)   # (目盛の共通化用)
    ax2x.set_xlim(xmin=x_min, xmax=x_max) # (目盛の共通化用)
    ax.set_ylim(ymin=lambda_min, ymax=lambda_max) # (目盛の共通化用)

'''
##### 凡例の作図 -----

# グラフオブジェクトを取得
ax = axes[0, 1]
ax.axis('off')

# 凡例情報を取得
gen_handles, gen_labels = axes[0, 0].get_legend_handles_labels()
smp_handles, smp_labels = axes[1, 1].get_legend_handles_labels()

# 凡例の体裁を設定
gen_legend = ax.legend(
    handles=gen_handles, labels=gen_labels, 
    loc='center', bbox_to_anchor=(0.25, 0.5), 
    title='generator', prop={'size': 8}
)
smp_legend = ax.legend(
    handles=smp_handles, labels=smp_labels, 
    loc='center', bbox_to_anchor=(0.75, 0.5), 
    title='sample', prop={'size': 8}
)

# 凡例を並べて描画
ax.add_artist(gen_legend)
ax.add_artist(smp_legend)
'''

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/gamma/generate_gaussian/gam_to_gauss_sd.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%
