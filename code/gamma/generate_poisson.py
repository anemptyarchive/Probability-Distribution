
# ガンマ分布 --------------------------------------------------------------------

# ポアソン分布の生成


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import gamma, poisson
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# ハイパラとサンプル分布の関係 ----------------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num = 101

# 生成分布のパラメータを指定
a_vals = np.tile(1.0, reps=frame_num)
b_vals = np.tile(1.0, reps=frame_num)

# 変化させるパラメータ用
a_vals = np.linspace(start=0, stop=10, num=frame_num+1)[1:] # (0を除去)
print(a_vals[:5])
print(b_vals[:5])


# %%

### 変数の設定 -----

# λ軸の範囲を設定
k = 1.0
u = 1.0 # 整数を指定:(x軸との対応用)
lambda_min  = 0.0 # 整数を指定:(x軸との対応用)
lambda_max  = np.max(a_vals / b_vals) # 期待値の最大値
lambda_max *= k # 定数倍
lambda_max  = np.ceil(lambda_max /u)*u # u単位で切り上げ
print('λ-axis size:', lambda_min, lambda_max)

# λ軸の値を作成
lambda_vec = np.linspace(start=lambda_min, stop=lambda_max, num=1001)
lambda_vec[lambda_vec == 0] = 1e-10 # 値を調整:(infの回避用)
print('λ values:   ', lambda_vec[:5])


# x軸の範囲を設定
x_min = np.ceil(lambda_min).astype(np.int64)  # (固定)
x_max = np.floor(lambda_max).astype(np.int64) # (固定)
print('x-axis size:', x_min, x_max)

# x軸の値を作成
x_vec = np.arange(start=x_min, stop=x_max+1, step=1)
print('x values:   ', x_vec[:5])


# %%

### 分布の生成と作図 -----

# サンプルサイズを指定
N = 7

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
smp_dens_max = poisson.pmf(
  k  = np.floor(lambda_min), # 最頻値
  mu = lambda_min # 期待値の最小値
)
smp_dens_max = np.ceil(smp_dens_max /u)*u # u単位で切り上げ
print('p(x) size:', smp_dens_max)


#%%

#### 期待値パラメータとの関係 -----

# ラベル位置を設定
gen_loc_x = 0.02
gen_loc_y = 0.96
smp_loc_x = 0.02
smp_loc_y = 0.96

# 図を初期化
fig, axes = plt.subplots(
    nrows=2, ncols=1, constrained_layout=True, 
    figsize=(10, 8), dpi=100, facecolor='white'
)
axes2 = [ax.twiny() for ax in  axes] # 第2軸の設定用

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes]
    [ax.cla() for ax in axes2]

    ##### パラメータの生成 -----

    # 生成分布のパラメータを取得
    a = a_vals[i] # 形状パラメータ
    b = b_vals[i] # 尺度パラメータ

    # 統計量を計算
    E_lambda = a / b
    s_lambda = np.sqrt(a) / b
    E_x      = E_lambda
    s_x      = np.sqrt(E_lambda)

    # サンプル分布のパラメータを作成
    pctl_min = gamma.cdf(x=lambda_min, a=a, scale=1.0/b)         # 描画範囲の最小値を取得
    pctl_max = gamma.cdf(x=lambda_max, a=a, scale=1.0/b)         # 描画範囲の最大値を取得
    pctl_n   = np.linspace(start=pctl_min, stop=pctl_max, num=N) # 等間隔の累積確率に設定
    lambda_n = gamma.ppf(q=pctl_n, a=a, scale=1.0/b)             # 分布の形状に応じて設定

    # 値を調整:(infの回避用)
    lambda_n[lambda_n <= 0.0]       = 1e-10
    lambda_n[lambda_n > lambda_max] = lambda_max


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
    ax   = axes[0]
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
    ax.legend(title='generator', prop={'size': 8}, bbox_to_anchor=(1, 1), loc='upper left')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=lambda_min, xmax=lambda_max)   # (目盛の共通化用)
    ax2x.set_xlim(xmin=lambda_min, xmax=lambda_max) # (目盛の共通化用)
    ax.set_ylim(ymin=0.0, ymax=gen_dens_max) # 描画範囲を固定


    ##### サンプル分布の作図 -----

    # 期待値による分布の確率を計算
    E_prob_vec = poisson.pmf(k=x_vec, mu=E_lambda)
    
    # サンプル分布の確率を計算
    smp_prob_lt = [poisson.pmf(k=x_vec, mu=lambda_n[n]) for n in range(N)]

    # 期待値のラベルを作成
    E_data_lbl   = '$E[\lambda]$'
    E_param_lbl  = f'$E[\lambda] = {E_lambda:.2f}$'
    E_stats_lbl  = f'$E[x] = \\frac{{a}}{{b}} = {E_x:.2f}$\n'
    E_stats_lbl += f'$s[x] = \\sqrt{{\\frac{{a}}{{b}}}} = {s_x:.2f}$'

    # サンプルのラベルを作成
    smp_data_lbl_lt  = [f'$\\lambda_{{{n+1}}}$' for n in range(N)]
    smp_param_lbl_lt = [f'$\\lambda_{{{n+1}}} = {lambda_n[n]:.2f}$' for n in range(N)]

    # サンプル分布を描画
    ax   = axes[1]
    ax2x = axes2[1]
    ax.axvline(
        x=E_x, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=10
    ) # 期待値との対応
    for n in range(N):
        ax.axvline(
            x=lambda_n[n], 
            color=cmap(n%color_num), linewidth=1.0, linestyle=':', 
            zorder=11
        ) # サンプルとの対応
    plt.bar(
        x=x_vec, height=E_prob_vec, 
        fc='none', ec='red', linewidth=1.0, linestyle=':', 
        zorder=12
    ) # 期待値による分布
    ax.plot(
        x_vec, E_prob_vec, 
        color='red', linewidth=1.0, linestyle='-.', 
        label=E_param_lbl, 
        zorder=13
    ) # 期待値による分布
    for n in range(N):
        ax.plot(
            x_vec, smp_prob_lt[n], 
            color=cmap(n%color_num), linewidth=1.0, 
            label=smp_param_lbl_lt[n], 
            zorder=14
        ) # サンプル分布
        ax.scatter(
            x=x_vec, y=smp_prob_lt[n], 
            marker='o', fc='none', ec=cmap(n%color_num), 
            zorder=15
        ) # サンプル分布
    ax.text(
        x=smp_loc_x, y=smp_loc_y, 
        s=E_stats_lbl, transform=ax.transAxes, ha='left', va='top', 
        bbox=dict(facecolor='white', alpha=0.8, edgecolor='black', linewidth=0.5), 
        size = 10, 
        zorder=100
    ) # 統計量のラベル
    ax2x.set_xticks(
        ticks =[E_lambda, *lambda_n], 
        labels=[E_data_lbl]+smp_data_lbl_lt
    ) # サンプルのラベル
    ax.set_xlabel('$x$')
    ax.set_ylabel('$p(x \mid \lambda)$')
    ax.set_title('Poisson distribution')
    ax.legend(title='sample', prop={'size': 8}, bbox_to_anchor=(1, 1), loc='upper left')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=x_min, xmax=x_max)   # (目盛の共通化用)
    ax2x.set_xlim(xmin=x_min, xmax=x_max) # (目盛の共通化用)
    ax.set_ylim(ymin=0.0, ymax=smp_dens_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/gamma/generate_poisson/gauss_to_gauss.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%


