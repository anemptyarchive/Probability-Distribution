
# 1次元ガウス分布 ---------------------------------------------------------------

# 確率分布の生成
## 1次元ガウス分布の平均パラメータとの関係


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import norm
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# ハイパラとサンプル分布の形状の関係 ----------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num = 101

# 生成分布のパラメータを指定
mu_vals    = np.linspace(start=-5.0, stop=5.0, num=frame_num)
sigma_vals = np.linspace(start=1.0, stop=1.0, num=frame_num)
#mu_vals    = np.linspace(start=0.0, stop=0.0, num=frame_num)
#sigma_vals = np.linspace(start=0.0, stop=10.0, num=frame_num+1)[1:] # (0を除去)
print('μ values:', mu_vals[:5])
print('σ values:', sigma_vals[:5])

# サンプル分布のパラメータを指定
s = 1.0


# %%

### 変数の設定 -----

# m軸の範囲を設定
u = 5.0
sgm_num = 1.0 # 倍率を指定
m_min = np.min(mu_vals - sgm_num*sigma_vals)
m_max = np.max(mu_vals + sgm_num*sigma_vals)
m_min = np.floor(m_min /u)*u # u単位で切り下げ
m_max = np.ceil(m_max /u)*u  # u単位で切り上げ
print('m-axis size:', m_min, m_max)

# m軸の値を作成
m_vec = np.linspace(start=m_min, stop=m_max, num=1001)
print('m values:', m_vec[:5])


# x軸の範囲を設定
x_min = m_min
x_max = m_max
print('x-axis size:', x_min, x_max)

# x軸の値を作成
x_vec = m_vec.copy()
print('x values:', x_vec[:5])


# %%

# サンプルサイズを指定
N = 7

# カラーマップを作成:(配色の共通化用)
cmap = plt.get_cmap('tab10') # カラーマップを指定
color_num = 10               # カラーマップの色数を設定


# 確率密度軸の範囲を設定
u = 0.05
gen_dens_max = np.max(norm.pdf(x=mu_vals, loc=mu_vals, scale=sigma_vals)) # 最頻値における値
gen_dens_max = np.ceil(gen_dens_max /u)*u # u単位で切り上げ
smp_dens_max = np.max(norm.pdf(x=mu_vals, loc=mu_vals, scale=s)) # 期待値による最頻値における値
smp_dens_max = np.ceil(smp_dens_max /u)*u # u単位で切り上げ
print('p-axis size:', gen_dens_max, smp_dens_max)

# 横軸の余白を追加
x_margin_rate = 0.05
u = 0.05
gen_x_margin_size = (m_max - m_min) * x_margin_rate
smp_x_margin_size = (x_max - x_min) * x_margin_rate
gen_x_margin_size = np.ceil(gen_x_margin_size /u)*u # u単位で切り上げ
smp_x_margin_size = np.ceil(smp_x_margin_size /u)*u # u単位で切り上げ
print('x-axis margin:', gen_x_margin_size, smp_x_margin_size)

# 縦軸の余白を追加
y_margin_rate = 0.049
u = 0.01
gen_y_margin_size = gen_dens_max * y_margin_rate
smp_y_margin_size = smp_dens_max * y_margin_rate
gen_y_margin_size = np.ceil(gen_y_margin_size /u)*u # u単位で切り上げ
smp_y_margin_size = np.ceil(smp_y_margin_size /u)*u # u単位で切り上げ
print('y-axis margin:', gen_y_margin_size, smp_y_margin_size)


# 図を初期化
fig, axes = plt.subplots(
    nrows=2, ncols=1, constrained_layout=True, 
    figsize=(10, 8), dpi=100, facecolor='white'
)
fig.suptitle('Gaussian distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes]

    # パラメータを取得
    mu    = mu_vals[i]
    sigma = sigma_vals[i]

    # サンプルの期待値を計算
    E_m = mu
    E_x = E_m

    # パラメータを生成
    pctl_min = norm.cdf(x=m_min, loc=mu, scale=sigma)            # 描画範囲の最小値を取得
    pctl_max = norm.cdf(x=m_max, loc=mu, scale=sigma)            # 描画範囲の最大値を取得
    pctl_n   = np.linspace(start=pctl_min, stop=pctl_max, num=N) # 等間隔の累積確率に設定
    m_n      = norm.ppf(q=pctl_n, loc=mu, scale=sigma)           # 分布の形状に応じて設定

    # 値を調整:(infの回避用)
    m_n[m_n < m_min] = m_min
    m_n[m_n > m_max] = m_max

    # 確率分布を計算
    gen_dens_vec = norm.pdf(x=m_vec, loc=mu, scale=sigma) # 生成分布
    E_dens_vec   = norm.pdf(x=x_vec, loc=E_m, scale=s)    # 期待値による分布
    smp_dens_lt  = [norm.pdf(x=x_vec, loc=m_n[n], scale=s) for n in range(N)] # サンプル分布

    # 生成分布を描画
    ax = axes[0]
    ax.axvline(
        x=E_m, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=0
    ) # 期待値の位置
    ax.plot(
        m_vec, gen_dens_vec, 
        color='black', linewidth=1.0, 
        label=f'$\\mu = {mu:.2f}, \\sigma = {sigma:.2f}$', 
        zorder=2
    ) # 生成分布

    # サンプル分布を描画
    ax = axes[1]
    ax.axvline(
        x=E_x, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=0
    ) # 期待値の位置
    ax.plot(
        x_vec, E_dens_vec, 
        color='red', linewidth=1.0, linestyle='--', 
        zorder=2
    ) # 期待値による分布

    for n in range(N):
        
        # 値を取得
        m = m_n[n] # サンプル
        smp_dens_vec = smp_dens_lt[n] # 確率密度
        color_tp = cmap(n%color_num) # 色

        # 生成分布を描画
        ax = axes[0]
        ax.axvline(
            x=m, 
            color=color_tp, linewidth=1.0, linestyle=':', 
            zorder=1
        ) # サンプルの位置
        ax.scatter(
            x=m, y=0.0, 
            color=color_tp, s=50, 
            zorder=3
        ) # サンプル

        # サンプル分布を描画
        ax = axes[1]
        ax.axvline(
            x=m, 
            color=color_tp, linewidth=1.0, linestyle=':', 
            zorder=1
        ) # サンプルの位置
        ax.plot(
            x_vec, smp_dens_vec, 
            color=color_tp, linewidth=1.0, 
            label=f'$m_{{{n+1}}} = {m:.2f}, s = {s}$', 
            zorder=3
        ) # サンプル分布

    # 生成分布を装飾
    ax = axes[0]
    ax.set_xlabel('$m$') # m軸ラベル
    ax.set_ylabel('$p(m \\mid \\mu, \\sigma^2)$') # 確率密度軸ラベル
    ax.set_title(f'$E[m] = {E_m:.2f}$', loc='left') # 期待値ラベル
    ax.legend(title='gaussian', prop={'size': 8}, bbox_to_anchor=(1, 1), loc='upper left') # パラメータラベル
    ax.grid()
    ax.set_xlim(xmin=m_min-gen_x_margin_size, xmax=m_max+gen_x_margin_size) # (軸の対応用)
    ax.set_ylim(ymin=-gen_y_margin_size, ymax=gen_dens_max+gen_y_margin_size) # (軸の固定用)

    # サンプル分布を装飾
    ax = axes[1]
    ax.set_xlabel('$x$') # x軸ラベル
    ax.set_ylabel('$p(x \\mid m, s^2)$') # 確率密度軸ラベル
    ax.set_title(f'$E[x] = {E_x:.2f}$', loc='left') # 期待値ラベル
    ax.legend(title='gaussian', prop={'size': 8}, bbox_to_anchor=(1, 1), loc='upper left') # パラメータラベル
    ax.grid()
    ax.set_xlim(xmin=m_min-smp_x_margin_size, xmax=m_max+smp_x_margin_size) # (軸の対応用)
    ax.set_ylim(ymin=-smp_y_margin_size, ymax=smp_dens_max+smp_y_margin_size) # (軸の固定用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/gaussian/generate_gaussian/gaussian_to_gaussian_mu.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%
