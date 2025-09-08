
# 混合ポアソン分布 --------------------------------------------------------------

# パラメータの可視化


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import poisson
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# パラメータの影響 ---------------------------------------------------------------

### パラメータの設定 -----

# フレーム数を設定
frame_num = 101

# クラスタ数を指定
K = 3

# フレームごとの期待値パラメータを指定
#'''
lambda_arr = np.stack(
    [np.repeat(a=20, repeats=frame_num), 
     np.linspace(start=0.0, stop=60.0, num=frame_num), 
     np.repeat(a=40, repeats=frame_num)], 
    axis=1
) # フレームごとの値を指定
'''
lambda_arr = np.tile(
    np.array([20.0, 30.0, 40.0]), 
    reps=(frame_num, 1)
) # 一定の値を指定
'''
print(lambda_arr[:5])

# フレームごとの混合比率パラメータを指定
'''
pi_1 = 1.0/3.0 # 固定する値を指定
pi_arr = np.stack(
    [np.repeat(a=pi_1, repeats=frame_num), 
     np.linspace(start=0.0, stop=(1.0-pi_1), num=frame_num), 
     (1.0-pi_1) - np.linspace(start=0.0, stop=(1-pi_1), num=frame_num)], 
    axis=1
) # フレームごとの値を作成
'''
pi_arr = np.tile(
    np.repeat(a=1.0/K, repeats=K), 
    reps=(frame_num, 1)
) # 一定の値を指定
#'''
print(pi_arr[:5])


# %%

### 変数の設定 -----

# x軸の範囲を設定
x_min = 0.0
u = 5.0
x_max = np.max(lambda_arr) # 基準値を指定
x_max *= 1.5 # 倍率を指定
x_max = np.ceil(x_max /u)*u # u単位で切り上げ
#x_max = 80
print(x_max)

# x軸の値を作成
x_vec = np.arange(start=x_min, stop=x_max+1, step=1)


# %%

### 分布の計算 -----

# 混合ポアソン分布の重み付け確率を計算
weighted_prob_lt = [
    pi_arr[i] * poisson.pmf(k=x_vec[:, np.newaxis], mu=lambda_arr[i]) for i in range(frame_num)
]


# %%

### パラメータと形状の関係 -----

# カラーマップを作成:(配色の共通化用)
cmap = plt.get_cmap('tab10') # カラーマップを指定
color_num = 10               # カラーマップの色数を設定

# 確率軸の範囲を設定
u = 0.05
prob_max = np.max(np.sum(weighted_prob_lt, axis=2))
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ
print(prob_max)

# 余白を追加
y_margin = 0.05
y_min = -prob_max * y_margin
y_max = prob_max * (1.0+y_margin)
print(y_max)

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('mixture Poisson distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を取得
    lambda_k = lambda_arr[i] # パラメータ
    pi_k     = pi_arr[i]     # 混合比率
    
    # ラベル用の文字列を作成
    lambda_str = ', '.join([f'{lmd:.2f}' for lmd in lambda_k])
    pi_str     = ', '.join([f'{pi:.2f}' for pi in pi_k])
    stats_str  = '\mu_k = \\lambda_k, \\sigma_k = \\sqrt{\\lambda_k}'
    param_lbl  = f'$K = {K}, \\lambda = ({lambda_str}), \\pi = ({pi_str}), {stats_str}$'
    
    for k in range(K):

        # 値を取得
        lmd = lambda_k[k] # パラメータ
        weighted_prob_vec = weighted_prob_lt[i][:, k] # 重み付け確率
        bottom_vec = np.sum(weighted_prob_lt[i][:, :k], axis=1) # 累積重み付け確率
        color_tp = cmap(k%color_num) # 色

        # 統計量を計算
        mu    = lmd          # 期待値
        sigma = np.sqrt(lmd) # 標準偏差

        # 混合ポアソン分布を描画
        plt.bar(
            x=x_vec, bottom=bottom_vec, height=weighted_prob_vec, 
            color=color_tp, alpha=0.5, 
            label=f'$k = {k+1}$', 
            zorder=16
        ) # 周辺確率
        ax.plot(
            x_vec, weighted_prob_vec, 
            color=color_tp, linewidth=1.0, 
            zorder=17
        ) # 重み付け確率
        ax.scatter(
            x=x_vec, y=weighted_prob_vec, 
            color=color_tp, s=10, 
            zorder=18
        ) # 重み付け確率
        ax.vlines(
            x=mu, ymin=0.0, ymax=y_max, 
            color=color_tp, linewidth=1.0, linestyles='--', 
            zorder=11
        ) # 期待値の位置
        ax.hlines(
            y=0.5*y_min, xmin=mu-sigma, xmax=mu+sigma, 
            color=color_tp, linewidth=1.0, 
            zorder=12
        ) # 標準偏差の範囲
        for label_x in [mu-sigma, mu+sigma]:
            ax.text(
                x=label_x, y=0.5*y_min, 
                s='|', ha='center', va='center', 
                color=color_tp, size=6, 
                zorder=13
            ) # 標準偏差の指示線
        ax.text(
            x=mu, y=prob_max, 
            s=f'$\\lambda_{k+1}$', ha='center', va='center', 
            size=10, 
            zorder=14
        ) # パラメータのラベル
        ax.text(
            x=mu, y=0.5*y_min, 
            s=f'$\\mu_{k+1} \pm \\sigma_{k+1}$', ha='center', va='center', 
            size=10, 
            zorder=15
        ) # 統計量のラベル
    ax.set_xlabel('$x$')
    ax.set_ylabel('$p(x \\mid \\lambda, \pi) = \\sum_{k=1}^K \pi_k p(x \\mid s = k, \\lambda_k)$')
    ax.set_title(param_lbl, loc='left')
    ax.legend(title='cluster', prop={'size': 8}, loc='upper right')
    ax.grid(zorder=0)
    ax.set_ylim(ymin=y_min, ymax=y_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/mixture_poisson/parameter/parameter.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%


