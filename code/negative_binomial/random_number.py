
# 負の二項分布 ------------------------------------------------------------------

# 乱数の可視化


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import nbinom
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# サンプルサイズの影響 -----------------------------------------------------------

# パラメータを指定
r   = 5
phi = 0.4


# %%

### 乱数の生成 -----

# サンプルサイズを指定
N = 3000

# 負の二項分布の乱数を生成
x_n = np.random.negative_binomial(n=r, p=phi, size=N)

# %%

### 変数の設定 -----

# x軸の範囲を指定
u = 10.0
x_max = np.max(x_n)
#x_max = np.max(x_n[:frame_num]) # 「1サンプルずつ」の場合
#x_max = np.max(x_n[:(smp_per_frame*frame_num)]) # 「複数サンプルずつ」の場合
x_max = np.ceil(x_max /u)*u # u単位で切り上げ
print(x_max)

# x軸の値を作成
x_vec = np.arange(start=0, stop=x_max+1, step=1)


# %%

### 分布の計算 -----

# 負の二項分布の確率を計算
prob_vec = nbinom.pmf(k=x_vec, n=r, p=phi)


# %%

### 乱数の可視化 -----

#### 1サンプルずつ集計 -----

# フレーム数を指定
frame_num = 300


# %%

##### 度数の作図 -----

# 度数軸の範囲を設定
u = 5.0
counts, bins = np.histogram(a=x_n[:frame_num], bins=int(x_max+1), range=(-0.5, x_max+0.5)) # 対象を抽出して集計
freq_max = np.max(counts)
freq_max = np.ceil(freq_max /u)*u # u単位で切り上げ

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Negative Binomial distribution', fontsize=20)

# 度数を初期化
freq_vec = np.zeros_like(a=x_vec, dtype='int') # (簡易集計処理用)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(n):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を調整
    n += 1
    
    # サンプルを集計
    #freq_vec = np.array([np.sum(x_n[:n] == x) for x in x_vec])
    freq_vec[x_n[n-1]] += 1 # (簡易集計処理用)

    # サンプルの度数を描画
    ax.bar(
        x=x_vec, height=freq_vec, 
        color='#00A968', zorder=0
    ) # 度数
    ax.scatter(
        x=x_n[n-1], y=0.0, 
        c='orange', s=50, clip_on=False, zorder=1
    ) # サンプル
    #ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('$x$')
    ax.set_ylabel('frequency')
    ax.set_title(f'$N = {n}, r = {r}, \\phi = {phi}$', loc='left')
    ax.set_ylim(ymin=0.0, ymax=freq_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/negative_binomial/random_number/freq_1smp.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

##### 相対度数の作図 -----

# 相対度数軸の範囲を設定
relfreq_max = 0.25
