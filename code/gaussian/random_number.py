
# 1次元ガウス分布 ---------------------------------------------------------------

# 乱数の可視化


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import norm
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# サンプルサイズの影響 -----------------------------------------------------------

### パラメータの設定 -----

# パラメータを指定
mu    = 0.0
sigma = 2.5


# %%

### 乱数の生成 -----

# サンプルサイズを指定
N = 3000

# ガウス分布の乱数を生成
x_n = np.random.normal(loc=mu, scale=sigma, size=N)


# %%

### 変数の設定 -----

# x軸の範囲を設定
u = 5.0
x_size = sigma
x_size *= 5.0 # 倍率を指定
x_min = mu - x_size
x_max = mu + x_size
x_min = np.floor(x_min /u)*u # u単位で切り下げ
x_max = np.ceil(x_max /u)*u # u単位で切り上げ

# x軸の値を作成
x_vec = np.linspace(start=x_min, stop=x_max, num=250)


# %%

### 分布の計算 -----

# ガウス分布の確率密度を計算
dens_vec = norm.pdf(x=x_vec, loc=mu, scale=sigma)


# %%

### 乱数の可視化 -----

#### 1サンプルずつ集計 -----

# フレーム数を指定
frame_num = 300


# %%

##### 度数の作図 -----

# 階級数を指定
bin_num = 30

# 度数軸の範囲を設定
u = 5.0
counts, bins = np.histogram(a=x_n[:frame_num], bins=bin_num, range=(x_min, x_max)) # 対象を抽出して集計
freq_max = np.max(counts)
freq_max = np.ceil(freq_max /u)*u # u単位で切り上げ

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Gaussian distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(n):

    # 前フレームのグラフを初期化
    ax.cla()
    
    # 値を調整
    n += 1
    
    # サンプルの度数を描画
    ax.hist(
        x=x_n[:n], 
        bins=bin_num, range=(x_min, x_max), 
        color='#00A968', zorder=0
    ) # 度数
    ax.scatter(
        x=x_n[:(n-1)], y=np.zeros(n-1), 
        color='orange', alpha=0.5, s=10, clip_on=False, zorder=1
    ) # 過去サンプル
    ax.scatter(
        x=x_n[n-1], y=0.0, 
        color='orange', s=50, clip_on=False, zorder=1
    ) # 新サンプル
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('frequency')
    ax.set_title(f'$N = {n}, \mu = {mu}, \sigma = {sigma}$', loc='left')
    ax.set_ylim(ymin=0.0, ymax=freq_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/gaussian/random_number/freq_1smp.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

##### 相対度数の作図 -----

# 階級数を指定
bin_num = 30

# 相対度数軸の範囲を設定
relfreq_max = 0.3

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Gaussian distribution', fontsize=20)
ax2 = ax.twinx()

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(n):

    # 前フレームのグラフを初期化
    ax.cla()
    ax2.cla()
    
    # 値を調整
    n += 1

    # サンプルを集計
    counts, bins = np.histogram(a=x_n[:n], bins=bin_num, range=(x_min, x_max))
    bins = bins[:(len(bins)-1)] + 0.5 * (bins[1] - bins[0]) # プロット位置を調整
    
    # サンプルの相対度数を描画
    ax.bar(
        x=bins, height=counts/n, 
        color='#00A968', alpha=0.5, 
        label='random number', zorder=0
    ) # 相対度数
    ax.plot(
        x_vec, dens_vec, 
        color='green', linewidth=1.0, linestyle='--', 
        label='generator', zorder=1
    ) # 確率密度
    ax.scatter(
        x=x_n[:(n-1)], y=np.zeros(n-1), 
        color='orange', alpha=0.5, s=10, clip_on=False, zorder=1
    ) # 過去サンプル
    ax.scatter(
        x=x_n[n-1], y=0.0, 
        color='orange', s=50, clip_on=False, zorder=1
    ) # 新サンプル
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('relative frequency, density')
    ax.set_title(f'$N = {n}, \mu = {mu}, \sigma = {sigma}$', loc='left')
    ax.set_ylim(ymin=0.0, ymax=relfreq_max) # 描画範囲を固定 # (目盛の共通化用)

    # 2軸を設定
    relfreq_vals = ax.get_yticks()  # 相対度数目盛を取得
    freq_vals    = relfreq_vals * n # 度数目盛に変換
    ax2.set_yticks(ticks=freq_vals, labels=[f'{y:.1f}' for y in freq_vals]) # 度数軸目盛
    ax2.set_ylabel('frequency')
    ax2.yaxis.set_label_position(position='right') # (ラベルの表示位置が初期化される対策)
    ax2.set_ylim(ymin=0.0, ymax=relfreq_max*n) # (目盛の共通化用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/gaussian/random_number/relfreq_1smp.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

#### 複数サンプルずつ集計 -----

# フレーム数を指定
frame_num = 300

# 1フレーム当たりのサンプル数を設定
smp_per_frame = N // frame_num


# %%

##### 度数の作図 -----

# 階級数を指定
bin_num = 30

# 度数軸の範囲を設定
u = 5.0
counts, bins = np.histogram(a=x_n[:(smp_per_frame*frame_num)], bins=bin_num, range=(x_min, x_max)) # 対象を抽出して集計
freq_max = np.max(counts)
freq_max = np.ceil(freq_max /u)*u # u単位で切り上げ

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Gaussian distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(n):

    # 前フレームのグラフを初期化
    ax.cla()
    
    # 値を調整
    n = smp_per_frame * (n+1)
    
    # サンプルの度数を描画
    ax.hist(
        x=x_n[:n], 
        bins=bin_num, range=(x_min, x_max), 
        color='#00A968', zorder=0
    ) # 度数
    ax.scatter(
        x=x_n[:n], y=np.zeros(n), 
        color='orange', alpha=0.5, s=10, clip_on=False, zorder=1
    ) # サンプル
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('frequency')
    ax.set_title(f'$N = {n}, \mu = {mu}, \sigma = {sigma}$', loc='left')
    #ax.set_ylim(ymin=0.0, ymax=freq_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/gaussian/random_number/freq_nsmp.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

##### 相対度数の作図 -----

# 階級数を指定
bin_num = 30

# 相対度数軸の範囲を設定
u = 0.05
relfreq_max = np.max(dens_vec)
relfreq_max = np.ceil(relfreq_max /u)*u # u単位で切り上げ
relfreq_max = 0.3

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Gaussian distribution', fontsize=20)
ax2 = ax.twinx()

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(n):

    # 前フレームのグラフを初期化
    ax.cla()
    ax2.cla()
    
    # 値を調整
    n = smp_per_frame * (n+1)

    # サンプルを集計
    counts, bins = np.histogram(a=x_n[:n], bins=bin_num, range=(x_min, x_max))
    bins = bins[:len(bins)-1] + 0.5 * (bins[1] - bins[0]) # プロット位置を調整
    
    # サンプルの相対度数を描画
    ax.bar(
        x=bins, height=counts/n, 
        color='#00A968', alpha=0.5, 
        label='random number', zorder=0
    ) # 相対度数
    ax.plot(
        x_vec, dens_vec, 
        color='green', linewidth=1.0, linestyle='--', 
        label='generator', zorder=1
    ) # 確率密度
    ax.scatter(
        x=x_n[:n], y=np.zeros(n), 
        color='orange', alpha=0.5, s=10, clip_on=False, zorder=1
    ) # サンプル
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('relative frequency, density')
    ax.set_title(f'$N = {n}, \mu = {mu}, \sigma = {sigma}$', loc='left')
    ax.set_ylim(ymin=0.0, ymax=relfreq_max) # 描画範囲を固定 # (目盛の共通化用)

    # 2軸を設定
    relfreq_vals = ax.get_yticks()  # 相対度数目盛を取得
    freq_vals    = relfreq_vals * n # 度数目盛に変換
    ax2.set_yticks(ticks=freq_vals, labels=[f'{y:.1f}' for y in freq_vals]) # 度数軸目盛
    ax2.set_ylabel('frequency')
    ax2.yaxis.set_label_position(position='right') # (ラベルの表示位置が初期化される対策)
    ax2.set_ylim(ymin=0.0, ymax=relfreq_max*n) # (目盛の共通化用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/gaussian/random_number/relfreq_nsmp.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%


