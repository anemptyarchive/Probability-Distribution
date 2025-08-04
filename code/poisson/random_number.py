
# ポアソン分布 ------------------------------------------------------------------

# 乱数の可視化


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import poisson
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# 乱数と生成分布の関係 ---------------------------------------------------------

### パラメータの設定 -----

# パラメータを指定
lmd = 4


# %%

### 乱数の生成 -----

# サンプルサイズを指定
N = 3000

# ポアソン分布の乱数を生成
x_n = np.random.poisson(lam=lmd, size=N)


# %%

### 乱数の可視化 -----

#### 1サンプルずつ集計 -----

# フレーム数を指定
frame_num = N
frame_num = 300

# x軸の範囲を設定
u = 5.0
x_max = np.max(x_n[:frame_num]) # 集計対象を抽出
x_max *= 1.5 # 倍率を指定
x_max = np.ceil(x_max /u)*u # u単位で切り上げ

# x軸の値を作成
x_vec = np.arange(start=0, stop=x_max+1, step=1)


# %%

##### 度数の作図 -----

# 度数軸の範囲を設定
u = 5.0
freq_max = np.max([np.sum(x_n[:frame_num] == x) for x in x_vec]) # 集計対象を抽出
freq_max = np.ceil(freq_max /u)*u # u単位で切り上げ

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Poisson distribution', fontsize=20)

# 作図処理を定義
def update(n):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を調整
    n += 1
    
    # サンプルを集計
    freq_vec = np.array([np.sum(x_n[:n] == x) for x in x_vec])

    # サンプルの度数を描画
    ax.bar(x=x_vec, height=freq_vec, color='#00A968', zorder=0) # 度数
    ax.scatter(x=x_n[n-1], y=0.0, c='orange', s=50, clip_on=False, zorder=1) # サンプル
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('frequency')
    ax.set_title(f'$N = {n}, \\lambda = {lmd}$', loc='left')
    ax.set_ylim(ymin=0.0, ymax=freq_max) # 描画範囲を固定

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/poisson/random_number/freq_1smp.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%

##### 相対度数の作図 -----

# ポアソン分布を計算
prob_vec = poisson.pmf(k=x_vec, mu=lmd)

# 確率軸の範囲を設定
u = 0.05
prob_max = np.max(prob_vec)
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ
prob_max = 0.25

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Poisson distribution', fontsize=20)
ax2 = ax.twinx()

# 作図処理を定義
def update(n):

    # 前フレームのグラフを初期化
    ax.cla()
    ax2.cla()

    # 値を調整
    n += 1
    
    # サンプルを集計
    freq_vec = np.array([np.sum(x_n[:n] == x) for x in x_vec])

    # サンプルの相対度数を描画
    ax.bar(x=x_vec, height=prob_vec, 
           facecolor='none', edgecolor='green', linewidth=1.0, linestyle='--', 
           label='generator', zorder=1) # 生成確率
    ax.bar(x=x_vec, height=freq_vec/n, color='#00A968', alpha=0.5, 
           label='random number', zorder=0) # 相対度数
    ax.scatter(x=x_n[n-1], y=0.0, c='orange', s=50, clip_on=False, zorder=2) # サンプル
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('relative frequency, probability')
    ax.set_title(f'$N = {n}, \\lambda = {lmd}$', loc='left')
    ax.legend(title='distribution')
    ax.set_ylim(ymin=0.0, ymax=prob_max) # (描画範囲の統一用)

    # 2軸を設定
    y_vals = ax.get_yticks() * n
    ax2.set_yticks(ticks=y_vals, labels=[f'{y:.1f}' for y in y_vals]) # 度数軸目盛
    ax2.set_ylabel('frequency')
    ax2.yaxis.set_label_position('right') # (ラベルの表示位置が初期化される対策)
    ax2.set_ylim(ymin=0.0, ymax=prob_max*n) # (描画範囲の統一用)

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/poisson/random_number/relfreq_1smp.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%

#### 複数サンプルずつ集計 -----

# フレーム数を指定
frame_num = 300

# 1フレーム当たりのサンプル数を設定
smp_per_frame = N // frame_num

# x軸の範囲を指定
u = 5.0
x_max = np.max(x_n[:(smp_per_frame*frame_num)]) # 集計対象を抽出
x_max *= 1.5 # 倍率を指定
x_max = np.ceil(x_max /u)*u # u単位で切り上げ

# x軸の値を作成
x_vec = np.arange(start=0, stop=x_max+1, step=1)


# %%

##### 度数の作図 -----

# 度数軸の範囲を設定
u = 5.0
freq_max = np.max([np.sum(x_n[:(smp_per_frame*frame_num)] == x) for x in x_vec]) # 集計対象を抽出
freq_max = np.ceil(freq_max /u)*u # u単位で切り上げ

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Poisson distribution', fontsize=20)

# 作図処理を定義
def update(n):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を調整
    n = smp_per_frame * (n+1)
    
    # サンプルを集計
    freq_vec = np.array([np.sum(x_n[:n] == x) for x in x_vec])

    # サンプルの度数を描画
    ax.bar(x=x_vec, height=freq_vec, color='#00A968', zorder=0) # 度数
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('frequency')
    ax.set_title(f'$N = {n}, \\lambda = {lmd}$', loc='left')
    #ax.set_ylim(ymin=0.0, ymax=freq_max) # 描画範囲を固定

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/poisson/random_number/freq_nsmp.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%

##### 相対度数の作図 -----

# ポアソン分布を計算
prob_vec = poisson.pmf(k=x_vec, mu=lmd)

# 確率軸の範囲を設定
u = 0.05
prob_max = np.max(prob_vec)
prob_max = np.ceil(prob_max /u)*u # u単位で切り上げ
prob_max = 0.25

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Poisson distribution', fontsize=20)
ax2 = ax.twinx()

# 作図処理を定義
def update(n):
    
    # 前フレームのグラフを初期化
    ax.cla()
    ax2.cla()

    # 値を調整
    n = smp_per_frame * (n+1)
    
    # サンプルを集計
    freq_vec = np.array([np.sum(x_n[:n] == x) for x in x_vec])

    # サンプルの相対度数を描画
    ax.bar(x=x_vec, height=prob_vec, 
           facecolor='none', edgecolor='green', linewidth=1.0, linestyle='--', 
           label='generator', zorder=1) # 生成確率
    ax.bar(x=x_vec, height=freq_vec/n, color='#00A968', alpha=0.5, 
           label='random number', zorder=0) # 相対度数
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.grid()
    ax.set_xlabel('x')
    ax.set_ylabel('relative frequency, probability')
    ax.set_title(f'$N = {n}, \\lambda = {lmd}$', loc='left')
    ax.legend(title='distribution')
    ax.set_ylim(ymin=0.0, ymax=prob_max) # (描画範囲の統一用)

    # 2軸を設定
    y_vals = ax.get_yticks() * n
    ax2.set_yticks(ticks=y_vals, labels=[f'{y:.1f}' for y in y_vals]) # 度数軸目盛
    ax2.set_ylabel('frequency')
    ax2.yaxis.set_label_position('right') # (ラベルの表示位置が初期化される対策)
    ax2.set_ylim(ymin=0.0, ymax=prob_max*n) # (描画範囲の統一用)

# 動画を作成
anime_freq = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
anime_freq.save(
    filename='../figure/poisson/random_number/relfreq_nsmp.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


 # %%


