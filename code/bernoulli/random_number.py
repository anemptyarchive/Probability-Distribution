# ベルヌーイ分布 ----------------------------------------------------------------

# 乱数の可視化


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import bernoulli
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# サンプルサイズの影響 -----------------------------------------------------------

### パラメータの設定 -----

# パラメータを指定
phi = 0.3


# %%

### 乱数の生成 -----

# サンプルサイズを指定
N = 1000

# ベルヌーイ分布の乱数を生成
x_n = np.random.binomial(n=1, p=phi, size=N)


# %%

### 変数の設定 -----

# x軸の範囲を設定
x_min = 0 # (固定)
x_max = 1
print('x size:', x_min, x_max)

# x軸の値を作成
x_vec = np.arange(start=x_min, stop=x_max+1, step=1)


# %%

### 分布の計算 -----

# ベルヌーイ分布の確率を計算
prob_vec = bernoulli.pmf(k=x_vec, p=phi)


# %%

### 乱数の可視化 -----

#### 1サンプルずつ集計 -----

# フレーム数を指定
frame_num = 300


# 度数軸の範囲を設定
u = 5.0
_, counts = np.unique(ar=x_n[:frame_num], return_counts=True) # 対象を抽出して集計
freq_max = np.max(counts)
freq_max = np.ceil(freq_max /u)*u # u単位で切り上げ
print('Nx size:', freq_max)

# 相対度数軸の範囲を設定
u = 1.0
_, counts = np.unique(ar=x_n[:frame_num], return_counts=True) # 対象を抽出して集計
relfreq_max = np.max(counts) / frame_num
relfreq_max = np.ceil(relfreq_max /u)*u # u単位で切り上げ
print('p(x) size:', relfreq_max)


# %%

##### 度数の作図 -----

# 階級幅を設定
bin_size = 1.0 # (固定)

# 度数を初期化
freq_vec = np.zeros_like(a=x_vec, dtype='int') # (簡易集計処理用)

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Bernoulli distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を設定
    n = i + 1 # サンプル数
    
    # サンプルを集計
    #freq_vec = np.array([np.sum(x_n[:n] == x) for x in x_vec])
    freq_vec[x_n[i]] += 1 # (簡易集計処理用)

    # ラベル用の文字列を作成
    param_lbl = f'$N = {n}, \\phi = {phi:.2g}$'

    # サンプルの度数を描画
    ax.bar(
        x=x_vec, height=freq_vec, 
        width=bin_size, align='center', 
        color='#00A968', 
        zorder=10
    ) # 度数
    ax.scatter(
        x=x_n[i], y=0.0, 
        c='orange', s=50, clip_on=False, 
        zorder=11
    ) # サンプル
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.set_xlabel('$x$')
    ax.set_ylabel('frequency')
    ax.set_title(param_lbl, loc='left')
    ax.grid()
    ax.set_ylim(ymin=0.0, ymax=freq_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/bernoulli/random_number/freq_1smp.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

##### 相対度数の作図 -----

# 度数を初期化
freq_vec = np.zeros_like(a=x_vec, dtype='int') # (簡易集計処理用)

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Bernoulli distribution', fontsize=20)
ax2 = ax.twinx() # 第2軸の設定用

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()
    ax2.cla()

    # 値を設定
    n = i + 1 # サンプル数
    
    # サンプルを集計
    #freq_vec = np.array([np.sum(x_n[:n] == x) for x in x_vec])
    freq_vec[x_n[i]] += 1 # (簡易集計処理用)

    # ラベル用の文字列を作成
    param_lbl = f'$N = {n}, \\phi = {phi:.2g}$'

    # サンプルの相対度数を描画
    ax.bar(
        x=x_vec, height=freq_vec/n, 
        color='#00A968', alpha=0.5, 
        label='random number', zorder=10
    ) # 相対度数
    ax.bar(
        x=x_vec, height=prob_vec, 
        facecolor='none', edgecolor='green', linewidth=1.0, linestyle='--', 
        label='generator', zorder=11
    ) # 確率
    ax.scatter(
        x=x_n[i], y=0.0, 
        c='orange', s=50, clip_on=False, 
        zorder=12
    ) # サンプル
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.set_xlabel('$x$')
    ax.set_ylabel('relative frequency, probability')
    ax.set_title(param_lbl, loc='left')
    ax.legend(title='distribution', loc='upper right')
    ax.grid()
    ax.set_ylim(ymin=0.0, ymax=relfreq_max) # (目盛の共通化用)

    # 度数軸を設定
    freq_max     = relfreq_max * n
    relfreq_vals = ax.get_yticks()  # 相対度数軸目盛を取得
    freq_vals    = relfreq_vals * n # 度数軸目盛に変換

    # 第2軸を描画
    ax2.set_yticks(ticks=freq_vals, labels=[f'{y:.1f}' for y in freq_vals]) # 度数軸目盛
    ax2.set_ylabel('frequency')
    ax2.yaxis.set_label_position(position='right') # (ラベルの表示位置が初期化される対策)
    ax2.set_ylim(ymin=0.0, ymax=freq_max) # (目盛の共通化用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/bernoulli/random_number/relfreq_1smp.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

#### 複数サンプルずつ集計 -----

# フレーム数を指定
frame_num = 300

# 1フレーム当たりのサンプル数を設定
smp_per_frame = N // frame_num


# 度数軸の範囲を設定
u = 5.0
_, counts = np.unique(ar=x_n[:(smp_per_frame*frame_num)], return_counts=True) # 対象を抽出して集計
freq_max = np.max(counts)
freq_max = np.ceil(freq_max /u)*u # u単位で切り上げ
print('Nx size:', freq_max)

# 相対度数軸の範囲を設定
u = 1.0
relfreq_max = np.max(prob_vec)
relfreq_max = np.ceil(relfreq_max /u)*u # u単位で切り上げ
print('p(x) size:', relfreq_max)


# %%

##### 度数の作図 -----

# 階級幅を設定
bin_size = 1.0 # (固定)

# 度数を初期化
freq_vec = np.zeros_like(a=x_vec, dtype='int') # (簡易集計処理用)

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Bernoulli distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を設定
    n = smp_per_frame * (i+1) # サンプル数

    # 集計対象を抽出
    tmp_x_n = x_n[(smp_per_frame*i):(smp_per_frame*(i+1))] # (簡易集計処理用)
    
    # サンプルを集計
    #freq_vec = np.array([np.sum(x_n[:n] == x) for x in x_vec])
    #freq_vec[:] += np.array([np.sum(tmp_x_n == x) for x in x_vec]) # (簡易集計処理用)
    for x in tmp_x_n:
        freq_vec[x] += 1 # (簡易集計処理用)

    # ラベル用の文字列を作成
    param_lbl = f'$N = {n}, \\phi = {phi:.2g}$'

    # サンプルの度数を描画
    ax.bar(
        x=x_vec, height=freq_vec, 
        width=bin_size, align='center', 
        color='#00A968'
    ) # 度数
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.set_xlabel('$x$')
    ax.set_ylabel('frequency')
    ax.set_title(param_lbl, loc='left')
    ax.grid()
    #ax.set_ylim(ymin=0.0, ymax=freq_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/bernoulli/random_number/freq_nsmp.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

##### 相対度数の作図 -----

# 度数を初期化
freq_vec = np.zeros_like(a=x_vec, dtype='int') # (簡易集計処理用)

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('Bernoulli distribution', fontsize=20)
ax2 = ax.twinx() # 第2軸の設定用

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):
    
    # 前フレームのグラフを初期化
    ax.cla()
    ax2.cla()

    # 値を調整
    n = smp_per_frame * (i+1) # サンプル数

    # 集計対象を抽出
    tmp_x_n = x_n[(smp_per_frame*i):(smp_per_frame*(i+1))] # (簡易集計処理用)
    
    # サンプルを集計
    #freq_vec = np.array([np.sum(x_n[:n] == x) for x in x_vec])
    #freq_vec[:] += np.array([np.sum(tmp_x_n == x) for x in x_vec]) # (簡易集計処理用)
    for x in tmp_x_n:
        freq_vec[x] += 1 # (簡易集計処理用)

    # ラベル用の文字列を作成
    param_lbl = f'$N = {n}, \\phi = {phi:.2g}$'

    # サンプルの相対度数を描画
    ax.bar(
        x=x_vec, height=freq_vec/n, 
        color='#00A968', alpha=0.5, 
        label='random number', zorder=10
    ) # 相対度数
    ax.bar(
        x=x_vec, height=prob_vec, 
        facecolor='none', edgecolor='green', linewidth=1.0, linestyle='--', 
        label='generator', zorder=11
    ) # 確率
    ax.set_xticks(ticks=x_vec) # x軸目盛
    ax.set_xlabel('$x$')
    ax.set_ylabel('relative frequency, probability')
    ax.set_title(param_lbl, loc='left')
    ax.legend(title='distribution', loc='upper right')
    ax.grid()
    ax.set_ylim(ymin=0.0, ymax=relfreq_max) # (目盛の共通化用)

    # 度数軸を設定
    freq_max     = relfreq_max * n
    relfreq_vals = ax.get_yticks()  # 相対度数軸目盛を取得
    freq_vals    = relfreq_vals * n # 度数軸目盛に変換

    # 第2軸を描画
    ax2.set_yticks(ticks=freq_vals, labels=[f'{y:.1f}' for y in freq_vals]) # 度数軸目盛
    ax2.set_ylabel('frequency')
    ax2.yaxis.set_label_position(position='right') # (ラベルの表示位置が初期化される対策)
    ax2.set_ylim(ymin=0.0, ymax=freq_max) # (目盛の共通化用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/bernoulli/random_number/relfreq_nsmp.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%


