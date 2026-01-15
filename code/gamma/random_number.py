
# ガンマ分布 --------------------------------------------------------------------

# 乱数の可視化


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import gamma
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# サンプルサイズの影響 -----------------------------------------------------------

### パラメータの設定 -----

# パラメータを指定
a = 2.0
b = 2.0


# %%

### 乱数の生成 -----

# サンプルサイズを指定
N = 1000

# ガンマ分布の乱数を生成
lambda_n = np.random.gamma(shape=a, scale=1.0/b, size=N)


# %%

### 変数の設定 -----

# λ軸の範囲を設定
u = 5.0
lambda_min = 0.0
lambda_max = np.max(lambda_n)
#lambda_max = np.max(lambda_n[:frame_num]) # 「1サンプルずつ」の場合
#lambda_max = np.max(lambda_n[:(smp_per_frame*frame_num)]) # 「複数サンプルずつ」の場合
lambda_max  = np.ceil(lambda_max /u)*u # u単位で切り上げ
print('λ size:', lambda_min, lambda_max)

# λ軸の値を作成
lambda_vec = np.linspace(start=lambda_min, stop=lambda_max, num=1001)


# %%

### 分布の計算 -----

# ガンマ分布の確率密度を計算
dens_vec = gamma.pdf(x=lambda_vec, a=a, scale=1.0/b)


# %%

### 乱数の可視化 -----

# 階級数を指定
bin_num = 40

# 階級幅を設定
bin_size = (lambda_max - lambda_min) / bin_num
print('bar size:', bin_size)


# %%

#### 1サンプルずつ集計 -----

# フレーム数を指定
frame_num = 300


# 度数軸の範囲を設定
u = 5.0
counts, _ = np.histogram(
    a=lambda_n[:frame_num], bins=bin_num, range=(lambda_min, lambda_max)
) # 対象を抽出して集計
freq_max = np.max(counts)
freq_max = np.ceil(freq_max /u)*u # u単位で切り上げ
print('Nx size:', freq_max)

# 密度軸の範囲を設定
u = 0.5
counts, _ = np.histogram(
    a=lambda_n[:frame_num], bins=bin_num, range=(lambda_min, lambda_max), 
    density=True
) # 対象を抽出して集計
dens_max = np.max(counts)
dens_max = np.ceil(dens_max /u)*u # u単位で切り上げ
print('p(x) size:', dens_max)


# %%

##### 度数の作図 -----

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Gamma distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を設定
    n = i + 1 # サンプル数

    # ラベル用の文字列を作成
    param_lbl = f'$N = {n}, a = {a:.2g}, b = {b:.2g}$'
    
    # サンプルの度数を描画
    ax.hist(
        x=lambda_n[:n], 
        bins=bin_num, range=(lambda_min, lambda_max), 
        color='#00A968', 
        zorder=10
    ) # 度数
    ax.scatter(
        x=lambda_n[:i], y=np.zeros(i), 
        color='orange', alpha=0.33, s=10, clip_on=False, 
        zorder=11
    ) # 過去サンプル
    ax.scatter(
        x=lambda_n[i], y=0.0, 
        color='orange', s=50, clip_on=False, 
        zorder=12
    ) # 新サンプル
    ax.set_xlabel('$\lambda$')
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
    filename='../../figure/gamma/random_number/freq_1smp.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

##### 密度の作図 -----

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Gamma distribution', fontsize=20)
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

    # ラベル用の文字列を作成
    param_lbl = f'$N = {n}, a = {a:.2g}, b = {b:.2g}$'

    # サンプルの密度を描画
    ax.hist(
        x=lambda_n[:n], 
        bins=bin_num, range=(lambda_min, lambda_max), density=True, 
        color='#00A968', alpha=0.5, 
        label='random number', zorder=10
    ) # 密度
    ax.plot(
        lambda_vec, dens_vec, 
        color='green', linewidth=1.0, linestyle='--', 
        label='generator', zorder=11
    ) # 確率密度
    ax.scatter(
        x=lambda_n[:(n-1)], y=np.zeros(n-1), 
        color='orange', alpha=0.33, s=10, clip_on=False, 
        zorder=12
    ) # 過去サンプル
    ax.scatter(
        x=lambda_n[n-1], y=0.0, 
        color='orange', s=50, clip_on=False, 
        zorder=13
    ) # 新サンプル
    ax.set_xlabel('$\lambda$')
    ax.set_ylabel('density')
    ax.set_title(param_lbl, loc='left')
    ax.legend(title='distribution', loc='upper right')
    ax.grid()
    ax.set_ylim(ymin=0.0, ymax=dens_max) # (目盛の共通化用)

    # 度数軸を設定
    freq_max  = dens_max * bin_size * n
    dens_vals = ax.get_yticks()          # 密度軸目盛を取得
    freq_vals = dens_vals * bin_size * n # 度数軸目盛に変換

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
    filename='../../figure/gamma/random_number/dens_1smp.mp4', 
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
counts, _ = np.histogram(
    a=lambda_n[:(smp_per_frame*frame_num)], bins=bin_num, range=(lambda_min, lambda_max)
) # 対象を抽出して集計
freq_max = np.max(counts)
freq_max = np.ceil(freq_max /u)*u # u単位で切り上げ
print('Nx size:', freq_max)

# 密度軸の範囲を設定
u = 0.5
counts, _ = np.histogram(
    a=lambda_n[:(smp_per_frame*frame_num)], bins=bin_num, range=(lambda_min, lambda_max), 
    density=True
) # 対象を抽出して集計
dens_max = np.max(counts)
dens_max = np.ceil(dens_max /u)*u # u単位で切り上げ
dens_max = 0.3
print('p(x) size:', dens_max)


# %%

##### 度数の作図 -----

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Gamma distribution', fontsize=20)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を設定
    n = smp_per_frame * (i+1) # サンプル数

    # ラベル用の文字列を作成
    param_lbl = f'$N = {n}, a = {a:.2g}, b = {b:.2g}$'
    
    # サンプルの度数を描画
    ax.hist(
        x=lambda_n[:n], 
        bins=bin_num, range=(lambda_min, lambda_max), 
        color='#00A968', 
        zorder=10
    ) # 度数
    ax.scatter(
        x=lambda_n[:n], y=np.zeros(n), 
        color='orange', alpha=0.5, s=10, clip_on=False, 
        zorder=11
    ) # サンプル
    ax.set_xlabel('$\lambda$')
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
    filename='../../figure/gamma/random_number/freq_nsmp.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

##### 密度の作図 -----

# 図を初期化
fig, ax = plt.subplots(figsize=(9, 6), dpi=100, facecolor='white')
fig.suptitle('Gamma distribution', fontsize=20)
ax2 = ax.twinx() # 第2軸の設定用

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

    # ラベル用の文字列を作成
    param_lbl = f'$N = {n}, a = {a:.2g}, b = {b:.2g}$'

    # サンプルの密度を描画
    ax.hist(
        x=lambda_n[:n], 
        bins=bin_num, range=(lambda_min, lambda_max), density=True, 
        color='#00A968', alpha=0.5, 
        label='random number', zorder=0
    ) # 密度
    ax.plot(
        lambda_vec, dens_vec, 
        color='green', linewidth=1.0, linestyle='--', 
        label='generator', zorder=1
    ) # 確率密度
    ax.scatter(
        x=lambda_n[:n], y=np.zeros(n), 
        color='orange', alpha=0.5, s=10, clip_on=False, zorder=2
    ) # サンプル
    ax.grid()
    ax.set_xlabel('$\lambda$')
    ax.set_ylabel('density')
    ax.set_title(param_lbl, loc='left')
    ax.set_ylim(ymin=0.0, ymax=dens_max) # 描画範囲を固定 # (目盛の共通化用)

    # 度数軸を設定
    freq_max  = dens_max * bin_size * n
    dens_vals = ax.get_yticks()          # 密度軸目盛を取得
    freq_vals = dens_vals * bin_size * n # 度数軸目盛に変換

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
    filename='../../figure/gamma/random_number/dens_nsmp.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%


