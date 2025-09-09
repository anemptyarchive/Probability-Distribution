
# 混合ポアソン分布 --------------------------------------------------------------

# 乱数の可視化


# %%

# ライブラリの読込 ---------------------------------------------------------------

# ライブラリを読込
import numpy as np
from scipy.stats import poisson
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# サンプルサイズの影響 -----------------------------------------------------------

### パラメータの設定 -----

# パラメータを指定
lambda_k = np.array([1.0, 5.5, 10.0, 16.8])
pi_k     = np.array([0.2, 0.3, 0.1, 0.4])

# クラスタ数を設定
K = len(lambda_k)


# %%

### 乱数の生成 -----

# サンプルサイズを指定
N = 3000

# クラスタを生成
s_nk = np.random.multinomial(n=1, pvals=pi_k, size=N)

# クラスタ番号を抽出
_, s_n = np.where(s_nk == 1)

# サンプルを生成
x_n = np.random.poisson(lam=lambda_k[s_n], size=N)


# %%

### 変数の設定 -----

# x軸の範囲を設定
u = 10.0
x_min = 0.0
x_max = np.max(x_n)
#x_max = np.max(x_n[:frame_num]) # 「1サンプルずつ」の場合
#x_max = np.max(x_n[:(smp_per_frame*frame_num)]) # 「複数サンプルずつ」の場合
x_max = np.ceil(x_max /u)*u # u単位で切り上げ
print(x_max)

# x軸の値を作成
x_vec = np.arange(start=0, stop=x_max+1, step=1)


# %%

### 分布の計算 -----

# クラスタごとのポアソン分布の重み付け確率を計算
weighted_prob_arr = np.array(
    [pi_k[k] * poisson.pmf(k=x_vec, mu=lambda_k[k]) for k in range(K)]
).T


# %%

### 配色の設定 -----

# カラーマップを作成:(配色の共通化用)
cmap = plt.get_cmap('tab10') # カラーマップを指定
color_num = 10               # カラーマップの色数を設定


# %%

### 乱数の可視化 -----

#### 1サンプルずつ集計 -----

# フレーム数を指定
frame_num = 300


# %%

##### 度数の作図 -----

# 度数軸の範囲を設定
u = 5.0
_, counts = np.unique(ar=x_n[:frame_num], return_counts=True) # 対象を抽出して集計
freq_max = np.max(counts)
freq_max = np.ceil(freq_max /u)*u # u単位で切り上げ
print('frequency:', freq_max)

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('mixture Poisson distribution', fontsize=20)

# 度数を初期化
cluster_freq_arr = np.zeros(shape=(len(x_vec), K), dtype='int') # (簡易集計処理用)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(n):

    # 前フレームのグラフを初期化
    ax.cla()

    # 値を調整
    n += 1

    # ラベル用の文字列を作成
    lambda_str = ', '.join([f'{lmd}' for lmd in lambda_k])
    pi_str     = ', '.join([f'{pi}' for pi in pi_k])
    param_lbl = f'$N = {n}, K = {K}, \\lambda = ({lambda_str}), \\pi = ({pi_str})$'
    
    # サンプルを集計
    cluster_freq_arr[x_n[n-1], s_n[n-1]] += 1 # (簡易集計処理用)

    for k in range(K):

        # サンプルを集計
        #cluster_freq_vec = np.array([np.sum(x_n[s_n[:n] == k] == x) for x in x_vec]) # (随時集計処理用)
        cluster_freq_vec = cluster_freq_arr[:, k] # (簡易集計処理用)
        bottom_freq_vec  = np.sum(cluster_freq_arr[:, :k], axis=1) # 累積度数

        # サンプルの度数を描画
        ax.bar(
            x=x_vec, bottom=bottom_freq_vec, height=cluster_freq_vec, 
            color=cmap(k%color_num), 
            label=f'$k = {k+1}$', 
            zorder=0
        ) # 累積度数
    ax.scatter(
        x=x_n[n-1], y=0.0, 
        color=cmap(s_n[n-1]%color_num), s=50, clip_on=False, 
        zorder=1
    ) # サンプル
    ax.set_xlabel('$x$')
    ax.set_ylabel('frequency')
    ax.set_title(param_lbl, loc='left')
    ax.grid()
    ax.legend(title='cluster', prop={'size': 8}, loc='upper right')
    ax.set_ylim(ymin=0.0, ymax=freq_max) # 描画範囲を固定


# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/mixture_poisson/random_number/freq_1smp.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

##### 相対度数の作図 -----

# 相対度数軸の範囲を設定
relfreq_max = 0.25

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('mixture Poisson distribution', fontsize=20)
ax2 = ax.twinx()

# 度数を初期化
cluster_freq_arr = np.zeros(shape=(len(x_vec), K), dtype='int') # (簡易集計処理用)

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

    # ラベル用の文字列を作成
    lambda_str = ', '.join([f'{lmd}' for lmd in lambda_k])
    pi_str     = ', '.join([f'{pi}' for pi in pi_k])
    param_lbl = f'$N = {n}, K = {K}, \\lambda = ({lambda_str}), \\pi = ({pi_str})$'
    
    # サンプルを集計
    cluster_freq_arr[x_n[n-1], s_n[n-1]] += 1 # (簡易集計処理用)

    for k in range(K):

        # 値を取得
        color_tp = cmap(k%color_num) # 色
        
        # サンプルを集計
        #cluster_freq_vec = np.array([np.sum(x_n[s_n[:n] == k] == x) for x in x_vec]) # (随時集計処理用)
        cluster_freq_vec = cluster_freq_arr[:, k] # (簡易集計処理用)
        bottom_freq_vec  = np.sum(cluster_freq_arr[:, :k], axis=1) # 累積度数

        weighted_prob_vec = weighted_prob_arr[:, k]
        bottom_prob_vec   = np.sum(weighted_prob_arr[:, :k], axis=1)

        # サンプルの相対度数を描画
        ax.bar(
            x=x_vec, bottom=bottom_prob_vec, height=weighted_prob_vec, 
            facecolor='none', edgecolor=color_tp, linewidth=1.0, linestyle='--', 
            zorder=0
        ) # 累積確率
        ax.bar(
            x=x_vec, bottom=bottom_freq_vec/n, height=cluster_freq_vec/n, 
            color=color_tp, alpha=0.5, 
            label=f'$k = {k+1}$', 
            zorder=1
        ) # 累積度数
    ax.scatter(
        x=x_n[n-1], y=0.0, 
        color=cmap(s_n[n-1]%color_num), s=50, clip_on=False, 
        zorder=2
    ) # サンプル
    ax.set_xlabel('$x$')
    ax.set_ylabel('relative frequency, probability')
    ax.set_title(param_lbl, loc='left')
    ax.grid()
    ax.legend(title='cluster', prop={'size': 8}, loc='upper right')
    ax.set_ylim(ymin=0.0, ymax=relfreq_max) # (目盛の共通化用)

    # 度数軸を設定
    freq_max     = relfreq_max * n
    relfreq_vals = ax.get_yticks()  # 相対度数目盛を取得
    freq_vals    = relfreq_vals * n # 度数目盛に変換

    # 2軸を描画
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
    filename='../figure/mixture_poisson/random_number/relfreq_1smp.mp4', 
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

# 度数軸の範囲を設定
u = 5.0
_, counts = np.unique(ar=x_n[:(smp_per_frame*frame_num)], return_counts=True) # 対象を抽出して集計
freq_max = np.max(counts)
freq_max = np.ceil(freq_max /u)*u # u単位で切り上げ
print('frequency:', freq_max)

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('mixture Poisson distribution', fontsize=20)

# 度数を初期化
cluster_freq_arr = np.zeros(shape=(len(x_vec), K), dtype='int') # (簡易集計処理用)

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(n):

    # 前フレームのグラフを初期化
    ax.cla()

    # 集計対象を抽出
    tmp_x_n = x_n[smp_per_frame*n:smp_per_frame*(n+1)] # (簡易集計処理用)
    tmp_s_n = s_n[smp_per_frame*n:smp_per_frame*(n+1)] # (簡易集計処理用)

    # 値を調整
    n = smp_per_frame * (n+1)

    # ラベル用の文字列を作成
    lambda_str = ', '.join([f'{lmd}' for lmd in lambda_k])
    pi_str     = ', '.join([f'{pi}' for pi in pi_k])
    param_lbl = f'$N = {n}, K = {K}, \\lambda = ({lambda_str}), \\pi = ({pi_str})$'
    
    # サンプルを集計
    for x, s in zip(tmp_x_n, tmp_s_n):
         cluster_freq_arr[x, s] += 1 # (簡易集計処理用)

    for k in range(K):

        # サンプルを集計
        #cluster_freq_vec = np.array([np.sum(x_n[s_n[:n] == k] == x) for x in x_vec]) # (随時集計処理用)
        cluster_freq_vec = cluster_freq_arr[:, k] # (簡易集計処理用)
        bottom_freq_vec  = np.sum(cluster_freq_arr[:, :k], axis=1) # 累積度数

        # サンプルの度数を描画
        ax.bar(
            x=x_vec, bottom=bottom_freq_vec, height=cluster_freq_vec, 
            color=cmap(k%color_num), 
            label=f'$k = {k+1}$'
        ) # 累積度数
    ax.set_xlabel('$x$')
    ax.set_ylabel('frequency')
    ax.set_title(param_lbl, loc='left')
    ax.grid()
    ax.legend(title='cluster', prop={'size': 8}, loc='upper right')
    #ax.set_ylim(ymin=0.0, ymax=freq_max) # 描画範囲を固定


# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../figure/mixture_poisson/random_number/freq_nsmp.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%

##### 相対度数の作図 -----

# 相対度数軸の範囲を設定
relfreq_max = 0.25

# 図を初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
fig.suptitle('mixture Poisson distribution', fontsize=20)
ax2 = ax.twinx()

# 度数を初期化
cluster_freq_arr = np.zeros(shape=(len(x_vec), K), dtype='int') # (簡易集計処理用)

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

    # ラベル用の文字列を作成
    lambda_str = ', '.join([f'{lmd}' for lmd in lambda_k])
    pi_str     = ', '.join([f'{pi}' for pi in pi_k])
    param_lbl = f'$N = {n}, K = {K}, \\lambda = ({lambda_str}), \\pi = ({pi_str})$'
    
    # サンプルを集計
    cluster_freq_arr[x_n[n-1], s_n[n-1]] += 1 # (簡易集計処理用)

    for k in range(K):

        # 値を取得
        color_tp = cmap(k%color_num) # 色
        
        # サンプルを集計
        #cluster_freq_vec = np.array([np.sum(x_n[s_n[:n] == k] == x) for x in x_vec]) # (随時集計処理用)
        cluster_freq_vec = cluster_freq_arr[:, k] # (簡易集計処理用)
        bottom_freq_vec  = np.sum(cluster_freq_arr[:, :k], axis=1) # 累積度数

        weighted_prob_vec = weighted_prob_arr[:, k]
        bottom_prob_vec   = np.sum(weighted_prob_arr[:, :k], axis=1)

        # サンプルの相対度数を描画
        ax.bar(
            x=x_vec, bottom=bottom_prob_vec, height=weighted_prob_vec, 
            facecolor='none', edgecolor=color_tp, linewidth=1.0, linestyle='--', 
            zorder=0
        ) # 累積確率
        ax.bar(
            x=x_vec, bottom=bottom_freq_vec/n, height=cluster_freq_vec/n, 
            color=color_tp, alpha=0.5, 
            label=f'$k = {k+1}$', 
            zorder=1
        ) # 累積度数
    ax.scatter(
        x=x_n[n-1], y=0.0, 
        color=cmap(s_n[n-1]%color_num), s=50, clip_on=False, 
        zorder=2
    ) # サンプル
    ax.set_xlabel('$x$')
    ax.set_ylabel('relative frequency, probability')
    ax.set_title(param_lbl, loc='left')
    ax.grid()
    ax.legend(title='cluster', prop={'size': 8}, loc='upper right')
    ax.set_ylim(ymin=0.0, ymax=relfreq_max) # (目盛の共通化用)

    # 度数軸を設定
    freq_max     = relfreq_max * n
    relfreq_vals = ax.get_yticks()  # 相対度数目盛を取得
    freq_vals    = relfreq_vals * n # 度数目盛に変換

    # 2軸を描画
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
    filename='../figure/mixture_poisson/random_number/relfreq_nsmp.mp4', 
    progress_callback=lambda i, n: print(f'frame: {i} / {n}')
)


# %%


