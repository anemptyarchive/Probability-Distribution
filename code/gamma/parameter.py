
# ガンマ分布 --------------------------------------------------------------------

# パラメータの可視化


# %%

# ライブラリを読込
import numpy as np
from scipy.stats import gamma, norm
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from matplotlib import cm
from matplotlib.colors import Normalize


# %%

# パラメータの影響 ---------------------------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num = 100

# フレームごとのパラメータを指定
a_vals = np.linspace(start=0, stop=10, num=frame_num+1)[1:]
b_vals = np.linspace(start=0, stop=10, num=frame_num+1)[1:]

# 固定するパラメータを指定
#a_vals = np.tile(1.0, reps=frame_num)
b_vals = np.tile(1.0, reps=frame_num)
print(a_vals[:5])
print(b_vals[:5])


# %%

### 変数の設定 -----

# λ軸の範囲を設定
k = 1.0
u = 5.0
lambda_min = 0.0
lambda_max  = np.max(a_vals / b_vals) # 期待値の最大値
lambda_max *= k # 定数倍
lambda_max = np.ceil(lambda_max /u)*u # u単位で切り上げ
print('λ size:', lambda_min, lambda_max)

# λ軸の値を作成
lambda_vec = np.linspace(start=lambda_min, stop=lambda_max, num=1001)


# %%

### 分布の計算 -----

# ガンマ分布の確率密度を計算
dens_lt = [
    gamma.pdf(x=lambda_vec, a=a_vals[i], scale=1.0/b_vals[i]) for i in range(frame_num)
]


# %%

### 分布の作図 -----

# 確率密度軸の範囲を設定
k = 0.5
u = 0.05
tmp_arr  = np.array(dens_lt)
dens_max = np.max(tmp_arr[np.isfinite(tmp_arr)]) # Infを除去
dens_max *= k # 定数倍
dens_max = np.ceil(dens_max /u)*u # u単位で切り上げ
print('p(λ) size:', dens_max)


# %%

#### パラメータと形状の関係 -----

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
    
    # 値を取得
    a = a_vals[i] # 形状パラメータ
    b = b_vals[i] # 尺度パラメータ
    dens_vec = dens_lt[i]  # 確率密度

    # ラベル用の文字列を作成
    param_lbl = f'$a = {a:.1f}, b = {b:.1f}$'
    
    # ガンマ分布を描画
    ax.plot(
        lambda_vec, dens_vec, 
        color='#00A968', linewidth=1.0
    ) # 確率密度
    ax.set_xlabel('$\lambda$')
    ax.set_ylabel('density')
    ax.set_title(param_lbl, loc='left')
    ax.grid()
    ax.set_ylim(ymin=0.0, ymax=dens_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/gamma/parameter/parameter.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

#### パラメータと統計量の関係 -----

# ラベルの表示用の余白を設定
y_margin = 0.05
y_min = -dens_max * y_margin
y_max = dens_max * (1.0+y_margin)

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
    
    # 値を取得
    a = a_vals[i] # 形状パラメータ
    b = b_vals[i] # 尺度パラメータ
    dens_vec = dens_lt[i]  # 確率密度

    # 統計量を計算
    mean_x = a / b             # 期待値
    sd_x   = np.sqrt(a / b**2) # 標準偏差
    mode_x = (a-1.0) / b       # 最頻値

    # 標準偏差の範囲を計算
    lambda_pm1sgm_vec = np.linspace(
        start=mean_x-sd_x if mean_x-sd_x > lambda_min else lambda_min, 
        stop =mean_x+sd_x if mean_x+sd_x < lambda_max else lambda_max, 
        num=1001
    ) # 確率変数
    dens_pm1sgm_vec = gamma.pdf(x=lambda_pm1sgm_vec, a=a, scale=1.0/b) # 確率密度

    # ラベル用の文字列を作成
    param_lbl = f'$a = {a:.1f}, b = {b:.1f}$'

    # ガンマ分布を描画
    ax.fill_between(
        x=lambda_pm1sgm_vec, y1=0.0, y2=dens_pm1sgm_vec, 
        color='gray', alpha=0.5, 
        zorder=9
    ) # 標準偏差の範囲
    ax.plot(
        lambda_vec, dens_vec, 
        color='#00A968', linewidth=1.0, 
        zorder=10
    ) # 確率密度
    ax.axvline(
        x=mean_x, 
        color='black', linewidth=1.0, linestyle='--', 
        label=f'$E[x] = \\frac{{a}}{{b}} = {mean_x:.2f}$', 
        zorder=11
    ) # 期待値の位置
    for i, coord_x in enumerate([mean_x-sd_x, mean_x+sd_x]):
        ax.axvline(
            x=coord_x, 
            color='black', linewidth=1.0, linestyle=':', 
            label=f'$\\sqrt{{V[x]}} = \\frac{{a}}{{b^2}} = {sd_x:.2f}$' if i == 0 else None, 
            zorder=11
        ) # 標準偏差の位置
    ax.axvline(
        x=mode_x, 
        color='black', linewidth=1.0, linestyle='-.', 
        label=f'$mode[x] = \\frac{{a-1}}{{b}} = {mode_x:.2f}$', 
        zorder=11
    ) # 最頻値の位置
    ax.hlines(
        y=0.0, xmin=mean_x-sd_x, xmax=mean_x+sd_x, 
        color='black', linewidth=1.0, 
        zorder=11
    ) # 標準偏差の範囲
    ax.text(
        x=mean_x, y=0.0, 
        s='$E[x] \pm \\sqrt{V[x]}$', ha='center', va='top', 
        size=8, 
        zorder=12
    ) # 統計量のラベル
    ax.set_xlabel('$\lambda$')
    ax.set_ylabel('density')
    ax.set_title(param_lbl, loc='left')
    ax.legend(title='statistics', prop={'size': 8}, loc='upper left')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=lambda_min, xmax=lambda_max) # (垂線がはみ出すときの対策用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (ラベルの表示用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/gamma/parameter/stats.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

#### パラメータとモーメントの関係 -----

# ラベルの表示用の余白を設定
y_margin = 0.05
y_min = -dens_max * y_margin
y_max = dens_max * (1.0+y_margin)

# ラベルの表示位置を設定
moment_loc_x = 0.02
moment_loc_y = 0.97

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
    
    # 値を取得
    a = a_vals[i] # 形状パラメータ
    b = b_vals[i] # 尺度パラメータ
    dens_vec = dens_lt[i]  # 確率密度

    # 統計量を計算
    mu    = a / b             # 期待値
    sigma = np.sqrt(a / b**2) # 標準偏差

    # モーメントを計算
    if a > 0.0: # パラメータの条件
        skew  = 2.0 / np.sqrt(a) # 歪度
        kurt  = 6.0 / np.sqrt(a) # 尖度
    else:
        skew = np.inf
        kurt = np.inf

    # ガウス分布の確率密度を計算
    norm_x_vec    = np.linspace(start=lambda_min, stop=lambda_max, num=1001)
    if sigma > 0.0: # パラメータの条件
        norm_dens_vec = norm.pdf(x=norm_x_vec, loc=mu, scale=sigma)
    else:
        norm_dens_vec = np.tile(np.nan, reps=len(norm_x_vec))

    # ラベル用の文字列を作成
    param_lbl  = f'$a = {a:.1f}, b = {b:.1f}, '
    param_lbl  += f'\\mu = {mu:.2f}, \\sigma = {sigma:.2f}$'
    moment_lbl  = f'skewness: {skew:.3f}\n'
    moment_lbl += f'kurtosis:    {kurt:.3f}' # (スペースによる位置調整)

    # ガンマ分布を描画
    ax.axvline(
        x=mu, 
        color='black', linewidth=1.0, linestyle='--', 
        zorder=10
    ) # 期待値の位置
    ax.hlines(
        y=0.0, xmin=mu-sigma, xmax=mu+sigma, 
        color='black', linewidth=1.0, 
        zorder=10
    ) # 標準偏差の範囲
    for label_x, label_str in zip([mu-sigma, mu, mu+sigma], ['$-\sigma$', '$\mu$', '$+\sigma$']):
        ax.text(
            x=label_x, y=0.0, 
            s=label_str, ha='center', va='top', 
            size=10, 
            zorder=11
        ) # 統計量のラベル
    ax.plot(
        lambda_vec, dens_vec, 
        color='#00A968', linewidth=1.0, 
        label='gamma', 
        zorder=12
    ) # 確率密度
    ax.plot(
        norm_x_vec, norm_dens_vec, 
        color='red', linewidth=1.0, linestyle='dashed', 
        label='gaussian', 
        zorder=13
    ) # ガウス分布の確率密度
    ax.text(
        x=moment_loc_x, y=moment_loc_y, 
        s=moment_lbl, transform=ax.transAxes, ha='left', va='top', 
        bbox=dict(facecolor='white', alpha=0.8, edgecolor='black', linewidth=0.5), 
        size = 10, 
        zorder=100
    ) # モーメントのラベル
    ax.set_xlabel('$\lambda$')
    ax.set_ylabel('density')
    ax.set_title(param_lbl, loc='left')
    ax.legend(title='statistics', prop={'size': 8}, loc='upper right')
    ax.grid(zorder=0)
    ax.set_xlim(xmin=lambda_min, xmax=lambda_max) # (垂線がはみ出すときの対策用)
    ax.set_ylim(ymin=y_min, ymax=y_max) # (ラベルの表示用)

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/gamma/parameter/moment.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

# 形状パラメータの影響 -----------------------------------------------------------

### パラメータの設定 -----

# 固定するパラメータを指定
b_vals = np.array(
    [0.1, 0.25, 0.5, 1.0, 1.5, 3.6, 5.0, 7.1, 10.0]
)
print(b_vals)

# グラフ数を設定
param_num = len(b_vals)

# フレーム数を指定
frame_num = 20

# フレームごとのパラメータを指定
a_vals = np.linspace(start=0.0, stop=10.0, num=frame_num+1)[1:]
print(a_vals[:5])


# %%

### 変数の設定 -----

# λ軸の範囲を設定
k = 0.1
u = 5.0
lambda_min = 0.0
lambda_max  = np.max(a_vals[:, None] / b_vals[None, :]) # 期待値の最大値
lambda_max *= k # 定数倍
lambda_max = np.ceil(lambda_max /u)*u # u単位で切り上げ
print('λ size:', lambda_min, lambda_max)

# λ軸の値を作成
lambda_vec = np.linspace(start=lambda_min, stop=lambda_max, num=1001)


# %%

### 分布の計算 -----

# ガンマ分布の確率密度を計算
dens_lt = [
    [gamma.pdf(x=lambda_vec, a=a_vals[i], scale=1.0/b_vals[j]) for j in range(param_num)] for i in range(frame_num)
]


# %%

### 分布の作図 -----

# 確率密度軸の範囲を設定
k = 0.1
u = 0.5
tmp_arr  = np.array(dens_lt) # Infの除去用
dens_max = np.max(tmp_arr[np.isfinite(tmp_arr)]) # Infを除去
dens_max *= k # 定数倍
dens_max = np.ceil(dens_max /u)*u # u単位で切り上げ
print('p(λ) size:', dens_max)

# パラメータ軸の範囲を設定
u = 1.0
a_max = a_vals.max()
a_max = np.ceil(a_max /u)*u # u単位で切り上げ
print('a size:', a_max)


# %%

#### 尺度パラメータの比較 -----

# サブプロット数を設定
col_num = 3 # 列数を指定
row_num = np.ceil(param_num / col_num).astype(np.int32) # 行数を計算

# カラーマップを設定
cmap = cm.viridis # カラーマップを指定
color_norm = Normalize(vmin=0.0, vmax=a_max) # ノーマライザを指定
sm = cm.ScalarMappable(norm=color_norm, cmap=cmap) # 配色用のオブジェクトを作成
sm.set_array([]) # 範囲を初期化

# 図を初期化
fig, axes = plt.subplots(
    nrows=row_num, ncols=col_num, constrained_layout=True, 
    figsize=(12, 8), dpi=100, facecolor='white'
)
fig.suptitle('Gamma distribution', fontsize=20)
fig.supxlabel('$\lambda$')
fig.supylabel('density')
cbar = fig.colorbar(mappable=sm, ax=axes.ravel(), orientation='vertical') # カラーバー
cbar.set_label('$a$')

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes.flatten()]

    for j in range(param_num):

         # サブプロットを取得
        r = j // col_num # 行番号
        c = j % col_num  # 列番号
        ax = axes[r, c]
    
        # 値を取得
        b = b_vals[j] # 尺度パラメータ

        # ラベル用の文字列を作成
        param_lbl = f'$a, b = {b:.1f}$'
        
        # ガンマ分布を描画
        for tmp_i in range(i+1):

            # 値を取得
            a = a_vals[tmp_i] # 形状パラメータ
            dens_vec = dens_lt[tmp_i][j] # 確率密度

            ax.plot(
                lambda_vec, dens_vec, 
                color=cmap(color_norm(a)), linewidth=1.0
            ) # 確率密度
        ax.set_title(param_lbl, loc='left')
        ax.grid()
        ax.set_xlim(xmin=lambda_min, xmax=lambda_max) # 描画範囲を固定
        ax.set_ylim(ymin=0.0, ymax=dens_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/gamma/parameter/shape_parameter.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%

# 尺度パラメータの影響 -----------------------------------------------------------

### パラメータの設定 -----

# 固定するパラメータを指定
a_vals = np.array(
    [0.1, 0.25, 0.5, 1.0, 1.5, 3.6, 5.0, 7.1, 10.0]
)
print(a_vals)

# グラフ数を設定
param_num = len(a_vals)

# フレーム数を指定
frame_num = 20

# フレームごとのパラメータを指定
b_vals = np.linspace(start=0.0, stop=10.0, num=frame_num+1)[1:]
print(b_vals[:5])


# %%

### 変数の設定 -----

# λ軸の範囲を設定
k = 1.0
u = 5.0
lambda_min  = 0.0
lambda_max  = np.max(a_vals[:, None] / b_vals[None, :]) # 期待値の最大値
lambda_max *= k # 定数倍
lambda_max = np.ceil(lambda_max /u)*u # u単位で切り上げ
print('λ size:', lambda_min, lambda_max)

# λ軸の値を作成
lambda_vec = np.linspace(start=lambda_min, stop=lambda_max, num=1001)


# %%

### 分布の計算 -----

# ガンマ分布の確率密度を計算
dens_lt = [
    [gamma.pdf(x=lambda_vec, a=a_vals[j], scale=1.0/b_vals[i]) for j in range(param_num)] for i in range(frame_num)
]


# %%

### 分布の作図 -----

# 確率密度軸の範囲を設定
k = 0.05
u = 0.5
tmp_arr  = np.array(dens_lt) # Infの除去用
dens_max = np.max(tmp_arr[np.isfinite(tmp_arr)]) # Infを除去
dens_max *= k # 定数倍
dens_max = np.ceil(dens_max /u)*u # u単位で切り上げ
print('p(λ) size:', dens_max)

# パラメータ軸の範囲を設定
u = 1.0
b_max = b_vals.max()
b_max = np.ceil(b_max /u)*u # u単位で切り上げ
print('b size:', b_max)


# %%

#### 形状パラメータの比較 -----

# サブプロット数を設定
col_num = 3 # 列数を指定
row_num = np.ceil(param_num / col_num).astype(np.int32) # 行数を計算

# カラーマップを設定
cmap = cm.viridis # カラーマップを指定
color_norm = Normalize(vmin=0.0, vmax=b_max) # ノーマライザを指定
sm = cm.ScalarMappable(norm=color_norm, cmap=cmap) # 配色用のオブジェクトを作成
sm.set_array([]) # 範囲を初期化

# 図を初期化
fig, axes = plt.subplots(
    nrows=row_num, ncols=col_num, constrained_layout=True, 
    figsize=(12, 8), dpi=100, facecolor='white'
)
fig.suptitle('Gamma distribution', fontsize=20)
fig.supxlabel('$\lambda$')
fig.supylabel('density')
cbar = fig.colorbar(mappable=sm, ax=axes.ravel(), orientation='vertical') # カラーバー
cbar.set_label('$b$')

# 初期化処理を定義
def init():
    pass

# 作図処理を定義
def update(i):

    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes.flatten()]

    for j in range(param_num):

         # サブプロットを取得
        r = j // col_num # 行番号
        c = j % col_num  # 列番号
        ax = axes[r, c]
    
        # 値を取得
        a = a_vals[j] # 形状パラメータ

        # ラベル用の文字列を作成
        param_lbl = f'$a = {a:.1f}, b$'
        
        # ガンマ分布を描画
        for tmp_i in range(i+1):

            # 値を取得
            b = b_vals[tmp_i] # 尺度パラメータ
            dens_vec = dens_lt[tmp_i][j] # 確率密度

            ax.plot(
                lambda_vec, dens_vec, 
                color=cmap(color_norm(b)), linewidth=1.0
            ) # 確率密度
        ax.set_title(param_lbl, loc='left')
        ax.grid()
        ax.set_xlim(xmin=lambda_min, xmax=lambda_max) # 描画範囲を固定
        ax.set_ylim(ymin=0.0, ymax=dens_max) # 描画範囲を固定

# 動画を作成
anim = FuncAnimation(
    fig=fig, func=update, init_func=init, 
    frames=frame_num, interval=100
)

# 動画を書出
anim.save(
    filename='../../figure/gamma/parameter/rate_parameter.mp4', 
    progress_callback=lambda i, n: print(f'\rframe: {i+1} / {n}', end='', flush=True)
)


# %%


