
# 1次元ガウス分布 ---------------------------------------------------------------

# 乱数の可視化


# %%

# 利用するライブラリ
import numpy as np
from scipy.stats import norm
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# 平均を指定
mu = 1.0

# 標準偏差を指定
sigma = 2.5

# データ数を指定
N = 1000

# ガウス分布に従う乱数を生成
x_n = np.random.normal(loc=mu, scale=sigma, size=N)

# 作図用のxの点を作成
x_vals = np.linspace(mu - sigma*4.0, mu + sigma*4.0, num=250)

# ガウス分布を計算
density = norm.pdf(x=x_vals, loc=mu, scale=sigma)


# %%

# フレーム数を指定
N_frame = 240

# 図を初期化
fig = plt.figure(figsize=(8, 6)) # 図の設定
fig.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル

# y軸(頻度)の最大値を設定
freq_max = np.max(
    np.histogram(a=x_n[:N_frame], bins=30, range=(x_vals.min(), x_vals.max()))[0], 
) + 1.0

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # サンプルのヒストグラムを作成
    plt.hist(x=x_n[:(n+1)], bins=50, range=(x_vals.min(), x_vals.max()), color='#00A968', zorder=1) # ヒストグラム
    plt.scatter(x=x_n[n], y=0.0, color='orange', s=100, zorder=2) # サンプル
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('freqency') # y軸ラベル
    plt.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル
    plt.title('$\mu=' + str(mu) + ', \sigma=' + str(sigma) + 
              ', N=' + str(n + 1) + '$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.ylim(ymin=-1.0, ymax=freq_max) # y軸の表示範囲

# gif画像を作成
anime_freq = FuncAnimation(fig, update, frames=N_frame, interval=100)

# gif画像を保存
anime_freq.save('../figure/gaussian/random_number/freq.gif')


# %%

# フレーム数を指定
N_frame = 240

# 図を初期化
fig = plt.figure(figsize=(8, 6)) # 図の設定
fig.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # サンプルのヒストグラムを作成
    plt.hist(x=x_n[:(n+1)], bins=50, range=(x_vals.min(), x_vals.max()), density=True, color='#00A968', zorder=1) # ヒストグラム
    plt.plot(x_vals, density, color='green', linestyle='--', zorder=2) # 元の分布
    plt.scatter(x=x_n[n], y=0.0, color='orange', s=100, zorder=3) # サンプル
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル
    plt.title('$\mu=' + str(mu) + ', \sigma=' + str(sigma) + 
              ', N=' + str(n + 1) + '$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.ylim(ymin=-0.01, ymax=density.max() + 0.1) # y軸の表示範囲

# gif画像を作成
anime_prop = FuncAnimation(fig, update, frames=N_frame, interval=100)

# gif画像を保存
anime_prop.save('../figure/gaussian/random_number/prop.gif')


# %%


