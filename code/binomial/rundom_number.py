
# %%

# 利用するライブラリ
import numpy as np
from scipy.stats import binom
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# パラメータを指定
phi = 0.3

# 試行回数を指定
M = 10

# データ数を指定
N = 1000

# 二項分布に従う乱数を生成
x_n = np.random.binomial(n=M, p=phi, size=N)


# %%

# 作図用のxの値を作成
x_vals = np.arange(M + 1)

# 分布を計算
probability = binom.pmf(k=x_vals, n=M, p=phi)


# %%

# フレーム数を指定
N = 100

# 図を初期化
fig = plt.figure(figsize=(12, 8))

# 頻度の最大値を取得
y_max = np.max([np.sum(x_n[:N] == m) for m in range(M + 1)])

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # n個の乱数を集計
    frequency = np.array([np.sum(x_n[:(n+1)] == m) for m in range(M + 1)])
    
    # サンプルのヒストグラムを作成
    plt.bar(x=x_vals, height=frequency, color='#00A968', zorder=1) # ヒストグラム
    plt.scatter(x=x_n[n], y=0.0, color='orange', s=100, zorder=2) # サンプル
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('frequency') # y軸ラベル
    plt.suptitle('Binomial Distribution', fontsize=20) # 図タイトル
    plt.title('$\phi=' + str(phi) + ', N=' + str(n) + 
              '=(' + ', '.join([str(f) for f in frequency]) + ')$', loc='left') # タイトル
    plt.xticks(ticks=x_vals) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(-1.0, y_max + 1.0) # y軸の表示範囲

# gif画像を作成
anime_freq = FuncAnimation(fig, update, frames=N, interval=100)

# gif画像を保存
anime_freq.save('../figure/binomial/random_number/Binomial_freq.gif')


# %%

# 図を初期化
fig = plt.figure(figsize=(9, 6))

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # n個の乱数を集計
    frequency = np.array([np.sum(x_n[:(n+1)] == m) for m in range(M + 1)])
    
    # サンプルの構成比を作成
    plt.bar(x=x_vals, height=probability, color='white', edgecolor='green', linestyle='--', zorder=1) # 分布
    plt.bar(x=x_vals, height=frequency / (n + 1), color='#00A968', alpha=0.8, zorder=2) # 構成比
    plt.scatter(x=x_n[n], y=0.0, color='orange', s=100, zorder=3) # サンプル
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('proportion') # y軸ラベル
    plt.suptitle('Binomial Distribution', fontsize=20) # 図タイトル
    plt.title('$\phi=' + str(phi) + ', N=' + str(n) + 
              '=(' + ', '.join([str(f) for f in frequency]) + ')$', loc='left') # タイトル
    plt.xticks(ticks=x_vals) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(-0.1, 1.1) # y軸の表示範囲

# gif画像を作成
anime_prop = FuncAnimation(fig, update, frames=N, interval=100)

# gif画像を保存
anime_prop.save('../figure/binomial/random_number/Binomial_prop.gif')


# %%
