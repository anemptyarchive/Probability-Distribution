
# %%

# ライブラリを読込
import numpy as np
from scipy.stats import binom
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# 試行回数を指定
M = 10

# 作図用のxの値を作成
x_vals = np.arange(M + 1)

# 作図用のphiの値を作成
phi_vals = np.arange(start=0.0, stop=1.01, step=0.01)

# 図を初期化
fig = plt.figure(figsize=(12, 8))

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i回目の値を取得
    phi = phi_vals[i]
    
    # 分布を計算
    probability = binom.pmf(k=x_vals, n=M, p=phi)
    
    # 二項分布を作図
    plt.bar(x=x_vals, height=probability, color='#00A968') # 棒グラフ
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('probability') # y軸ラベル
    plt.suptitle('Binomial Distribution', fontsize=20) # 図タイトル
    plt.title('$\phi=' + str(np.round(phi, 2)) + ', M=' + str(M) + '$', loc='left') # タイトル
    plt.xticks(ticks=x_vals) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(-0.1, 1.1) # y軸の表示範囲

# gif画像を作成
anime_prob = FuncAnimation(fig, update, frames=len(phi_vals), interval=100)

# gif画像を保存
anime_prob.save('../figure/binomial/parameter/Binomial_prob_phi.gif')


# %%

# パラメータを指定
phi = 0.3

# 試行回数の最大値を指定
M_max = 100

# 図を初期化
fig = plt.figure(figsize=(12, 8))

# 作図処理を関数として定義
def update(M):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # 作図用のxの値を作成
    x_vals = np.arange(M + 1)
    
    # 分布を計算
    probability = binom.pmf(k=x_vals, n=M, p=phi)
    
    # 二項分布を作図
    plt.bar(x=x_vals, height=probability, color='#00A968') # 棒グラフ
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('probability') # y軸ラベル
    plt.suptitle('Binomial Distribution', fontsize=20) # 図タイトル
    plt.title('$\phi=' + str(np.round(phi, 2)) + ', M=' + str(M) + '$', loc='left') # タイトル
    #plt.xticks(ticks=x_vals) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(-0.1, 1.1) # y軸の表示範囲

# gif画像を作成
anime_prob = FuncAnimation(fig, update, frames=M_max, interval=100)

# gif画像を保存
anime_prob.save('../figure/binomial/parameter/Binomial_prob_M.gif')


# %%


