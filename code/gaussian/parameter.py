
# 1次元ガウス分布 ---------------------------------------------------------------

# パラメータの可視化

# %%

# 利用するライブラリ
import numpy as np
from scipy.stats import norm
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

# 平均として利用する値を指定
mu_vals = np.arange(start=-5.0, stop=5.0, step=0.1)
print(len(mu_vals)) # フレーム数

# 標準偏差を指定
sigma = 1.0

# 作図用のxの点を作成
x_vals = np.linspace(start=np.median(mu_vals) - sigma*4.0, stop=np.median(mu_vals) + sigma*4.0, num=250)

# y軸(確率密度)の最大値を設定
dens_max = np.max(norm.pdf(x=x_vals, loc=0.0, scale=sigma)) + 0.05

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
fig.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の平均パラメータを取得
    mu = mu_vals[i]
    
    # ガウス分布を計算
    dens = norm.pdf(x=x_vals, loc=mu, scale=sigma)
    
    # ガウス分布を作図
    plt.plot(x_vals, dens, color='#00A968') # 折れ線グラフ
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.title('$\mu=' + str(np.round(mu, 1)) + ', \sigma=' + str(sigma) + '$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.ylim(ymin=-0.01, ymax=dens_max) # y軸の表示範囲

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=len(mu_vals), interval=100)

# gif画像を保存
anime_dens.save('../figure/gaussian/parameter/parameter_mu.gif')


# %%

# 標準偏差として利用する値を指定
sigma_vals = np.arange(start=1.0, stop=10.1, step=0.1)
print(len(sigma_vals)) # フレーム数

# 平均を指定
mu = 0.0

# 作図用のxの点を作成
x_vals = np.linspace(start=mu - np.max(sigma_vals)*2.0, stop=mu + np.max(sigma_vals)*2.0, num=250)

# y軸(確率密度)の最大値を設定
dens_max = np.max(norm.pdf(x=x_vals, loc=mu, scale=np.min(sigma_vals))) + 0.05

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
fig.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の標準偏差パラメータを取得
    sigma = sigma_vals[i]
    
    # ガウス分布を計算
    dens = norm.pdf(x=x_vals, loc=mu, scale=sigma)
    
    # ガウス分布を作図
    plt.plot(x_vals, dens, color='#00A968') # 折れ線グラフ
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.title('$\mu=' + str(mu) + ', \sigma=' + str(np.round(sigma, 1)) + '$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.ylim(ymin=-0.01, ymax=dens_max) # y軸の表示範囲

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=len(sigma_vals), interval=100)

# gif画像を保存
anime_dens.save('../figure/gaussian/parameter/parameter_sigma.gif')


# %%


