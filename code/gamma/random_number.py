
# ガンマ分布 --------------------------------------------------------------------

# 乱数の可視化


# %%

# ライブラリの読込 ---------------------------------------------------------------

# 利用するライブラリ
import numpy as np
from scipy.stats import gamma, norm, poisson # ガンマ分布, 1次元ガウス分布, ポアソン分布
import scipy.special as sp # ガンマ関数, 対数ガンマ関数
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# %%

### 乱数の生成

## 乱数の生成

# パラメータを指定
a = 2.0
b = 2.0

# データ数(サンプルサイズ)を指定
N = 1000

# ガンマ分布に従う乱数を生成
lambda_n = np.random.gamma(shape=a, scale=1.0 / b, size=N)

# 作図用のlambdaの点を作成
lambda_vals = np.linspace(start=0.0, stop=np.max(lambda_n) + 1.0, num=250)

# ガンマ分布を計算
density = gamma.pdf(x=lambda_vals, a=a, scale=1.0 / b)

#%%

## 乱数の可視化

# サンプルのヒストグラム(頻度)を作成
plt.figure(figsize=(12, 9)) # 図の設定
plt.hist(x=lambda_n, bins=30, range=(lambda_vals.min(), lambda_vals.max()), color='#00A968') # ヒストグラム
plt.xlabel('$\lambda$') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle('Gamma Distribution', fontsize=20) # 全体のタイトル
plt.title('$a=' + str(a) + ', b=' + str(b) + ', N=' + str(N) + '$', loc='left') # タイトル
plt.grid() # グリッド線
plt.ylim(ymin=-0.01) # y軸の表示範囲
plt.show() # 描画

# サンプルのヒストグラム(密度)を作成
plt.figure(figsize=(12, 9)) # 図の設定
plt.hist(x=lambda_n, bins=30, range=(lambda_vals.min(), lambda_vals.max()), density=True, color='#00A968') # ヒストグラム
plt.plot(lambda_vals, density, color='green', linestyle='--') # 元の分布
plt.xlabel('$\lambda$') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle('Gamma Distribution', fontsize=20) # 全体のタイトル
plt.title('$a=' + str(a) + ', b=' + str(b) + ', N=' + str(N) + '$', loc='left') # タイトル
plt.grid() # グリッド線
plt.ylim(ymin=-0.01) # y軸の表示範囲
plt.show() # 描画

#%%

## アニメーションによる可視化:(頻度)

# フレーム数を指定
N_frame = 100

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
fig.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル

# y軸(頻度)の最大値を設定
freq_max = np.max(
    np.histogram(a=lambda_n[:N_frame], bins=30, range=(lambda_vals.min(), lambda_vals.max()))[0], 
) + 1.0

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # サンプルのヒストグラムを作成
    plt.hist(x=lambda_n[:(n+1)], bins=30, range=(lambda_vals.min(), lambda_vals.max()), color='#00A968', zorder=1) # ヒストグラム
    plt.scatter(x=lambda_n[n], y=0.0, s=100, color='orange', zorder=2) # サンプル
    plt.xlabel('$\lambda$') # x軸ラベル
    plt.ylabel('frequency') # y軸ラベル
    plt.suptitle('Gamma Distribution', fontsize=20) # 全体のタイトル
    plt.title('$a=' + str(a) + ', b=' + str(b) + ', N=' + str(n + 1) + '$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.ylim(ymin=-0.5, ymax=freq_max) # y軸の表示範囲

# gif画像を作成
anime_freq = FuncAnimation(fig, update, frames=N_frame, interval=100)

# gif画像を保存
anime_freq.save('ProbabilityDistribution/Gamma_freq.gif')

#%%

## アニメーションによる可視化:(密度)

# フレーム数を指定
N_frame = 100

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
fig.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル

# y軸(確率密度)の最大値を設定
dens_max = np.max(
    np.hstack([
        np.histogram(a=lambda_n[:N_frame], bins=30, range=(lambda_vals.min(), lambda_vals.max()), density=True)[0], 
        density
    ])
) + 0.1

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # サンプルのヒストグラムを作成
    plt.hist(x=lambda_n[:(n+1)], bins=30, range=(lambda_vals.min(), lambda_vals.max()), density=True, color='#00A968', zorder=1) # ヒストグラム
    plt.plot(lambda_vals, density, color='green', linestyle='--', zorder=2) # 元の分布
    plt.scatter(x=lambda_n[n], y=0.0, s=100, color='orange', zorder=3) # サンプル
    plt.xlabel('$\lambda$') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.suptitle('Gamma Distribution', fontsize=20) # 全体のタイトル
    plt.title('$a=' + str(a) + ', b=' + str(b) + ', N=' + str(n + 1) + '$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.ylim(ymin=-0.01, ymax=dens_max) # y軸の表示範囲

# gif画像を作成
anime_freq = FuncAnimation(fig, update, frames=N_frame, interval=100)

# gif画像を保存
anime_freq.save('ProbabilityDistribution/Gamma_prop.gif')

#%%

