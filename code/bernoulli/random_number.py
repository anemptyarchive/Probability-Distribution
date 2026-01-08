# ベルヌーイ分布

# 利用するライブラリ
import numpy as np
from scipy.stats import bernoulli, binom, multinomial # ベルヌーイ分布, 二項分布, 多項分布
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

#%%

### 乱数の生成

## 乱数の可視化

# パラメータを指定
phi = 0.3

# 作図用のxの値を作成
x_vals = np.array([0.0, 1.0])

# 分布を計算
probability = np.array([1.0 - phi, phi])

# データ数を指定
N = 1000

# ベルヌーイ分布に従う乱数を生成
x_n = np.random.binomial(n=1, p=phi, size=N)

# 乱数を集計
frequency = np.array([np.sum(x_n == 0), np.sum(x_n == 1)])

#%%

# サンプルのヒストグラムを作成
plt.figure(figsize=(9, 8)) # 図の設定
plt.bar(x=x_vals, height=frequency, color='#00A968') # ヒストグラム
plt.xlabel('x') # x軸ラベル
plt.ylabel('frequency') # y軸ラベル
plt.suptitle('Bernoulli Distribution', fontsize=20) # 図タイトル
plt.title('$\phi=' + str(phi) + ', N=' + str(N) + 
          '=(' + str(np.sum(x_n == 0)) + ', ' + str(np.sum(x_n == 1)) + ')$', loc='left') # タイトル
plt.xticks(ticks=x_vals) # x軸目盛
plt.grid() # グリッド線
plt.show() # 描画

# サンプルの構成比を作図
plt.figure(figsize=(9, 8)) # 図の設定
plt.bar(x=x_vals, height=probability, color='white', edgecolor='green', linestyle='--') # 分布
plt.bar(x=x_vals, height=frequency / N, color='#00A968', alpha=0.8) # 構成比
plt.xlabel('x') # x軸ラベル
plt.ylabel('proportion') # y軸ラベル
plt.suptitle('Bernoulli Distribution', fontsize=20) # 図タイトル
plt.title('$\phi=' + str(phi) + ', N=' + str(N) + 
          '=(' + str(np.sum(x_n == 0)) + ', ' + str(np.sum(x_n == 1)) + ')$', loc='left') # タイトル
plt.xticks(ticks=x_vals) # x軸目盛
plt.grid() # グリッド線
plt.show() # 描画

#%%

## アニメーションによる可視化

# フレーム数を指定
N = 100

# 図を初期化
fig = plt.figure(figsize=(9, 8))

# 頻度の最大値を取得
y_max = np.max([np.sum(x_n[:N] == 0), np.sum(x_n[:N] == 1)])

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # n個の乱数を集計
    frequency = np.array([np.sum(x_n[:(n+1)] == 0), np.sum(x_n[:(n+1)] == 1)])
    
    # サンプルのヒストグラムを作成
    plt.bar(x=x_vals, height=frequency, color='#00A968', zorder=1) # ヒストグラム
    plt.scatter(x=x_n[n], y=0.0, color='orange', s=100, zorder=2) # サンプル
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('frequency') # y軸ラベル
    plt.suptitle('Bernoulli Distribution', fontsize=20) # 図タイトル
    plt.title('$\phi=' + str(phi) + ', N=' + str(n) + 
              '=(' + str(frequency[0]) + ', ' + str(frequency[1]) + ')$', loc='left') # タイトル
    plt.xticks(ticks=x_vals) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(-1.0, y_max + 1.0) # y軸の表示範囲

# gif画像を作成
anime_hist = FuncAnimation(fig, update, frames=N, interval=100)

# gif画像を保存
anime_hist.save('ProbabilityDistribution/Bernoulli_hist.gif')

#%%

# 図を初期化
fig = plt.figure(figsize=(9, 8))

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # n個の乱数を集計
    frequency = np.array([np.sum(x_n[:(n+1)] == 0), np.sum(x_n[:(n+1)] == 1)])
    
    # サンプルの構成比を作成
    plt.bar(x=x_vals, height=probability, color='white', edgecolor='green', linestyle='--', zorder=1) # 分布
    plt.bar(x=x_vals, height=frequency / (n + 1), color='#00A968', alpha=0.8, zorder=2) # 構成比
    plt.scatter(x=x_n[n], y=0.0, color='orange', s=100, zorder=3) # サンプル
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('proportion') # y軸ラベル
    plt.suptitle('Bernoulli Distribution', fontsize=20) # 図タイトル
    plt.title('$\phi=' + str(phi) + ', N=' + str(n) + 
              '=(' + str(frequency[0]) + ', ' + str(frequency[1]) + ')$', loc='left') # タイトル
    plt.xticks(ticks=x_vals) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(-0.1, 1.1) # y軸の表示範囲

# gif画像を作成
anime_prop = FuncAnimation(fig, update, frames=N, interval=100)

# gif画像を保存
anime_prop.save('ProbabilityDistribution/Bernoulli_prop.gif')

#%%

print('end')

