# ベルヌーイ分布

# 利用するライブラリ
import numpy as np
from scipy.stats import bernoulli, binom, multinomial # ベルヌーイ分布, 二項分布, 多項分布
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

#%%

### 確率の計算

# パラメータを指定
phi = 0.3

# 確率変数の値を指定
x = 1.0

# ベクトルに変換
x_v = np.array([1 - x, x])
phi_v = np.array([1.0 - phi, phi])


# 定義式により確率を計算
prob = phi**x * (1.0 - phi)**(1.0 - x)
print(prob)

# 対数をとった定義式により確率を計算
log_prob = x * np.log(phi) + (1.0 - x) * np.log(1.0 - phi)
prob = np.exp(log_prob)
print(prob, log_prob)

# ベルヌーイ分布の関数により確率を計算
prob = bernoulli.pmf(k=x, p=phi)
print(prob)

# ベルヌーイ分布の対数をとった関数により確率を計算
log_prob = bernoulli.logpmf(k=x, p=phi)
prob = np.exp(log_prob)
print(prob, log_prob)

# 二項分布の関数により確率を計算
prob = binom.pmf(k=x, n=1, p=phi)
print(prob)

# 二項分布の対数をとった関数により確率を計算
log_prob = binom.logpmf(k=x, n=1, p=phi)
prob = np.exp(log_prob)
print(prob, log_prob)

# 多項分布の関数により確率を計算
prob = multinomial.pmf(x=x_v, n=1, p=phi_v)
print(prob)

# 多項分布の対数をとった関数により確率を計算
log_prob = multinomial.logpmf(x=x_v, n=1, p=phi_v)
prob = np.exp(log_prob)
print(prob, log_prob)

# インデックスにより確率を抽出
prob = phi_v[np.int(x)]
print(prob)

#%%

### 統計量の計算

# パラメータを指定
phi = 0.3


# 定義式により平均を計算
E_x = phi
print(E_x)

# ベルヌーイ分布の関数により平均を計算
print(bernoulli.mean(p=phi))

# 二項分布の関数により平均を計算
print(binom.mean(n=1, p=phi))

# 分散を計算
V_x = phi * (1.0 - phi)
print(V_x)

# ベルヌーイ分布の関数により分散を計算
print(bernoulli.var(p=phi))

# 二項分布の関数により分散を計算
print(binom.var(n=1, p=phi))

#%%

### グラフの作成

# パラメータを指定
phi = 0.3

# 作図用のxの値を作成
x_vals = np.array([0.0, 1.0])

# 確率を計算
probability = np.array([1.0 - phi, phi])

# ベルヌーイ分布を作図
plt.figure(figsize=(9, 8)) # 図の設定
plt.bar(x=x_vals, height=probability, color='#00A968') # 棒グラフ
#plt.vlines(x=E_x, ymin=0.0, ymax=np.max(probability), color='orange', linestyle='--', label='$E[x]$') # 平均
#plt.vlines(x=E_x - V_x, ymin=0.0, ymax=np.max(probability), color='orange', linestyle=':', label='$E[x] - \sqrt{V[x]}$') # 平均 - 標準偏差
#plt.vlines(x=E_x + V_x, ymin=0.0, ymax=np.max(probability), color='orange', linestyle=':', label='$E[x] + \sqrt{V[x]}$') # 平均 + 標準偏差
plt.xlabel('x') # x軸ラベル
plt.ylabel('probability') # y軸ラベル
plt.suptitle('Bernoulli Distribution', fontsize=20) # 図タイトル
plt.title('$\phi=' + str(phi) + '$', loc='left') # タイトル
plt.xticks(ticks=x_vals) # x軸目盛
#plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

### パラメータと分布の形状の関係

# 作図用のphiの値を作成
phi_vals = np.arange(start=0.0, stop=1.01, step=0.01)

# 図を初期化
fig = plt.figure(figsize=(9, 8))

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i回目の値を取得
    phi = phi_vals[i]
    
    # ベルヌーイ分布を作図
    plt.bar(x=[0.0, 1.0], height=[1.0 - phi, phi], color='#00A968') # 棒グラフ
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('probability') # y軸ラベル
    plt.suptitle('Bernoulli Distribution', fontsize=20) # 図タイトル
    plt.title('$\phi=' + str(np.round(phi, 2)) + '$', loc='left') # タイトル
    plt.xticks(ticks=[0, 1]) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(-0.1, 1.1) # y軸の表示範囲

# gif画像を作成
anime_prob = FuncAnimation(fig, update, frames=len(phi_vals), interval=100)

# gif画像を保存
anime_prob.save('ProbabilityDistribution/Bernoulli_prob.gif')

#%%

### 乱数の生成

## 乱数の可視化

# パラメータを指定
phi = 0.3

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

