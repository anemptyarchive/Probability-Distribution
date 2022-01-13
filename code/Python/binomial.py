# 二項分布

# 利用するライブラリ
import numpy as np
from scipy.stats import binom, multinomial # 二項分布, 多項分布
from scipy.special import gamma, loggamma # ガンマ関数, 対数ガンマ関数
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

#%%

### 確率の計算

# パラメータを指定
phi = 0.3

# 試行回数を指定
M = 10

# 確率変数の値を指定:(x <= M)
x = 3

# ベクトルに変換
x_v = np.array([M - x, x])
phi_v = np.array([1.0 - phi, phi])


# 定義式により確率を計算
C = gamma(M + 1) / gamma(M - x + 1) / gamma(x + 1)
prob = C * phi**x * (1 - phi)**(M - x)
print(prob)

# 対数をとった定義式により確率を計算
log_C = loggamma(M + 1) - loggamma(M - x + 1) - loggamma(x + 1)
log_prob = log_C + x * np.log(phi) + (M - x) * np.log(1 - phi)
pron = np.exp(log_prob)
print(prob, log_prob)

# 二項分布の関数により確率を計算
prob = binom.pmf(k=x, n=M, p=phi)
print(prob)

# 二項分布の対数をとった関数により確率を計算
log_prob = binom.logpmf(k=x, n=M, p=phi)
prob = np.exp(log_prob)
print(prob, log_prob)

# 多項分布の関数により確率を計算
prob = multinomial.pmf(x=x_v, n=M, p=phi_v)
print(prob)

# 多項分布の対数をとった関数により確率を計算
log_prob = multinomial.logpmf(x=x_v, n=M, p=phi_v)
prob = np.exp(log_prob)
print(prob, log_prob)

#%%

### 統計量の計算

# パラメータを指定
phi = 0.3

# 試行回数を指定
M = 10


# 平均を計算
E_x = M * phi
print(E_x)

# ベルヌーイ分布の関数により平均を計算
print(binom.mean(n=M, p=phi))

# 分散を計算
V_x = M * phi * (1.0 - phi)
print(V_x)

# ベルヌーイ分布の関数により分散を計算
print(binom.var(n=M, p=phi))

#%%

### グラフの作成

# 作図用のxの値を作成
x_vals = np.arange(M + 1)

# 分布を計算
probability = binom.pmf(k=x_vals, n=M, p=phi)

# 二項分布を作図
plt.figure(figsize=(12, 8)) # 図の設定
plt.bar(x=x_vals, height=probability, color='#00A968') # 棒グラフ
#plt.vlines(x=E_x, ymin=0.0, ymax=np.max(probability), color='orange', linestyle='--', label='$E[x]$') # 平均
#plt.vlines(x=E_x - V_x, ymin=0.0, ymax=np.max(probability), color='orange', linestyle=':', label='$E[x] - \sqrt{V[x]}$') # 平均 - 標準偏差
#plt.vlines(x=E_x + V_x, ymin=0.0, ymax=np.max(probability), color='orange', linestyle=':', label='$E[x] + \sqrt{V[x]}$') # 平均 + 標準偏差
plt.xlabel('x') # x軸ラベル
plt.ylabel('probability') # y軸ラベル
plt.suptitle('Binomial Distribution', fontsize=20) # 図タイトル
plt.title('$\phi=' + str(phi) + ', M=' + str(M) + '$', loc='left') # タイトル
plt.xticks(ticks=x_vals) # x軸目盛
#plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

### パラメータと分布の形状の関係

## phiを変更した場合

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
anime_prob.save('ProbabilityDistribution/Binomial_prob_phi.gif')

#%%

## Mを変更した場合

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
anime_prob.save('ProbabilityDistribution/Binomial_prob_M.gif')

#%%

### 乱数の生成

## 乱数の可視化

# パラメータを指定
phi = 0.3

# 試行回数を指定
M = 10

# データ数を指定
N = 1000

# 二項分布に従う乱数を生成
x_n = np.random.binomial(n=M, p=phi, size=N)

# 乱数を集計
frequency = np.array([np.sum(x_n == m) for m in range(M + 1)])

#%%

# サンプルのヒストグラムを作成
plt.figure(figsize=(12, 8)) # 図の設定
plt.bar(x=x_vals, height=frequency, color='#00A968') # ヒストグラム
plt.xlabel('x') # x軸ラベル
plt.ylabel('frequency') # y軸ラベル
plt.suptitle('Binomial Distribution', fontsize=20) # 図タイトル
plt.title('$\phi=' + str(phi) + ', N=' + str(N) + 
          '=(' + ', '.join([str(f) for f in frequency]) + ')$', loc='left') # タイトル
plt.xticks(ticks=x_vals) # x軸目盛
plt.grid() # グリッド線
plt.show() # 描画

# サンプルの構成比を作図
plt.figure(figsize=(12, 8)) # 図の設定
plt.bar(x=x_vals, height=probability, color='white', edgecolor='green', linestyle='--') # 分布
plt.bar(x=x_vals, height=frequency / N, color='#00A968', alpha=0.8) # 構成比
plt.xlabel('x') # x軸ラベル
plt.ylabel('proportion') # y軸ラベル
plt.suptitle('Binomial Distribution', fontsize=20) # 図タイトル
plt.title('$\phi=' + str(phi) + ', N=' + str(N) + 
          '=(' + ', '.join([str(f) for f in frequency]) + ')$', loc='left') # タイトル
plt.xticks(ticks=x_vals) # x軸目盛
plt.grid() # グリッド線
plt.show() # 描画

#%%

## アニメーションによる可視化

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
anime_freq.save('ProbabilityDistribution/Binomial_freq.gif')

#%%

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
anime_prop.save('ProbabilityDistribution/Binomial_prop.gif')

#%%

print('end')

