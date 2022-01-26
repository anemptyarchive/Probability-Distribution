# ポアソン分布

# 利用するライブラリ
import numpy as np
from scipy.stats import poisson # ポアソン分布
from scipy.special import gamma, loggamma # ガンマ関数, 対数ガンマ関数
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

#%%

### 確率の計算

# パラメータを指定
lmd = 4.0

# 確率変数の値を指定
x = 2.0


# 定義式により確率を計算
prob = lmd**x / gamma(x + 1.0) * np.exp(-lmd)
print(prob)

# 対数をとった定義式により確率を計算
log_prob = x * np.log(lmd) - loggamma(x + 1.0) - lmd
prob = np.exp(log_prob)
print(prob, log_prob)

# ポアソン分布の関数により確率を計算
prob = poisson.pmf(k=x, mu=lmd)
print(prob)

# ポアソン分布の対数をとった関数により確率を計算
log_prob = poisson.logpmf(k=x, mu=lmd)
prob = np.exp(log_prob)
print(prob, log_prob)

#%%

### 統計量の計算

# パラメータを指定
lmd = 4.0


# 計算式により平均を計算
E_x = lmd
print(E_x)

# 計算式により分散を計算
V_x = lmd
print(V_x)

# ポアソン分布の関数により平均を計算
E_x = poisson.mean(mu=lmd)
print(E_x)

# ポアソン分布の関数により分散を計算
V_x = poisson.var(mu=lmd)
print(V_x)

#%%

### 分布の可視化

## 分布の計算

# パラメータを指定
lmd = 4.0

# 作図用のxの点を作成
x_vals = np.arange(np.ceil(lmd) * 4.0)

# ポアソン分布を計算
probability = poisson.pmf(k=x_vals, mu=lmd)

#%%

## 分布の作図

# ポアソン分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.bar(x=x_vals, height=probability, color='#00A968') # 棒グラフ
plt.xlabel('x') # x軸ラベル
plt.ylabel('probability') # y軸ラベル
plt.suptitle('Poisson Distribution', fontsize=20) # 全体のタイトル
plt.title('$\lambda=' + str(lmd) + '$', loc='left') # タイトル
plt.xticks(ticks=x_vals) # x軸目盛
plt.grid() # グリッド線
plt.show() # 描画

#%%

# 統計量を計算
E_x = lmd
s_x = np.sqrt(lmd)

# 統計量を重ねたポアソン分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.bar(x=x_vals, height=probability, color='#00A968') # 分布
plt.vlines(x=E_x, ymin=0.0, ymax=probability.max(), color='orange', linewidth=2.5, linestyle='--', label='$E[x]$') # 平均
plt.vlines(x=E_x - s_x, ymin=0.0, ymax=probability.max(), color='orange', linewidth=2.5, linestyle=':', label='$E[x] \pm \\sqrt{V[x]}$') # 平均 - 標準偏差
plt.vlines(x=E_x + s_x, ymin=0.0, ymax=probability.max(), color='orange', linewidth=2.5, linestyle=':') # 平均 + 標準偏差
plt.xlabel('x') # x軸ラベル
plt.ylabel('probability') # y軸ラベル
plt.suptitle('Poisson Distribution', fontsize=20)# 全体のタイトル
plt.title('$\lambda=' + str(lmd) + '$', loc='left') # タイトル
plt.xticks(ticks=x_vals) # x軸目盛
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

### パラメータと分布の形状の関係

# lambdaとして利用する値を指定
lambda_vals = np.arange(start=0.0, stop=10.1, step=0.1)
print(len(lambda_vals)) # フレーム数

# 作図用のxの点を作成
x_vals = np.arange(np.ceil(lambda_vals.max()) * 2.0)

# y軸(確率)の最大値を設定
prob_max = np.max(poisson.pmf(k=x_vals, mu=lambda_vals.min())) + 0.1
#prob_max = 0.5

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
fig.suptitle('Poisson Distribution', fontsize=20)# 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i回目のパラメータを取得
    lmd = lambda_vals[i]
    
    # ポアソン分布を計算
    probability = poisson.pmf(k=x_vals, mu=lmd)
    
    # ポアソン分布を作図
    plt.bar(x=x_vals, height=probability, color='#00A968') # 棒グラフ
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('probability') # y軸ラベル
    plt.title('$\lambda=' + str(np.round(lmd, 1)) + '$', loc='left') # タイトル
    plt.xticks(ticks=x_vals) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(ymin=0.0, ymax=prob_max) # y軸の表示範囲

# gif画像を作成
anime_prob = FuncAnimation(fig, update, frames=len(lambda_vals), interval=100)

# gif画像を保存
anime_prob.save('ProbabilityDistribution/Poisson_prob.gif')

#%%

### 乱数の生成

## 乱数の生成

# パラメータを指定
lmd = 4.0

# データ数(サンプルサイズ)を指定
N = 1000

# ポアソン分布に従う乱数を生成
x_n = np.random.poisson(lam=lmd, size=N)

# 作図用のxの点を作成
x_vals = np.arange(x_n.max() + 5.0)

# 乱数を集計
frequency = np.array([np.sum(x_n == m) for m in x_vals])

# ポアソン分布を計算
probability = poisson.pmf(k=x_vals, mu=lmd)

#%%

## 乱数の可視化

# サンプルのヒストグラムを作成
plt.figure(figsize=(12, 9)) # 図の設定
plt.bar(x=x_vals, height=frequency, color='#00A968') # ヒストグラム
plt.xlabel('x') # x軸ラベル
plt.ylabel('frequency') # y軸ラベル
plt.suptitle('Poisson Distribution', fontsize=20)# 全体のタイトル
plt.title('$\lambda=' + str(lmd) + ', N=' + str(N) + 
          '=(' + ', '.join([str(f) for f in frequency]) + ')$', loc='left') # タイトル
plt.xticks(ticks=x_vals) # x軸目盛
plt.grid() # グリッド線
plt.show() # 描画

#%%

# サンプルの構成比を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.bar(x=x_vals, height=probability, color='white', edgecolor='green', linestyle='--') # 元の分布
plt.bar(x=x_vals, height=frequency / N, color='#00A968', alpha=0.8) # 構成比
plt.xlabel('x') # x軸ラベル
plt.ylabel('proportion') # y軸ラベル
plt.suptitle('Poisson Distribution', fontsize=20)# 全体のタイトル
plt.title('$\lambda=' + str(lmd) + ', N=' + str(N) + 
          '=(' + ', '.join([str(f) for f in frequency]) + ')$', loc='left') # タイトル
plt.xticks(ticks=x_vals) # x軸目盛
plt.grid() # グリッド線
plt.show() # 描画

#%%

## アニメーションによる可視化:(頻度)

# フレーム数を指定
N_frame = 100

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
fig.suptitle('Poisson Distribution', fontsize=20)# 全体のタイトル

# y軸(頻度)の最大値を設定
freq_max = np.max([np.sum(x_n[:N_frame] == m) for m in x_vals]) + 1.0

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # n個の乱数を集計
    frequency = np.array([np.sum(x_n[:(n+1)] == m) for m in x_vals])
    
    # サンプルのヒストグラムを作成
    plt.bar(x=x_vals, height=frequency, color='#00A968', zorder=1) # ヒストグラム
    plt.scatter(x=x_n[n], y=0.0, s=100, c='orange', zorder=2) # サンプル
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('frequency') # y軸ラベル
    plt.title('$\lambda=' + str(lmd) + ', N=' + str(n + 1) + 
              '=(' + ', '.join([str(f) for f in frequency]) + ')$', loc='left') # タイトル
    plt.xticks(ticks=x_vals) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(ymin=-0.5, ymax=freq_max) # y軸の表示範囲

# gif画像を作成
anime_freq = FuncAnimation(fig, update, frames=N_frame, interval=100)

# gif画像を保存
anime_freq.save('ProbabilityDistribution/Poisson_freq.gif')

#%%

## アニメーションによる可視化:(構成比)

# フレーム数を指定
N_frame = 100

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
fig.suptitle('Poisson Distribution', fontsize=20)# 全体のタイトル

# y軸(割合)の最大値を設定
prop_max = np.max([np.sum(x_n[:N_frame] == m) for m in x_vals]) / N_frame + 0.1

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # n個の乱数を集計
    frequency = np.array([np.sum(x_n[:(n+1)] == m) for m in x_vals])
    
    # サンプルのヒストグラムを作成
    plt.bar(x=x_vals, height=probability, color='white', edgecolor='green', linestyle='--', zorder=1) # 元の分布
    plt.bar(x=x_vals, height=frequency / (n + 1), color='#00A968', alpha=0.8, zorder=2) # 構成比
    plt.scatter(x=x_n[n], y=0.0, s=100, c='orange', zorder=3) # サンプル
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('proportion') # y軸ラベル
    plt.title('$\lambda=' + str(lmd) + ', N=' + str(n + 1) + 
              '=(' + ', '.join([str(f) for f in frequency]) + ')$', loc='left') # タイトル
    plt.xticks(ticks=x_vals) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(ymin=-0.01, ymax=prop_max) # y軸の表示範囲

# gif画像を作成
anime_prop = FuncAnimation(fig, update, frames=N_frame, interval=100)

# gif画像を保存
anime_prop.save('ProbabilityDistribution/Poisson_prop.gif')

#%%

