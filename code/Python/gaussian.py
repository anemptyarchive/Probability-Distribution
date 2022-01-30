# 1次元ガウス分布の作図

# 利用するライブラリ
import numpy as np
from scipy.stats import norm # 1次元ガウス分布
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

#%%

### 確率密度の計算

# 平均を指定
mu = 1.0

# 標準偏差を指定
sigma = 2.5

# 確率変数の値を指定
x = 1.0


# 定義式により確率密度を計算
C = 1.0 / np.sqrt(2.0 * np.pi * sigma**2)
dens = C * np.exp(-0.5 * (x - mu)**2 / sigma**2)
print(dens)

# 対数をとった定義式により確率密度を計算
log_C = -0.5 * np.log(2.0 * np.pi) - np.log(sigma)
log_dens = log_C - 0.5 * (x - mu)**2 / sigma**2
dens = np.exp(log_dens)
print(dens, log_dens)

# ガウス分布の関数により確率密度を計算
dens = norm.pdf(x=x, loc=mu, scale=sigma)
print(dens)

# ガウス分布の対数をとった関数により確率密度を計算
log_dens = norm.logpdf(x=x, loc=mu, scale=sigma)
dens = np.exp(log_dens)
print(dens, log_dens)

#%%

### 統計量の計算

# 平均を指定
mu = 1.0

# 標準偏差を指定
sigma = 2.5


# 計算式により平均を計算
E_x = mu
print(E_x)

# 計算式により分散を計算
V_x = sigma**2
print(V_x)

# 関数により平均を計算
E_x = norm.mean(loc=mu)
print(E_x)

# 関数により分散を計算
V_x = norm.var(scale=sigma)
print(V_x)

#%%

### 分布の可視化

## 分布の計算

# 平均を指定
mu = 0.0

# 標準偏差を指定
sigma = 1.0

# 作図用のxの点を作成
x_vals = np.linspace(start=mu - sigma*4.0, stop=mu + sigma*4.0, num=250)

# ガウス分布を計算
density = norm.pdf(x=x_vals, loc=mu, scale=sigma)

#%%

## 分布の作図

# ガウス分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(x_vals, density, color='#00A968') # 折れ線グラフ
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル
plt.title('$\mu=' + str(mu) + ', \sigma=' + str(sigma) + '$', loc='left') # タイトル
plt.grid() # グリッド線
plt.show() # 描画

#%%

## 統計量を重ねた分布の作図

# ガウス分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(x_vals, density, color='#00A968') # 分布
plt.vlines(x=mu, ymin=0.0, ymax=np.max(density), color='orange', linestyle='--', label='$\mu$') # 平均
plt.vlines(x=mu - sigma, ymin=0.0, ymax=np.max(density), color='orange', linestyle=':', label='$\mu \pm \\sigma$') # 平均 - 標準偏差
plt.vlines(x=mu + sigma, ymin=0.0, ymax=np.max(density), color='orange', linestyle=':') # 平均 + 標準偏差
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル
plt.title('$\mu=' + str(mu) + ', \sigma=' + str(sigma) + '$', loc='left') # タイトル
plt.grid() # グリッド線
plt.legend() # 凡例
plt.show() # 描画

#%%

### パラメータと分布の形状の関係

## 平均の影響

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
anime_dens.save('ProbabilityDistribution/Gaussian_dens_mu.gif')

#%%

## 標準偏差の影響

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
anime_dens.save('ProbabilityDistribution/Gaussian_dens_sigma.gif')

#%%

### 乱数の生成

## 乱数の可視化

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

#%%

## 乱数の可視化

# サンプルのヒストグラムを作成
plt.figure(figsize=(12, 9)) # 図の設定
plt.hist(x=x_n, bins=50, range=(x_vals.min(), x_vals.max()), color='#00A968') # ヒストグラム
plt.xlabel('x') # x軸ラベル
plt.ylabel('frequency') # y軸ラベル
plt.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル
plt.title('$\mu=' + str(mu) + ', \sigma=' + str(sigma) + ', N=' + str(N) + '$', loc='left') # タイトル
plt.grid() # グリッド線
plt.show() # 描画

# サンプルのヒストグラムを作成
plt.figure(figsize=(12, 9)) # 図の設定
plt.hist(x=x_n, bins=50, range=(x_vals.min(), x_vals.max()), density=True, color='#00A968') # ヒストグラム
plt.plot(x_vals, density, color='green', linestyle='--') # 元の分布
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル
plt.title('$\mu=' + str(mu) + ', \sigma=' + str(sigma) + ', N=' + str(N) + '$', loc='left') # タイトル
plt.grid() # グリッド線
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
    plt.ylim(ymin=-0.5, ymax=freq_max) # y軸の表示範囲

# gif画像を作成
anime_freq = FuncAnimation(fig, update, frames=N_frame, interval=100)

# gif画像を保存
anime_freq.save('ProbabilityDistribution/Gaussian_freq.gif')

#%%

## アニメーションによる可視化:(密度)

# フレーム数を指定
N_frame = 100

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
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
anime_ｐrop = FuncAnimation(fig, update, frames=N_frame, interval=100)

# gif画像を保存
anime_prop.save('ProbabilityDistribution/Gaussian_prop.gif')

#%%

### 分布の生成

## パラメータの生成

# 超パラメータを指定
mu_prior = 1.0
sigma_prior = 2.5

# サンプルサイズを指定
N = 10

# 1次元ガウス分布の平均パラメータを生成
mu_n = np.random.normal(loc=mu_prior, scale=sigma_prior, size=N)


# 標準偏差パラメータを指定
sigma = 1.0

# 平均パラメータを計算
E_mu = mu_prior

# 作図用のxの点を作成
x_vals = np.linspace(E_mu - sigma*5.0, E_mu + sigma*5.0, num=250)

# 平均パラメータの期待値による1次元ガウス分布の確率密度を計算
E_dens = norm.pdf(x=x_vals, loc=E_mu, scale=sigma)

#%%

## 分布の作図

# サンプルによる分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(x_vals, E_dens, color='blue', linestyle='--', label='$E[\mu]=' + str(E_mu) + '$') # 期待値による分布
for n in range(N):
    tmp_dens = norm.pdf(x=x_vals, loc=mu_n[n], scale=sigma)
    plt.plot(x_vals, tmp_dens, alpha=0.5, label='$\mu=' + str(np.round(mu_n[n], 2)) + '$') # サンプルによる分布
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル
plt.title('$\mu_{pri}=' + str(mu_prior) + ', \sigma_{pri}=' + str(sigma_prior) + 
          ', \sigma=' + str(sigma) + ', N=' + str(N) + '$', loc='left') # タイトル
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

