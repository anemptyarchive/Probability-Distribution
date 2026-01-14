# ガンマ分布

# 利用するライブラリ
import numpy as np
from scipy.stats import gamma, norm, poisson # ガンマ分布, 1次元ガウス分布, ポアソン分布
import scipy.special as sp # ガンマ関数, 対数ガンマ関数
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

#%%

### 確率密度の計算

# パラメータを指定
a = 2.0
b = 2.0

# 確率変数の値を指定
lmd = 2.0


# 定義式により確率密度を計算
C = b**a / sp.gamma(a)
dens = C * lmd**(a - 1.0) * np.exp(-b * lmd)
print(dens)

# 対数をとった定義式により確率密度を計算
log_C = a * np.log(b) - sp.loggamma(a)
log_dens = log_C + (a - 1.0) * np.log(lmd) - b * lmd
dens = np.exp(log_dens)
print(dens, log_dens)

# ガンマ分布の関数により確率密度を計算
dens = gamma.pdf(x=lmd, a=a, scale=1.0 / b)
print(dens)

# ガンマ分布の対数をとった関数により確率密度を計算
log_dens = gamma.logpdf(x=lmd, a=a, scale=1.0 / b)
dens = np.exp(log_dens)
print(dens, log_dens)

#%%

### 統計量の計算

# パラメータを指定
a = 2.0
b = 2.0


# 計算式により平均を計算
E_lambda = a / b
print(E_lambda)

# 計算式により分散を計算
V_lambda = a / b**2
print(V_lambda)

# 計算式により最頻値を計算
mode_lambda = (a - 1.0) / b
print(mode_lambda)

# ガンマ分布の関数により平均を計算
E_lambda = gamma.mean(a=a, scale=1.0 / b)
print(E_lambda)

# ガンマ分布の関数により分散を計算
V_lambda = gamma.var(a=a, scale=1.0 / b)
print(V_lambda)

#%%

### 分布の可視化

## 分布の計算

# パラメータを指定
a = 2.0
b = 2.0

# 作図用のlambdaの点を作成
lambda_vals = np.linspace(start=0.0, stop=5.0, num=250)

# ガンマ分布を計算
density = gamma.pdf(x=lambda_vals, a=a, scale = 1.0 / b)

#%%

## 分布の作図

# ガンマ分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(lambda_vals, density, color='#00A968') # 折れ線グラフ
plt.xlabel('$\lambda$') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle('Gamma Distribution', fontsize=20) # 全体のタイトル
plt.title('$a=' + str(a) + ', b=' + str(b) + '$', loc='left') # タイトル
plt.grid() # グリッド線
plt.show() # 描画

#%%

# 統計量を計算
E_lmd = a / b
s_lmd = np.sqrt(a / b**2)
mode_lmd = (a - 1.0) / b

# 統計量を重ねたガンマ分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(lambda_vals, density, color='#00A968') # 分布
plt.vlines(x=E_lmd, ymin=0.0, ymax=np.max(density), color='orange', linestyle='--', label='$E[\lambda]$') # 平均
plt.vlines(x=E_lmd - s_lmd, ymin=0.0, ymax=np.max(density), color='orange', linestyle=':', label='$E[\lambda] - \\sqrt{V[\lambda]}$') # 平均 - 標準偏差
plt.vlines(x=E_lmd + s_lmd, ymin=0.0, ymax=np.max(density), color='orange', linestyle=':', label='$E[\lambda] + \\sqrt{V[\lambda]}$') # 平均 + 標準偏差
plt.vlines(x=mode_lmd, ymin=0.0, ymax=np.max(density), color='chocolate', linestyle='--', label='$mode[\lambda]$') # 最頻値
plt.xlabel('$\lambda$') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle('Gamma Distribution', fontsize=20) # 全体のタイトル
plt.title('$a=' + str(a) + ', b=' + str(b) + '$', loc='left') # タイトル
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

### パラメータと分布の形状の関係

## a, bの影響

# パラメータとして利用する値を指定
a_vals = np.arange(start=0.1, stop=10.1, step=0.1)
b_vals = np.arange(start=0.1, stop=10.1, step=0.1)
print(len(a_vals)) # フレーム数

# 固定するパラメータを指定
a = 2.0
b = 2.0

# 作図用のlambdaの点を作成
lambda_vals = np.linspace(start=0.0, stop=5.0, num=250)

# y軸(確率密度)の最大値を設定
dens_max = 4.0

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
fig.suptitle('Gamma Distribution', fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のパラメータを取得
    a = a_vals[i]
    #b = b_vals[i]
    
    # ガンマ分布を計算
    density = gamma.pdf(x=lambda_vals, a=a, scale = 1.0 / b)
    
    # ガンマ分布を作図
    plt.plot(lambda_vals, density, color='#00A968') # 折れ線グラフ
    plt.xlabel('$\lambda$') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.title('$a=' + str(np.round(a, 1)) + ', b=' + str(np.round(b, 1)) + '$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.ylim(ymin=-0.1, ymax=dens_max) # y軸の表示範囲

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=len(a_vals), interval=100)

# gif画像を保存
anime_dens.save('ProbabilityDistribution/Gamma_dens.gif')

#%%

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

### 分布の生成

## パラメータの生成

# パラメータを指定
a = 5.0
b = 2.0

# サンプルサイズを指定
N = 10

# ガウス分布・ポアソン分布のパラメータを生成
lambda_n = np.random.gamma(shape=a, scale=1.0 / b, size=N)

#%%

## 分布の作図:(1次元ガウス分布)

# 平均パラメータを指定
mu = 0.0

# 精度パラメータの期待値を計算
E_lambda = a / b

# 標準偏差の期待値を計算
E_sigma = np.sqrt(1.0 / E_lambda)

# 作図用のxの点を作成
x_vals = np.linspace(start=mu - E_sigma*4.0, stop=mu + E_sigma*4.0, num=250)

# 精度パラメータの期待値による1次元ガウス分布を計算
E_dens = norm.pdf(x=x_vals, loc=mu, scale=E_sigma)

# サンプルによる分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(x_vals, E_dens, color='blue', linestyle='--', label='$E[\lambda]=' + str(np.round(E_lambda, 2)) + '$') # 期待値による分布
for n in range(N):
    tmp_dens = norm.pdf(x=x_vals, loc=mu, scale=np.sqrt(1.0 / lambda_n[n]))
    plt.plot(x_vals, tmp_dens, alpha=0.5, label='$\lambda=' + str(np.round(lambda_n[n], 2)) + '$') # サンプルによる分布
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル
plt.title('$a=' + str(a) + ', b=' + str(b) + '$', loc='left') # タイトル
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

## 分布の作図:(ポアソン分布)

# パラメータの期待値を計算
E_lambda = a / b

# 作図用のxの点を作成
x_vals = np.arange(np.ceil(E_lambda) * 4.0)

# パラメータの期待値によるポアソン分布を計算
E_prob = poisson.pmf(k=x_vals, mu=E_lambda)

# サンプルによる分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.step(x=x_vals, y=E_prob, where='mid', 
         color='blue', linestyle='--', label='$E[\lambda]=' + str(np.round(E_lambda, 2)) + '$') # 期待値による分布
for n in range(N):
    tmp_prob = poisson.pmf(k=x_vals, mu=lambda_n[n])
    plt.step(x=x_vals, y=tmp_prob, where='mid', 
             alpha=0.5, label='$\lambda=' + str(np.round(lambda_n[n], 2)) + '$') # サンプルによる分布
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle('Poisson Distribution', fontsize=20) # 全体のタイトル
plt.title('$a=' + str(a) + ', b=' + str(b) + '$', loc='left') # タイトル
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

