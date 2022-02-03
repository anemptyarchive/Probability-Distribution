# ガウス-ガンマ分布

# 利用するライブラリ
import numpy as np
from scipy.stats import norm, gamma # 1次元ガウス分布, ガンマ分布
import scipy.special as sp # ガンマ関数, 対数ガンマ関数
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

#%%

### 確率密度の計算

# 1次元ガウス分布の平均パラメータを指定
m = 0.0

# 1次元ガウス分布の精度パラメータの係数を指定
beta = 2.0

# ガンマ分布のパラメータを指定
a = 5.0
b = 6.0

# 確率変数の値を指定
mu = 1.5
lmd = 2.5


# 定義式により確率密度を計算
C_N = 1 / np.sqrt(2.0 * np.pi / beta / lmd)
dens_N = C_N * np.exp(-0.5 * beta * lmd * (mu - m)**2)
C_Gam = b**a / sp.gamma(a)
dens_Gam = C_Gam * lmd**(a - 1.0) * np.exp(-b * lmd)
dens = dens_N * dens_Gam
print(dens)

# 対数をとった定義式により確率密度を計算
log_C_N = -0.5 * (np.log(2.0 * np.pi) - np.log(beta * lmd))
log_dens_N = log_C_N - 0.5 * beta * lmd * (mu - m)**2
log_C_Gam = a * np.log(b) - sp.loggamma(a)
log_dens_Gam = log_C_Gam + (a - 1.0) * np.log(lmd) - b * lmd
log_dens = log_dens_N + log_dens_Gam
dens = np.exp(log_dens)
print(dens)
print(log_dens)

# ガウス分布とガンマ分布の関数により確率密度を計算
dens_N = norm.pdf(x=mu, loc=m, scale=np.sqrt(1.0 / beta / lmd))
dens_Gam = gamma.pdf(x=lmd, a=a, scale=1.0 / b)
dens = dens_N * dens_Gam
print(dens)

# ガウス分布とガンマ分布の対数をとった関数により確率密度を計算
log_dens_N = norm.logpdf(x=mu, loc=m, scale=np.sqrt(1.0 / beta / lmd))
log_dens_Gam <- gamma.logpdf(x=lmd, a=a, scale=1.0 / b)
log_dens = log_dens_N + log_dens_Gam
dens = np.exp(log_dens)
print(dens)
print(log_dens)

#%%

### 統計量の計算

# 1次元ガウス分布の平均パラメータを指定
m = 0.0

# 1次元ガウス分布の精度パラメータの係数を指定
beta = 2.0

# ガンマ分布のパラメータを指定
a = 5.0
b = 6.0


# 計算式により平均を計算
E_mu = m
E_lambda = a / b
print(E_mu)
print(E_lambda)

# 計算式により分散を計算
V_mu = 1.0 / beta / E_lambda
V_lambda = a / b**2
print(V_mu)
print(V_lambda)

# 計算式により最頻値を計算
mode_lambda = (a - 1.0) / b
print(mode_lambda)


# 関数により平均を計算
E_mu = norm.mean(loc=m)
E_lambda = gamma.mean(a=a, scale=1.0 / b)
print(E_mu)
print(E_lambda)

# 関数により分散を計算
V_mu = norm.var(scale=np.sqrt(1.0 / beta / E_lambda))
V_lambda = gamma.var(a=a, scale=1.0 / b)
print(V_mu)
print(V_lambda)

#%%

### 分布の可視化

## 分布の計算

# 1次元ガウス分布の平均パラメータを指定
m = 0.0

# 1次元ガウス分布の精度パラメータの係数を指定
beta = 2.0

# ガンマ分布のパラメータを指定
a = 5.0
b = 6.0


# 作図用のmuの値を作成
mu_vals = np.linspace(start=-3.0, stop=3.0, num=200)

# 作図用のlambdaの値を作成
lambda_vals = np.linspace(start=0.01, stop=2.0, num=200)

# 作図用のmuとlambdaの点を作成
mu_grid, lambda_grid = np.meshgrid(mu_vals, lambda_vals)

# 1次元ガウス分布を計算
density_N = norm.pdf(
    x=mu_grid.flatten(), loc=m, scale=np.sqrt(1.0 / beta / lambda_grid.flatten())
)

# ガンマ分布を計算
density_Gam = gamma.pdf(x=lambda_grid.flatten(), a=a, scale=1.0 / b)

# ガウス-ガンマ分布を計算
density = density_N * density_Gam

#%%

## ガウス-ガンマ分布の作図:(等高線図)

# ガウス-ガンマ分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.contour(mu_grid, lambda_grid, density.reshape(mu_grid.shape)) # 等高線図
plt.xlabel('$\mu$') # x軸ラベル
plt.ylabel('$\lambda$') # y軸のラベル
plt.suptitle('Gaussian-Gamma Distribution', fontsize=20) # 全体のタイトル
plt.title('$m=' + str(m)+ ', \\beta=' + str(beta) + 
          ', a=' + str(a) + ', b=' + str(b) + '$', loc='left') # タイトル
plt.colorbar(label='density') # カラーバー
plt.grid() # グリッド線
plt.show() # 描画

#%%

## ガウス-ガンマ分布の作図:(3Dグラフ)

# ガウス-ガンマ分布を作図
fig = plt.figure(figsize=(9, 8))
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.plot_surface(mu_grid, lambda_grid, density.reshape(mu_grid.shape), cmap='viridis', alpha=0.9) # 曲面図
ax.contour(mu_grid, lambda_grid, density.reshape(mu_grid.shape), offset=0.0) # 等高線図
ax.set_xlabel('$\mu$') # x軸ラベル
ax.set_ylabel('$\lambda$') # y軸ラベル
ax.set_zlabel('density') # z軸ラベル
ax.set_title('$m=' + str(m)+ ', \\beta=' + str(beta) + 
          ', a=' + str(a) + ', b=' + str(b) + '$', loc='left') # タイトル
fig.suptitle('Gaussian-Gamma Distribution', fontsize=20) # 全体のタイトル
plt.show() # 描画

#%%

## 統計量を重ねたガウス-ガンマ分布の作図:(等高線図)

# 統計量を計算
E_mu = m
E_s_mu = np.sqrt(1 / beta / E_lambda)
s_mu_vals = np.sqrt(1.0 / beta / lambda_vals)
E_lambda = a / b
s_lambda = np.sqrt(a / b**2)
mode_lambda = (a - 1.0) / b

# 統計量を重ねたガウス-ガンマ分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.contour(mu_grid, lambda_grid, density.reshape(mu_grid.shape)) # 分布
plt.vlines(x=E_mu, ymin=lambda_vals.min(), ymax=lambda_vals.max(), color='#00A968', linestyle='--', label='$E[\mu]$') # 平均
plt.plot(E_mu-s_mu_vals, lambda_vals, color='#00A968', linestyle=':', label='$E[\mu] \pm \\sqrt{\\frac{1}{\\beta \lambda}}$') # 平均 - 標準偏差
plt.plot(E_mu+s_mu_vals, lambda_vals, color='#00A968', linestyle=':') # 平均 + 標準偏差
plt.hlines(y=E_lambda, xmin=mu_vals.min(), xmax=mu_vals.max(), color='orange', linestyle='--', label='$E[\lambda]$') # 平均
plt.hlines(y=E_lambda-s_lambda, xmin=mu_vals.min(), xmax=mu_vals.max(), color='orange', linestyle=':', label='$E[\mu] \pm \\sqrt{\\frac{1}{V[\lambda]}}$') # 平均 - 標準偏差
plt.hlines(y=E_lambda+s_lambda, xmin=mu_vals.min(), xmax=mu_vals.max(), color='orange', linestyle=':') # 平均 + 標準偏差
plt.hlines(y=mode_lambda, xmin=mu_vals.min(), xmax=mu_vals.max(), color='chocolate', linestyle='--', label='$mode[\mu]$') # 最頻値
plt.xlabel('$\mu$') # x軸ラベル
plt.ylabel('$\lambda$') # y軸のラベル
plt.suptitle('Gaussian-Gamma Distribution', fontsize=20) # 全体のタイトル
plt.title('$m=' + str(m)+ ', \\beta=' + str(beta) + 
          ', a=' + str(a) + ', b=' + str(b) + '$', loc='left') # タイトル
plt.colorbar(label='density') # カラーバー
plt.legend() # 凡例
plt.grid() # グリッド線
plt.xlim(xmin=mu_vals.min(), xmax=mu_vals.max()) # x軸の表示範囲
plt.show() # 描画

#%%

## mu軸側から見たグラフ

# 1次元ガウス分布を計算
density_N = norm.pdf(x=mu_vals, loc=m, scale=np.sqrt(1.0 / beta / E_lambda))

# 統計量を重ねた精度の期待値による1次元ガウス分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(mu_vals, density_N) # 分布
plt.vlines(x=E_mu, ymin=0.0, ymax=density_N.max(), color='#00A968', linestyle='--', label='$E[\mu]$') # 平均
plt.vlines(x=E_mu-E_s_mu, ymin=0.0, ymax=density_N.max(), color='#00A968', linestyle=':', label='$E[\mu] \pm \\sqrt{\\frac{1}{\\beta E[\lambda]}}$') # 平均 + 標準偏差
plt.vlines(x=E_mu+E_s_mu, ymin=0.0, ymax=density_N.max(), color='#00A968', linestyle=':') # 平均 - 標準偏差
plt.xlabel('$\mu$') # x軸ラベル
plt.ylabel('$p(\mu)$') # y軸のラベル
plt.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル
plt.title('$E[\mu]=' + str(E_mu)+ ', \\beta=' + str(beta) + ', E[\lambda]=' + str(np.round(E_lambda, 2)) + '$', loc='left') # タイトル
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

# y軸(確率密度)の最大値を設定
dens_max = np.max(norm.pdf(x=mu_vals, loc=m, scale=np.sqrt(1.0 / beta / lambda_vals.max()))) + 0.05

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
fig.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の平均パラメータを取得
    lmd = lambda_vals[i]
    s_mu = s_mu_vals[i]
    
    # 1次元ガウス分布を計算
    density_N = norm.pdf(x=mu_vals, loc=m, scale=np.sqrt(1.0 / beta / lmd))
    
    # 統計量を重ねた1次元ガウス分布を作図
    plt.plot(mu_vals, density_N) # 分布
    plt.vlines(x=E_mu, ymin=0.0, ymax=dens_max, color='#00A968', linestyle='--', label='$E[\mu]$') # 平均
    plt.vlines(x=E_mu-s_mu, ymin=0.0, ymax=dens_max, color='#00A968', linestyle=':', label='$E[\mu] \pm \\sqrt{\\frac{1}{\\beta \lambda}}$') # 平均 - 標準偏差
    plt.vlines(x=E_mu+s_mu, ymin=0.0, ymax=dens_max, color='#00A968', linestyle=':') # 平均 + 標準偏差
    plt.xlabel('$\mu$') # x軸ラベル
    plt.ylabel('$p(\lambda)$') # y軸ラベル
    plt.title('$E[\mu]=' + str(E_mu)+ ', \\beta=' + str(beta) + ', \lambda=' + str(np.round(lmd, 2)) + '$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.xlim(xmin=mu_vals.min(), xmax=mu_vals.max()) # x軸の表示範囲
    plt.ylim(ymin=-0.05, ymax=dens_max) # y軸の表示範囲

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=len(lambda_vals), interval=100)

# gif画像を保存
anime_dens.save('ProbabilityDistribution/Gaussian_dens_N.gif')

#%%

## lambda軸側から見たグラフ

# ガンマ分布を計算
density_Gam = gamma.pdf(x=lambda_vals, a=a, scale=1.0 / b)

# 統計量を重ねたガンマ分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(lambda_vals, density_Gam) # 分布
plt.vlines(x=E_lambda, ymin=0.0, ymax=density_Gam.max(), color='orange', linestyle='--', label='$E[\lambda]$') # 平均
plt.vlines(x=E_lambda-s_lambda, ymin=0.0, ymax=density_Gam.max(), color='orange', linestyle=':', label='$E[\lambda] \pm \\sqrt{\\frac{1}{V[\lambda]}}$') # 平均 - 標準偏差
plt.vlines(x=E_lambda+s_lambda, ymin=0.0, ymax=density_Gam.max(), color='orange', linestyle=':') # 平均 + 標準偏差
plt.vlines(x=mode_lambda, ymin=0.0, ymax=density_Gam.max(), color='chocolate', linestyle='--', label='$mode[\lambda]$') # 最頻値
plt.xlabel('$\lambda$') # x軸ラベル
plt.ylabel('$p(\lambda)$') # y軸のラベル
plt.suptitle('Gamma Distribution', fontsize=20) # 全体のタイトル
plt.title('$a=' + str(a)+ ', b=' + str(b) + '$', loc='left') # タイトル
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

### パラメータと分布の形状の関係

## オブジェクトの初期化

# パラメータとして利用する値を指定
m_vals = np.arange(start=-2.0, stop=2.1, step=0.1)
beta_vals = np.arange(start=0.1, stop=10.1, step=0.1)
a_vals = np.arange(start=0.1, stop=10.1, step=0.1)
b_vals = np.arange(start=0.1, stop=10.1, step=0.1)

# 固定するパラメータを指定
m = 0.0
beta = 2.0
a = 5.0
b = 6.0

# 作図用の変数の値を作成
mu_vals = np.linspace(start=-3.0, stop=3.0, num=100)
lambda_vals = np.linspace(start=0.01, stop=2.0, num=100)

# 作図用の変数の点を作成
mu_grid, lambda_grid = np.meshgrid(mu_vals, lambda_vals)

#%%

## ガウス-ガンマ分布のアニメーション:(等高線図)

# フレーム数を設定
#frame_num = len(m_vals)
#frame_num = len(beta_vals)
#frame_num = len(a_vals)
frame_num = len(b_vals)
print(frame_num)

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
fig.suptitle('Gaussian-Gamma  Distribution', fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のパラメータを取得
    #m = m_vals[i]
    #beta = beta_vals[i]
    #a = a_vals[i]
    b = b_vals[i]
    
    # ガウス-ガンマ分布を計算
    density = norm.pdf(
        x=mu_grid.flatten(), loc=m, scale=np.sqrt(1.0 / beta / lambda_grid.flatten())
    )
    density *= gamma.pdf(x=lambda_grid.flatten(), a=a, scale=1.0 / b)
    
    # ガウス-ガンマ分布を作図
    plt.contour(mu_grid, lambda_grid, density.reshape(mu_grid.shape)) # 等高線図
    plt.xlabel('$\mu$') # x軸ラベル
    plt.ylabel('$\lambda$') # y軸のラベル
    plt.title('$m=' + str(np.round(m, 1)) + 
              ', \\beta=' + str(np.round(beta, 1)) + 
              ', a=' + str(np.round(a, 1)) + 
              ', b=' + str(np.round(b, 1)) + '$', loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('ProbabilityDistribution/GaussianGamma_dens_cntr.gif')

#%%

## ガウス-ガンマ分布のアニメーション:(3Dグラフ)

# パラメータとして利用する値を指定
m_vals = np.arange(start=-2.0, stop=2.1, step=0.1)
beta_vals = np.arange(start=0.1, stop=10.1, step=0.1)
a_vals = np.arange(start=0.1, stop=10.1, step=0.1)
b_vals = np.arange(start=0.1, stop=10.1, step=0.1)

# フレーム数を設定
#frame_num = len(m_vals)
#frame_num = len(beta_vals)
#frame_num = len(a_vals)
frame_num = len(b_vals)
print(frame_num)

# z軸(確率密度)の最大値を設定
dens_max = 0.9

# 図を初期化
fig = plt.figure(figsize=(9, 8)) # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle('Gaussian-Gamma  Distribution', fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のパラメータを取得
    #m = m_vals[i]
    #beta = beta_vals[i]
    #a = a_vals[i]
    b = b_vals[i]
    
    # ガウス-ガンマ分布を計算
    density = norm.pdf(
        x=mu_grid.flatten(), loc=m, scale=np.sqrt(1.0 / beta / lambda_grid.flatten())
    )
    density *= gamma.pdf(x=lambda_grid.flatten(), a=a, scale=1.0 / b)
    
    # ガウス-ガンマ分布を作図
    ax.plot_surface(mu_grid, lambda_grid, density.reshape(mu_grid.shape), cmap='viridis', alpha=0.9) # 曲面図
    ax.contour(mu_grid, lambda_grid, density.reshape(mu_grid.shape), offset=0.0) # 等高線図
    ax.set_xlabel('$\mu$') # x軸ラベル
    ax.set_ylabel('$\lambda$') # y軸ラベル
    ax.set_zlabel('density') # z軸ラベル
    plt.title('$m=' + str(np.round(m, 1)) + 
              ', \\beta=' + str(np.round(beta, 1)) + 
              ', a=' + str(np.round(a, 1)) + 
              ', b=' + str(np.round(b, 1)) + '$', loc='left') # タイトル
    ax.set_zlim(zmin=0.0, zmax=dens_max) # z軸の表示範囲

# gif画像を作成
anime_dens = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_dens.save('ProbabilityDistribution/GaussianGamma_dens_srfc.gif')

#%%

### 乱数の生成

## サンプリング

# 1次元ガウス分布の平均パラメータを指定
m = 0.0

# 1次元ガウス分布の精度パラメータの係数を指定
beta = 2.0

# ガンマ分布のパラメータを指定
a = 5.0
b = 6.0

# データ数(サンプルサイズ)を指定
N = 10000

# ガンマ分布に従う乱数を生成
lambda_n = np.random.gamma(shape=a, scale=1.0 / b, size=N)

# ガウス分布に従う乱数を生成
mu_n = np.random.normal(loc=m, scale=np.sqrt(1.0 / beta / lambda_n), size=N)


# 作図用のmuの値を作成
mu_vals = np.linspace(start=-3.0, stop=3.0, num=200)

# 作図用のlambdaの値を作成
lambda_vals = np.linspace(start=0.01, stop=2.0, num=200)

# 作図用のmu, lambdaの点を作成
mu_grid, lambda_grid = np.meshgrid(mu_vals, lambda_vals)

# 1次元ガウス分布を計算
density_N = norm.pdf(
    x=mu_grid.flatten(), loc=m, scale=np.sqrt(1.0 / beta / lambda_grid.flatten())
)

# ガンマ分布を計算
density_Gam = gamma.pdf(x=lambda_grid.flatten(), a=a, scale=1.0 / b)

# ガウス-ガンマ分布を計算
density = density_N * density_Gam

#%%

## 乱数の可視化

# サンプルの散布図を作成
plt.figure(figsize=(12, 9)) # 図の設定
plt.scatter(x=mu_n, y=lambda_n, color='orange') # サンプル
plt.contour(mu_grid, lambda_grid, density.reshape(mu_grid.shape)) # 元の分布
plt.xlabel('$\mu$') # x軸ラベル
plt.ylabel('$\lambda$') # y軸のラベル
plt.suptitle('Gaussian-Gamma Distribution', fontsize=20) # 全体のタイトル
plt.title('$\mu=' + str(m)+ ', \\beta=' + str(beta) + 
          ', a=' + str(a) + ', b=' + str(b) + 
          ', N=' + str(N) + '$', loc='left') # タイトル
plt.colorbar(label='density') # カラーバー
plt.grid() # グリッド線
plt.show() # 描画

#%%

# サンプルのヒストグラムを作成
plt.figure(figsize=(12, 9)) # 図の設定
plt.contour(mu_grid, lambda_grid, density.reshape(mu_grid.shape), cmap='jet', zorder=2) # 元の分布
plt.hist2d(x=mu_n, y=lambda_n, bins=25, cmap='jet', alpha=0.7, zorder=1) # ヒストグラム:(頻度)
plt.xlabel('$\mu$') # x軸ラベル
plt.ylabel('$\lambda$') # y軸のラベル
plt.suptitle('Gaussian-Gamma Distribution', fontsize=20) # 全体のタイトル
plt.title('$\mu=' + str(m)+ ', \\beta=' + str(beta) + 
          ', a=' + str(a) + ', b=' + str(b) + 
          ', N=' + str(N) + '$', loc='left') # タイトル
plt.colorbar(label='frequency') # カラーバー
plt.grid() # グリッド線
plt.show() # 描画

#%%

## アニメーションによる可視化:(1データずつ)

# フレーム数を指定
N_frame = 100

# y軸(頻度)の最大値を設定
freq_max = np.max(
    np.histogram2d(
        x=mu_n[:N_frame], y=lambda_n[:N_frame], 
        range=[[mu_grid.min(), mu_grid.max()], [lambda_grid.min(), lambda_grid.max()]], bins=25
    )[0]
)

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
fig.suptitle('Gaussian-Gamma Distribution', fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # サンプルのヒストグラムを作成
    plt.contour(mu_grid, lambda_grid, density.reshape(mu_grid.shape), cmap='jet', zorder=2) # 元の分布
    plt.hist2d(x=mu_n[:(n+1)], y=lambda_n[:(n+1)], 
               range=[[mu_grid.min(), mu_grid.max()], [lambda_grid.min(), lambda_grid.max()]], bins=25, 
               cmap='jet', vmin=0.0, vmax=freq_max, alpha=0.7, zorder=1) # ヒストグラム
    plt.scatter(x=mu_n[n], y=lambda_n[n], c='orange', s=100, zorder=3) # サンプル
    plt.xlabel('$\mu$') # x軸ラベル
    plt.ylabel('$\lambda$') # y軸のラベル
    plt.title('$\mu=' + str(m)+ ', \\beta=' + str(beta) + 
              ', a=' + str(a) + ', b=' + str(b) + 
              ', N=' + str(n + 1) + '$', loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_freq = FuncAnimation(fig, update, frames=N_frame, interval=100)

# gif画像を保存
anime_freq.save('ProbabilityDistribution/GaussianGamma_freq_each.gif')

#%%

## アニメーションによる可視化:(全データ)

# フレーム数を指定
frame_num = 100

# 1フレーム当たりのデータ数を計算
n_per_frame = N // frame_num

# y軸(頻度)の最大値を設定
freq_max = np.max(
    np.histogram2d(
        x=mu_n, y=lambda_n, 
        range=[[mu_grid.min(), mu_grid.max()], [lambda_grid.min(), lambda_grid.max()]], bins=25
    )[0]
)

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
fig.suptitle('Gaussian-Gamma Distribution', fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # サンプルのヒストグラムを作成
    plt.contour(mu_grid, lambda_grid, density.reshape(mu_grid.shape), cmap='jet', zorder=2) # 元の分布
    plt.hist2d(x=mu_n[:(n+1)*n_per_frame], y=lambda_n[:(n+1)*n_per_frame], 
               range=[[mu_grid.min(), mu_grid.max()], [lambda_grid.min(), lambda_grid.max()]], bins=25, 
               cmap='jet', vmin=0.0, vmax=freq_max, alpha=0.7, zorder=1) # ヒストグラム
    plt.xlabel('$\mu$') # x軸ラベル
    plt.ylabel('$\lambda$') # y軸のラベル
    plt.title('$\mu=' + str(m)+ ', \\beta=' + str(beta) + 
              ', a=' + str(a) + ', b=' + str(b) + 
              ', N=' + str((n + 1)*n_per_frame) + '$', loc='left') # タイトル
    plt.grid() # グリッド線

# gif画像を作成
anime_freq = FuncAnimation(fig, update, frames=frame_num, interval=100)

# gif画像を保存
anime_freq.save('ProbabilityDistribution/GaussianGamma_freq_all.gif')

#%%

### 分布の生成

## パラメータの生成

# 1次元ガウス分布の平均パラメータを指定
m = 0.0

# 1次元ガウス分布の精度パラメータの係数を指定
beta = 2.0

# ガンマ分布のパラメータを指定
a = 5.0
b = 6.0

# サンプルサイズを指定
N = 10

# 精度パラメータを生成
lambda_n = np.random.gamma(shape=a, scale=1.0 / b, size=N)

# 平均パラメータを生成
mu_n = np.random.normal(loc=m, scale=np.sqrt(1.0 / beta / lambda_n), size=N)


# パラメータの期待値を計算
E_mu = m
E_lambda = a / b
E_sigma = np.sqrt(1.0 / E_lambda)

# 作図用のxの点を作成
x_vals = np.linspace(start=E_mu - E_sigma*5.0, stop=E_mu + E_sigma*5.0, num=200)

# 期待値による1次元ガウス分布を計算
E_dens = norm.pdf(x=x_vals, loc=E_mu, scale=E_sigma)

#%%

## 分布の作図

# サンプルによる分布を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(x_vals, E_dens, color='blue', linestyle='--', 
         label='$E[\mu]=' + str(E_mu) + ', E[\lambda]=' + str(np.round(E_lambda, 2)) + '$') # 期待値による分布
for n in range(N):
    tmp_dens = norm.pdf(x=x_vals, loc=mu_n[n], scale=np.sqrt(1.0 / lambda_n[n]))
    plt.plot(x_vals, tmp_dens, alpha=0.5, 
             label='$\mu=' + str(np.round(mu_n[n], 2)) + ', \lambda=' + str(np.round(lambda_n[n], 2)) + '$') # サンプルによる分布
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle('Gaussian Distribution', fontsize=20) # 全体のタイトル
plt.title('$\mu=' + str(m) + ', \\beta=' + str(beta) + 
          ', a=' + str(a) + ', b=' + str(b) + '$', loc='left') # タイトル
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

