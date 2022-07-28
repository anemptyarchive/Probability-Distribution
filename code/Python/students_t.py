#%% 1次元スチューデントのt分布

# 利用ライブラリ
import numpy as np
from scipy.stats import t
from scipy.special import loggamma, gamma
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


#%% # 確率密度の計算

### パラメータの設定

# 形状パラメータ(自由度)を指定
nu = 5

# 位置パラメータを指定
mu = 2.0

# スケールパラメータを指定
sigma = 0.5

# 逆スケールパラメータを指定
lmd = 4.0

# 確率変数を指定
x = 1.5

#%%

### 標準化t分布

# 定義式により確率密度を計算
C = gamma(0.5 * (nu + 1)) / gamma(0.5 * nu) / np.sqrt(np.pi * nu)
term = np.sqrt(1.0 + x**2 / nu)**(nu + 1)
dens = C / term
print(dens)

# 対数をとった定義式により確率密度を計算
log_C = loggamma(0.5 * (nu + 1)) - loggamma(0.5 * nu) - 0.5 * np.log(np.pi * nu)
log_term = 0.5 * (nu + 1) * np.log(1.0 + x**2 / nu)
dens = np.exp(log_C - log_term)
print(dens)

# t分布の関数により確率密度を計算
dens = t.pdf(x=x, df=nu)
print(dens)

# t分布の対数をとった関数により確率密度を計算
log_dens = t.logpdf(x=x, df=nu)
dens = np.exp(log_dens)
print(dens)

# 確率密度を計算
dens = t.pdf(x=x, df=nu, loc=0.0, scale=1.0)
print(dens)

#%% 

### 非標準化t分布：スケールパラメータを使用

# sigmaを使った定義式により確率密度を計算
C = gamma(0.5 * (nu + 1)) / gamma(0.5 * nu)
C /= np.sqrt(np.pi * nu) * sigma
term = np.sqrt(1.0 + ((x - mu) / sigma)**2 / nu)**(nu + 1)
dens = C / term
print(dens)

# sigmaを使った定義式(対数)により確率密度を計算
log_C = loggamma(0.5 * (nu + 1)) - loggamma(0.5 * nu)
log_C -= np.log(sigma) + 0.5 * np.log(np.pi * nu)
log_term = 0.5 * (nu + 1) * np.log(1 + ((x - mu) / sigma)**2 / nu)
dens = np.exp(log_C - log_term)
print(dens)

# sigmaを使ってt分布の関数により確率密度を計算
dens = t.pdf(x=x, df=nu, loc=mu, scale=sigma)
print(dens)

# sigmaを使ってt分布の対数をとった関数により確率密度を計算
log_dens = t.logpdf(x=x, df=nu, loc=mu, scale=sigma)
dens = np.exp(log_dens)
print(dens)

# sigmaを使って標準化して確率密度を計算
y = (x - mu) / sigma # 値を変換
dens = t.pdf(x=y, df=nu) / sigma
print(dens)

#%%

### 非標準化t分布：逆スケールパラメータを使用

# lambdaを使った定義式により確率密度を計算
C = gamma(0.5 * (nu + 1)) / gamma(0.5 * nu)
C *= np.sqrt(lmd / np.pi / nu)
term = np.sqrt(1.0 + lmd / nu * (x - mu)**2)**(nu + 1)
dens = C / term
print(dens)

# lambdaを使った定義式(対数)により確率密度を計算
log_C = loggamma(0.5 * (nu + 1)) - loggamma(0.5 * nu)
log_C += 0.5 * (np.log(lmd) - np.log(np.pi * nu))
log_term = 0.5 * (nu + 1) * np.log(1.0 + lmd / nu * (x - mu)**2)
dens = np.exp(log_C - log_term)
print(dens)

# lambdaを使ってt分布の関数により確率密度を計算
dens = t.pdf(x=x, df=nu, loc=mu, scale=1.0/np.sqrt(lmd))
print(dens)

# lambdaを使ってt分布の対数をとった関数により確率密度を計算
log_dens = t.logpdf(x=x, df=nu, loc=mu, scale=1.0/np.sqrt(lmd))
dens = np.exp(log_dens)
print(dens)

# lambdaを使って標準化して確率密度を計算
y = (x - mu) * np.sqrt(lmd) # 値を変換
dens = t.pdf(x=y, df=nu) * np.sqrt(lmd)
print(dens)


# %% # 統計量の計算

# 形状パラメータ(自由度)を指定
nu = 5

# 位置パラメータを指定
mu = 2.0

# スケールパラメータを指定
sigma = 0.5

# 逆スケールパラメータを指定
lmd = 4.0


# 計算式により期待値を計算:(nu > 1)
E_x = mu
print(E_x)

# sigmaを使って計算式により分散を計算:(nu > 2)
V_x = sigma**2 * nu / (nu - 2.0)
print(V_x)

# lambdaを使って計算式により分散を計算:(nu > 2)
V_x = nu / (nu - 2.0) / lmd
print(V_x)

# 計算式により最頻値を計算
mode_x = mu
print(mode_x)


# メソッドにより期待値を計算:(nu > 1)
E_x = t.mean(df=nu, loc=mu, scale=sigma)
print(E_x)

# メソッドにより分散を計算:(nu > 2)
V_x = t.var(df=nu, loc=mu, scale=sigma)
print(V_x)


# %% # グラフの作図

### パラメータの設定

# 形状パラメータ(自由度)を指定
nu = 5

# 位置パラメータを指定
mu = 2.0

# 尺度パラメータを指定
sigma = 0.5

# xの値を作成
x_vals = np.linspace(start=mu-sigma*5, stop=mu+sigma*5, num=251)

# スチューデントのt分布を計算
density = t.pdf(x=x_vals, df=nu, loc=mu, scale=sigma)

#%%

### 作図

# スチューデントのt分布のグラフを作成
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(x_vals, density, color='#00A968', linewidth=2.5) # 折れ線グラフ
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル
plt.title('$\\nu='+str(nu) + ', \mu='+str(mu) + ', \sigma='+str(sigma)+'$', loc='left') # タイトル
plt.grid() # グリッド線
plt.show() # 描画

#%%

### 統計量の作図

# 補助線用の統計量を計算
E_x = mu
s_x = np.sqrt(sigma**2 * nu / (nu - 2))

# 統計量を重ねたt分布のグラフを作成
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(x_vals, density, color='#00A968', linewidth=2.5, label='$p(x | \\nu, \mu, \sigma)$') # 分布
plt.axvline(x=E_x, color='blue', linewidth=2.5, linestyle='--', label='$E[x]$') # 期待値
plt.axvline(x=E_x-s_x, color='orange', linewidth=2.5, linestyle=':', label='$E[x]\pm \sqrt{V[x]}$') # 期待値 - 標準偏差
plt.axvline(x=E_x+s_x, color='orange', linewidth=2.5, linestyle=':') # 期待値 + 標準偏差
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル
plt.title('$\\nu='+str(nu) + ', \mu='+str(mu) + ', \lambda='+str(lmd)+'$', loc='left') # タイトル
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画


#%% # パラメータと分布の関係を並べて比較

### 自由度の影響

# 自由度として利用する値を指定
nu_vals = np.array([1, 2, 3, 4, 5, 10, 100])

# 固定するパラメータを指定
mu = 0.0
sigma = 1.0

# xの値を作成
x_vals = np.linspace(start=mu-sigma*5, stop=mu+sigma*5, num=251)

# スチューデントのt分布のグラフを作成
plt.figure(figsize=(12, 9)) # 図の設定
for nu in nu_vals:
    density = t.pdf(x=x_vals, df=nu, loc=mu, scale=sigma) # 確率密度を計算
    plt.plot(x_vals, density, label='$\\nu='+str(nu) + ', \mu='+str(mu) + ', \sigma='+str(sigma)+'$') # 折れ線グラフ
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

### 位置パラメータの影響

# 位置パラメータとして利用する値を指定
mu_vals = np.array([-3.5, -2.0, -0.5, 1.0, 2.5])

# 固定するパラメータを指定
nu = 1
sigma = 1.0

# xの値を作成
x_vals = np.linspace(start=mu_vals.min()-sigma, stop=mu_vals.max()+sigma, num=251)

# スチューデントのt分布のグラフを作成
plt.figure(figsize=(12, 9)) # 図の設定
for mu in mu_vals:
    density = t.pdf(x=x_vals, df=nu, loc=mu, scale=sigma) # 確率密度を計算
    plt.plot(x_vals, density, label='$\\nu='+str(nu) + ', \mu='+str(mu) + ', \sigma='+str(sigma)+'$') # 折れ線グラフ
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

### スケールパラメータの影響

# スケールパラメータとして利用する値を指定
sigma_vals = np.array([0.5, 1.0, 2.0, 4.0, 8.0])

# 固定するパラメータを指定
nu = 1
mu = 0.0

# xの値を作成
x_vals = np.linspace(start=mu-sigma_vals.max(), stop=mu+sigma_vals.max(), num=251)

# スチューデントのt分布のグラフを作成
plt.figure(figsize=(12, 9)) # 図の設定
for sigma in sigma_vals:
    density = t.pdf(x=x_vals, df=nu, loc=mu, scale=sigma) # 確率密度を計算
    plt.plot(x_vals, density, label='$\\nu='+str(nu) + ', \mu='+str(mu) + ', \sigma='+str(sigma)+'$') # 折れ線グラフ
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

### 逆スケールパラメータの影響

# 逆スケールパラメータとして利用する値を指定
lambda_vals = np.array([0.25, 0.5, 1.0, 2.0, 4.0])

# 固定するパラメータを指定
nu = 1
mu = 0.0

# xの値を作成
sigma = 1.0 / np.sqrt(lambda_vals.min())
x_vals = np.linspace(start=mu-sigma*4, stop=mu+sigma*4, num=251)

# スチューデントのt分布のグラフを作成
plt.figure(figsize=(12, 9)) # 図の設定
for lmd in lambda_vals:
    density = t.pdf(x=x_vals, df=nu, loc=mu, scale=1.0/np.sqrt(lmd)) # 確率密度を計算
    plt.plot(x_vals, density, label='$\\nu='+str(nu) + ', \mu='+str(mu) + ', \lambda='+str(lmd)+'$') # 折れ線グラフ
plt.xlabel('x') # x軸ラベル
plt.ylabel('density') # y軸ラベル
plt.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル
plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画


#%% # パラメータと分布の関係をアニメーションで可視化

### 自由度の影響

# 自由度として利用する値を指定
nu_vals = np.arange(1, 31)
print(len(nu_vals)) # フレーム数

# 固定するパラメータを指定
mu = 0.0
sigma = 1.0

# xの値を作成
x_vals = np.linspace(start=mu-sigma*5, stop=mu+sigma*5, num=251)

# 確率密度(y軸)の最大値を計算
dens_max = t.pdf(x=x_vals, df=nu_vals.max(), loc=mu, scale=sigma).max()

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の値を取得
    nu = nu_vals[i]
    
    # スチューデントのt分布を計算
    density = t.pdf(x=x_vals, df=nu, loc=mu, scale=sigma)
    
    # スチューデントのt分布を作図
    plt.plot(x_vals, density, color='#00A968', linewidth=2.5) # 折れ線グラフ
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.title('$\\nu='+str(np.round(nu, 2)) + ', \mu='+str(mu) + ', \sigma='+str(sigma)+'$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.ylim(-0.01, dens_max*1.1) # y軸の表示範囲

# gif画像を作成
dens_anime = FuncAnimation(fig, func=update, frames=len(nu_vals), interval=100)

# gif画像を保存
dens_anime.save('../../figure/Python/t_dens_nu.gif')

#%% 

### 位置パラメータの影響

# 位置パラメータとして利用する値を指定
mu_vals = np.arange(-3.0, 3.1, 0.1)
print(len(mu_vals)) # フレーム数

# 固定するパラメータを指定
nu = 1
sigma = 1.0

# xの値を作成
x_vals = np.linspace(start=mu_vals.min()-sigma, stop=mu_vals.max()+sigma, num=251)

# 確率密度(y軸)の最大値を計算
dens_max = t.pdf(x=x_vals, df=nu, loc=np.median(mu_vals), scale=sigma).max()

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の値を取得
    mu = mu_vals[i]
    
    # スチューデントのt分布を計算
    density = t.pdf(x=x_vals, df=nu, loc=mu, scale=sigma)
    
    # スチューデントのt分布を作図
    plt.plot(x_vals, density, color='#00A968', linewidth=2.5) # 折れ線グラフ
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.title('$\\nu='+str(nu) + ', \mu='+str(np.round(mu, 2)) + ', \sigma='+str(sigma)+'$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.ylim(-0.01, dens_max*1.1) # y軸の表示範囲

# gif画像を作成
dens_anime = FuncAnimation(fig, func=update, frames=len(mu_vals), interval=100)

# gif画像を保存
dens_anime.save('../../figure/Python/t_dens_mu.gif')

# %%

### スケールパラメータの影響

# スケールパラメータとして利用する値を指定
sigma_vals = np.arange(0.5, 3.1, 0.1)
print(len(sigma_vals)) # フレーム数

# 固定するパラメータを指定
nu = 1
mu = 0.0

# xの値を作成
x_vals = np.linspace(start=mu-sigma_vals.max()*2, stop=mu+sigma_vals.max()*2, num=251)

# 確率密度(y軸)の最大値を計算
dens_max = t.pdf(x=x_vals, df=nu, loc=mu, scale=sigma_vals.min()).max()

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の値を取得
    sigma = sigma_vals[i]
    
    # スチューデントのt分布を計算
    density = t.pdf(x=x_vals, df=nu, loc=mu, scale=sigma)
    
    # スチューデントのt分布を作図
    plt.plot(x_vals, density, color='#00A968', linewidth=2.5) # 折れ線グラフ
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.title('$\\nu='+str(nu) + ', \mu='+str(mu) + ', \sigma='+str(np.round(sigma, 2))+'$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.ylim(-0.01, dens_max*1.1) # y軸の表示範囲

# gif画像を作成
dens_anime = FuncAnimation(fig, func=update, frames=len(sigma_vals), interval=100)

# gif画像を保存
dens_anime.save('../../figure/Python/t_dens_sigma.gif')

# %%

### 逆スケールパラメータの影響

# 逆スケールパラメータとして利用する値を指定
lambda_vals = np.arange(0.1, 3.1, 0.1)
print(len(lambda_vals)) # フレーム数

# 固定するパラメータを指定
nu = 1
mu = 0.0

# xの値を作成
sigma = 1.0 / np.sqrt(lambda_vals.min())
x_vals = np.linspace(start=mu-sigma*2, stop=mu+sigma*2, num=251)

# 確率密度(y軸)の最大値を計算
dens_max = t.pdf(x=x_vals, df=nu, loc=mu, scale=1.0/np.sqrt(lambda_vals.max())).max()

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の値を取得
    lmd = lambda_vals[i]
    
    # スチューデントのt分布を計算
    density = t.pdf(x=x_vals, df=nu, loc=mu, scale=1.0/np.sqrt(lmd))
    
    # スチューデントのt分布を作図
    plt.plot(x_vals, density, color='#00A968', linewidth=2.5) # 折れ線グラフ
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.title('$\\nu='+str(nu) + ', \mu='+str(mu) + ', \lambda='+str(np.round(lmd, 2))+'$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.ylim(-0.01, dens_max*1.1) # y軸の表示範囲

# gif画像を作成
dens_anime = FuncAnimation(fig, func=update, frames=len(lambda_vals), interval=100)

# gif画像を保存
dens_anime.save('../../figure/Python/t_dens_lambda.gif')


#%% # パラメータと統計量の関係をアニメーションで可視化

### 自由度の影響

# 自由度として利用する値を指定
nu_vals = np.arange(1, 31)
print(len(nu_vals)) # フレーム数

# 固定するパラメータを指定
mu = 0.0
sigma = 1.0

# xの値を作成
x_vals = np.linspace(start=mu-sigma*5, stop=mu+sigma*5, num=251)

# 確率密度(y軸)の最大値を計算
dens_max = t.pdf(x=x_vals, df=nu_vals.max(), loc=mu, scale=sigma).max()

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の値を取得
    nu = nu_vals[i]
    
    # スチューデントのt分布を計算
    density = t.pdf(x=x_vals, df=nu, loc=mu, scale=sigma)
    
    # スチューデントのt分布を作図
    plt.plot(x_vals, density, color='#00A968', linewidth=2.5, label='$p(x | \\nu, \mu, \sigma)$') # 分布
    if nu > 1:
        plt.axvline(x=mu, color='blue', linewidth=2.5, linestyle='--', label='$E[x]$') # 期待値
    if nu > 2:
        s_x = np.sqrt(sigma**2 * nu / (nu - 2)) # 標準偏差を計算
        plt.axvline(x=mu-s_x, color='orange', linewidth=2.5, linestyle=':', label='$E[x]\pm \sqrt{V[x]}$') # 期待値 - 標準偏差
        plt.axvline(x=mu+s_x, color='orange', linewidth=2.5, linestyle=':') # 期待値 + 標準偏差
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.title('$\\nu='+str(nu) + ', \mu='+str(np.round(mu, 2)) + ', \sigma='+str(sigma)+'$', loc='left') # タイトル
    plt.legend() # 凡例
    plt.grid() # グリッド線
    plt.ylim(-0.01, dens_max*1.1) # y軸の表示範囲

# gif画像を作成
dens_anime = FuncAnimation(fig, func=update, frames=len(nu_vals), interval=100)

# gif画像を保存
dens_anime.save('../../figure/Python/t_stat_nu.gif')

# %%

### 位置パラメータの影響

# 位置パラメータとして利用する値を指定
mu_vals = np.arange(-3.0, 3.1, 0.1)
print(len(mu_vals)) # フレーム数

# 固定するパラメータを指定
nu = 5
sigma = 1.0

# xの値を作成
x_vals = np.linspace(start=mu_vals.min()-sigma, stop=mu_vals.max()+sigma, num=251)

# 確率密度(y軸)の最大値を計算
dens_max = t.pdf(x=x_vals, df=nu, loc=np.median(mu_vals), scale=sigma).max()

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の値を取得
    mu = mu_vals[i]
    
    # スチューデントのt分布を計算
    density = t.pdf(x=x_vals, df=nu, loc=mu, scale=sigma)
    
    # スチューデントのt分布を作図
    plt.plot(x_vals, density, color='#00A968', linewidth=2.5, label='$p(x | \\nu, \mu, \sigma)$') # 分布
    if nu > 1:
        plt.axvline(x=mu, color='blue', linewidth=2.5, linestyle='--', label='$E[x]$') # 期待値
    if nu > 2:
        s_x = np.sqrt(sigma**2 * nu / (nu - 2)) # 標準偏差を計算
        plt.axvline(x=mu-s_x, color='orange', linewidth=2.5, linestyle=':', label='$E[x]\pm \sqrt{V[x]}$') # 期待値 - 標準偏差
        plt.axvline(x=mu+s_x, color='orange', linewidth=2.5, linestyle=':') # 期待値 + 標準偏差
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.title('$\\nu='+str(nu) + ', \mu='+str(np.round(mu, 2)) + ', \sigma='+str(sigma)+'$', loc='left') # タイトル
    plt.legend() # 凡例
    plt.grid() # グリッド線
    plt.ylim(-0.01, dens_max*1.1) # y軸の表示範囲

# gif画像を作成
dens_anime = FuncAnimation(fig, func=update, frames=len(mu_vals), interval=100)

# gif画像を保存
dens_anime.save('../../figure/Python/t_stat_mu.gif')

# %%

### スケールパラメータの影響

# スケールパラメータとして利用する値を指定
sigma_vals = np.arange(0.5, 3.1, 0.1)
print(len(sigma_vals)) # フレーム数

# 固定するパラメータを指定
nu = 5
mu = 0.0

# xの値を作成
s_x = np.sqrt(sigma_vals.max()**2 * nu / (nu - 2))
x_vals = np.linspace(start=mu-s_x*2, stop=mu+s_x*2, num=251)

# 確率密度(y軸)の最大値を計算
dens_max = t.pdf(x=x_vals, df=nu, loc=mu, scale=sigma_vals.min()).max()

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の値を取得
    sigma = sigma_vals[i]
    
    # スチューデントのt分布を計算
    density = t.pdf(x=x_vals, df=nu, loc=mu, scale=sigma)
    
    # スチューデントのt分布を作図
    plt.plot(x_vals, density, color='#00A968', linewidth=2.5, label='$p(x | \\nu, \mu, \sigma)$') # 分布
    if nu > 1:
        plt.axvline(x=mu, color='blue', linewidth=2.5, linestyle='--', label='$E[x]$') # 期待値
    if nu > 2:
        s_x = np.sqrt(sigma**2 * nu / (nu - 2)) # 標準偏差を計算
        plt.axvline(x=mu-s_x, color='orange', linewidth=2.5, linestyle=':', label='$E[x]\pm \sqrt{V[x]}$') # 期待値 - 標準偏差
        plt.axvline(x=mu+s_x, color='orange', linewidth=2.5, linestyle=':') # 期待値 + 標準偏差
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.title('$\\nu='+str(nu) + ', \mu='+str(np.round(mu, 2)) + ', \sigma='+str(sigma)+'$', loc='left') # タイトル
    plt.legend() # 凡例
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.title('$\\nu='+str(nu) + ', \mu='+str(mu) + ', \sigma='+str(np.round(sigma, 2))+'$', loc='left') # タイトル
    plt.grid() # グリッド線
    plt.ylim(-0.01, dens_max*1.1) # y軸の表示範囲

# gif画像を作成
dens_anime = FuncAnimation(fig, func=update, frames=len(sigma_vals), interval=100)

# gif画像を保存
dens_anime.save('../../figure/Python/t_stat_sigma.gif')

#%%

### 逆スケールパラメータの影響

# 逆スケールパラメータとして利用する値を指定
lambda_vals = np.arange(0.1, 3.1, 0.1)
print(len(lambda_vals)) # フレーム数

# 固定するパラメータを指定
nu = 5
mu = 0.0

# xの値を作成
sigma = 1.0 / np.sqrt(lambda_vals.min())
x_vals = np.linspace(start=mu-sigma*2, stop=mu+sigma*2, num=251)

# 確率密度(y軸)の最大値を計算
dens_max = t.pdf(x=x_vals, df=nu, loc=mu, scale=1.0/np.sqrt(lambda_vals.max())).max()

# 図を初期化
fig = plt.figure(figsize=(12, 9), facecolor='white') # 図の設定
fig.suptitle("Student's t Distribution", fontsize=20) # 全体のタイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の値を取得
    lmd = lambda_vals[i]
    
    # スチューデントのt分布を計算
    density = t.pdf(x=x_vals, df=nu, loc=mu, scale=1.0/np.sqrt(lmd))
    
    # スチューデントのt分布を作図
    plt.plot(x_vals, density, color='#00A968', linewidth=2.5, label='$p(x | \\nu, \mu, \lambda)$') # 分布
    if nu > 1:
        plt.axvline(x=mu, color='blue', linewidth=2.5, linestyle='--', label='$E[x]$') # 期待値
    if nu > 2:
        s_x = np.sqrt(nu / (nu - 2) / lmd) # 標準偏差を計算
        plt.axvline(x=mu-s_x, color='orange', linewidth=2.5, linestyle=':', label='$E[x]\pm \sqrt{V[x]}$') # 期待値 - 標準偏差
        plt.axvline(x=mu+s_x, color='orange', linewidth=2.5, linestyle=':') # 期待値 + 標準偏差
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('density') # y軸ラベル
    plt.title('$\\nu='+str(nu) + ', \mu='+str(mu) + ', \lambda='+str(np.round(lmd, 2))+'$', loc='left') # タイトル
    plt.legend() # 凡例
    plt.grid() # グリッド線
    plt.ylim(-0.01, dens_max*1.1) # y軸の表示範囲

# gif画像を作成
dens_anime = FuncAnimation(fig, func=update, frames=len(lambda_vals), interval=100)

# gif画像を保存
dens_anime.save('../../figure/Python/t_stat_lambda.gif')


#%%
