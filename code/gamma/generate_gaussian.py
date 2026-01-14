
# ガンマ分布 --------------------------------------------------------------------

# 確率分布の生成
## 1次元ガウス分布の精度パラメータとの関係


# %%

# ライブラリの読込 ---------------------------------------------------------------

# 利用するライブラリ
import numpy as np
from scipy.stats import gamma, norm, poisson # ガンマ分布, 1次元ガウス分布, ポアソン分布
import scipy.special as sp # ガンマ関数, 対数ガンマ関数
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

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
