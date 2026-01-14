
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
