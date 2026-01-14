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
