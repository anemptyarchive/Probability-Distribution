
# ガンマ分布 --------------------------------------------------------------------

# パラメータの可視化

# %%

# 利用するライブラリ
import numpy as np
from scipy.stats import gamma, norm, poisson # ガンマ分布, 1次元ガウス分布, ポアソン分布
import scipy.special as sp # ガンマ関数, 対数ガンマ関数
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

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
