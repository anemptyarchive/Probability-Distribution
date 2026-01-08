# ベルヌーイ分布

# 利用するライブラリ
import numpy as np
from scipy.stats import bernoulli, binom, multinomial # ベルヌーイ分布, 二項分布, 多項分布
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

#%%

### グラフの作成

# パラメータを指定
phi = 0.3

# 作図用のxの値を作成
x_vals = np.array([0.0, 1.0])

# 分布を計算
probability = np.array([1.0 - phi, phi])

# ベルヌーイ分布を作図
plt.figure(figsize=(9, 8)) # 図の設定
plt.bar(x=x_vals, height=probability, color='#00A968') # 棒グラフ
#plt.vlines(x=E_x, ymin=0.0, ymax=np.max(probability), color='orange', linestyle='--', label='$E[x]$') # 平均
#plt.vlines(x=E_x - V_x, ymin=0.0, ymax=np.max(probability), color='orange', linestyle=':', label='$E[x] - \sqrt{V[x]}$') # 平均 - 標準偏差
#plt.vlines(x=E_x + V_x, ymin=0.0, ymax=np.max(probability), color='orange', linestyle=':', label='$E[x] + \sqrt{V[x]}$') # 平均 + 標準偏差
plt.xlabel('x') # x軸ラベル
plt.ylabel('probability') # y軸ラベル
plt.suptitle('Bernoulli Distribution', fontsize=20) # 図タイトル
plt.title('$\phi=' + str(phi) + '$', loc='left') # タイトル
plt.xticks(ticks=x_vals) # x軸目盛
#plt.legend() # 凡例
plt.grid() # グリッド線
plt.show() # 描画

#%%

### パラメータと分布の形状の関係

# 作図用のphiの値を作成
phi_vals = np.arange(start=0.0, stop=1.01, step=0.01)

# 図を初期化
fig = plt.figure(figsize=(9, 8))

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i回目の値を取得
    phi = phi_vals[i]
    
    # ベルヌーイ分布を作図
    plt.bar(x=[0.0, 1.0], height=[1.0 - phi, phi], color='#00A968') # 棒グラフ
    plt.xlabel('x') # x軸ラベル
    plt.ylabel('probability') # y軸ラベル
    plt.suptitle('Bernoulli Distribution', fontsize=20) # 図タイトル
    plt.title('$\phi=' + str(np.round(phi, 2)) + '$', loc='left') # タイトル
    plt.xticks(ticks=[0, 1]) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(-0.1, 1.1) # y軸の表示範囲

# gif画像を作成
anime_prob = FuncAnimation(fig, update, frames=len(phi_vals), interval=100)

# gif画像を保存
anime_prob.save('ProbabilityDistribution/Bernoulli_prob.gif')

#%%

print('end')

