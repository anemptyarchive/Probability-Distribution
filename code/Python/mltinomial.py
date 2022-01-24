# 多項分布

# 利用するライブラリ
import numpy as np
from scipy.stats import multinomial # 多項分布
from scipy.special import gamma, loggamma # ガンマ関数, 対数ガンマ関数
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

#%%

### 確率の計算

# パラメータを指定
phi_v = np.array([0.3, 0.5, 0.2])

# 確率変数の値を指定
x_v = np.array([2.0, 3.0, 1.0])

# データ数を計算
M = np.sum(x_v)

# 定義式により確率を計算
C = gamma(M + 1.0) / np.prod(gamma(x_v + 1.0))
prob = C * np.prod(phi_v**x_v)
print(prob)

# 対数をとった定義式により確率を計算
log_C = loggamma(M + 1.0) - np.sum(loggamma(x_v + 1.0))
log_prob = log_C + np.sum(x_v * np.log(phi_v))
prob = np.exp(log_prob)
print(prob, log_prob)

# 多項分布の関数により確率を計算
prob = multinomial.pmf(x=x_v, n=M, p=phi_v)
print(prob)

# 多項分布の対数をとった関数により確率を計算
log_prob = multinomial.logpmf(x=x_v, n=M, p=phi_v)
prob = np.exp(log_prob)
print(prob, log_prob)

#%%

### 統計量の計算

# パラメータを指定
phi_v = np.array([0.3, 0.5, 0.2])

# 試行回数を指定
M = 10

# クラス番号を指定
v = 1

# クラスvの平均を計算
E_x = M * phi_v[v]
print(E_x)

# クラスvの分散を計算
V_x = M * phi_v[v] * (1.0 - phi_v[v])
print(V_x)

#%%

### グラフの作成

## 分布の計算

# パラメータを指定
phi_v = np.array([0.3, 0.5, 0.2])

# 試行回数を指定
M = 10

# 作図用のxの値を作成
x_vals = np.arange(M + 1)

# 格子点を作成
X1, X2 = np.meshgrid(x_vals, x_vals)

# 作図用のxの点を作成
x1_vals = X1.flatten()
x2_vals = X2.flatten()
x3_vals = np.where(x1_vals+x2_vals <= M, M - (x1_vals+x2_vals), 0.0)

# 計算用のxの点を作成
x_points = np.stack([x1_vals, x2_vals, x3_vals], axis=1)
print(x_points.shape)

# 分布を計算
probability = multinomial.pmf(x=x_points, n=M, p=phi_v)

#%%

## 行ごとに色付け

# カラーマップを設定
cm = plt.get_cmap('hsv')

# 多項分布を作図
fig = plt.figure(figsize=(12, 9)) # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.bar3d(x=x1_vals - 0.5, y=x2_vals - 0.5, z=np.zeros_like(x1_vals), 
         dx=1.0, dy=1.0, dz=probability, 
         color=cm(x2_vals / M), alpha=0.5, shade=True) # 3D棒グラフ
ax.set_xlabel('$x_1$')
ax.set_ylabel('$x_2$')
ax.set_zlabel('probability')
fig.suptitle('Maltinomial Distribution', fontsize=20)
ax.set_title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + '), M=' + str(M) + '$', loc='left')
#ax.view_init(elev=90, azim=270) # 表示アングル
plt.show()

#%%

# xがとり得ない値の要素をマスク
x2_mask_vals = np.ma.masked_where(condition=x1_vals+x2_vals > M, a=x2_vals)
probability_mask = np.ma.masked_where(condition=x1_vals+x2_vals > M, a=probability)

# カラーマップを設定
cm = plt.get_cmap('hsv')

# 多項分布を作図
fig = plt.figure(figsize=(12, 9)) # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.bar3d(x=x1_vals - 0.5, y=x2_vals - 0.5, z=np.zeros_like(x1_vals), 
         dx=1.0, dy=1.0, dz=probability_mask, 
         color=cm(x2_mask_vals / M), alpha=0.5, shade=True) # 3D棒グラフ
ax.set_xlabel('$x_1$')
ax.set_ylabel('$x_2$')
ax.set_zlabel('probability')
fig.suptitle('Maltinomial Distribution', fontsize=20)
ax.set_title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + '), M=' + str(M) + '$', loc='left')
#ax.view_init(elev=90, azim=270) # 表示アングル
plt.show()

#%%

# xがとり得ない値の要素を削除
x1_del_vals = np.delete(arr=x1_vals, obj=x1_vals+x2_vals > M)
x2_del_vals = np.delete(arr=x2_vals, obj=x1_vals+x2_vals > M)
probability_del = np.delete(arr=probability, obj=x1_vals+x2_vals > M)

# カラーマップを設定
cm = plt.get_cmap('hsv')

# 多項分布を作図
fig = plt.figure(figsize=(12, 9)) # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.bar3d(x=x1_del_vals - 0.5, y=x2_del_vals - 0.5, z=np.zeros_like(x1_del_vals), 
         dx=1.0, dy=1.0, dz=probability_del, 
         color=cm(x2_del_vals / M), alpha=0.5, shade=True) # 3D棒グラフ
ax.set_xlabel('$x_1$')
ax.set_ylabel('$x_2$')
ax.set_zlabel('probability')
fig.suptitle('Maltinomial Distribution', fontsize=20)
ax.set_title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + '), M=' + str(M) + '$', loc='left')
#ax.view_init(elev=90, azim=270) # 表示アングル
plt.show()

#%%

## 確率に応じて色付け

# カラーマップを設定
cm = plt.get_cmap('jet')

# 多項分布を作図
fig = plt.figure(figsize=(12, 9)) # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.bar3d(x=x1_del_vals - 0.5, y=x2_del_vals - 0.5, z=np.zeros_like(x1_del_vals), 
         dx=1.0, dy=1.0, dz=probability_del, 
         color=cm(probability_del / np.max(probability_del)), alpha=0.5, shade=True) # 3D棒グラフ
ax.set_xlabel('$x_1$')
ax.set_ylabel('$x_2$')
ax.set_zlabel('probability')
fig.suptitle('Maltinomial Distribution', fontsize=20)
ax.set_title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + '), M=' + str(M) + '$', loc='left')
#ax.view_init(elev=90, azim=270) # 表示アングル
plt.show()

#%%

## 統計量の情報を表示

# 補助線用の値を作成
vals = np.arange(-1, M + 2)
zeros = np.repeat(0.0, len(vals))

# 補助線用の統計量を計算
E_x1 = M * phi_v[0]
V_x1 = M * phi_v[0] * (1.0 - phi_v[0])
s_x1 = np.sqrt(V_x1)
E_x2 = M * phi_v[1]
V_x2 = M * phi_v[1] * (1.0 - phi_v[1])
s_x2 = np.sqrt(V_x2)

# カラーマップを設定
cm = plt.get_cmap('jet')

# 多項分布を作図
fig = plt.figure(figsize=(12, 9)) # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.bar3d(x=x1_del_vals - 0.5, y=x2_del_vals - 0.5, z=np.zeros_like(x1_del_vals), 
         dx=1.0, dy=1.0, dz=probability_del, 
         color=cm(probability_del / np.max(probability_del)), alpha=0.5, shade=False) # 分布
ax.plot(xs=np.repeat(E_x1, len(vals)), ys=vals, zs=zeros, color='#00A968', linestyle='--', label='$E[x_v]$') # クラス1の平均
ax.plot(xs=np.repeat(E_x1 - s_x1, len(vals)), ys=vals, zs=zeros, color='#00A968', linestyle=':', label='$E[x_v] \pm \sqrt{V[x_v]}$') # クラス1の平均 + 標準偏差
ax.plot(xs=np.repeat(E_x1 + s_x1, len(vals)), ys=vals, zs=zeros, color='#00A968', linestyle=':') # クラス1の平均 + 標準偏差
ax.plot(xs=vals, ys=np.repeat(E_x2, len(vals)), zs=zeros, color='#00A968', linestyle='--') # クラス2の平均
ax.plot(xs=vals, ys=np.repeat(E_x2 - s_x2, len(vals)), zs=zeros, color='#00A968', linestyle=':') # クラス2の平均 + 標準偏差
ax.plot(xs=vals, ys=np.repeat(E_x2 + s_x2, len(vals)), zs=zeros, color='#00A968', linestyle=':') # クラス2の平均 + 標準偏差
ax.set_xlabel('$x_1$')
ax.set_ylabel('$x_2$')
ax.set_zlabel('probability')
fig.suptitle('Maltinomial Distribution', fontsize=20)
ax.set_title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + '), M=' + str(M) + '$', loc='left')
ax.set_xticks(ticks=x_vals) # x軸目盛
ax.set_yticks(ticks=x_vals) # y軸目盛
ax.legend() # 凡例
ax.view_init(elev=90, azim=270) # 表示アングル
plt.show()

#%%

### パラメータと分布の形状の関係

## パラメータphiの影響

# 作図用のphiの値を作成
phi_vals = np.arange(start=0.0, stop=1.01, step=0.01)

# 作図用のphiの点を作成
phi_points = np.stack([
    phi_vals, 
    (1.0 - phi_vals) * 0.6, # 配分を指定
    (1.0 - phi_vals) * 0.4 # 配分を指定
], axis=1)


# 試行回数を指定
M = 10

# 作図用のxの値を作成
x_vals = np.arange(M + 1)

# 格子点を作成
X1, X2 = np.meshgrid(x_vals, x_vals)

# 作図用のxの点を作成:(xがとり得ない値を消去する場合)
x1_vals = np.delete(X1.flatten(), obj=(X1 + X2).flatten() > M)
x2_vals = np.delete(X2.flatten(), obj=(X1 + X2).flatten() > M)

# 作図用のxの点を作成:(全ての組み合わせを描画する場合)
#x1_vals = X1.flatten()
#x2_vals = X2.flatten()
x3_vals = np.where(x1_vals+x2_vals <= M, M - (x1_vals+x2_vals), 0.0)

# 計算用のxの点を作成
x_points = np.stack([x1_vals, x2_vals, x3_vals], axis=1)


# カラーマップの設定
cm = plt.get_cmap('jet')

# カラーマップの最大値を指定
p_max = 0.15

# 図を初期化
fig = plt.figure(figsize=(12, 9)) # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle('Multinomial Distribution', fontsize=20)

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のパラメータを取得
    phi_v = phi_points[i]
    
    # i番目のパラメータによる分布を計算
    probability = multinomial.pmf(x=x_points, n=M, p=phi_v)
    
    # xがとり得ない値の要素をマスク:(xがとり得ない値をマスクする場合)
    #probability = np.ma.masked_where(x1_vals + x2_vals > M, probability)
    
    # 多項分布を作図
    ax.bar3d(x=x1_vals - 0.45, y=x2_vals - 0.45, z=np.zeros_like(x1_vals), 
             dx=0.9, dy=0.9, dz=probability, 
             color=cm(probability / p_max), alpha=0.5, shade=True) # 3D棒グラフ
    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')
    ax.set_zlabel('probability')
    ax.set_title('$\phi=(' + ', '.join([str(phi) for phi in np.round(phi_v, 2)]) + ')' +
                 ', M=' + str(M) + '$', loc='left')
    ax.set_zlim(0.0, p_max) # z軸の表示範囲
    #ax.view_init(elev=0, azim=315) # 表示アングル

# gif画像を作成
anime_prob = FuncAnimation(fig, update, frames=len(phi_vals), interval=50)

# gif画像を保存
anime_prob.save('ProbabilityDistribution/Multinomial_prob_phi.gif')

#%%

## 試行回数Mの影響

# パラメータを指定
phi_v = np.array([0.3, 0.5, 0.2])

# 試行回数の最大値を指定
M_max = 50


# カラーマップの設定
cm = plt.get_cmap('jet')

# カラーマップの最大値を指定
p_max = 0.1

# 図を初期化
fig = plt.figure(figsize=(12, 8)) # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle('Multinomial Distribution', fontsize=20)

# 作図処理を関数として定義
def update(M):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # 1からMに変換
    M += 1
    
    # 作図用のxの値を作成
    x_vals = np.arange(M + 1)
    
    # 格子状のxの点を作成
    X1, X2 = np.meshgrid(x_vals, x_vals)
    
    # 作図用のxの点を作成:(xがとり得ない値を消去する場合)
    x1_vals = np.delete(X1.flatten(), obj=(X1 + X2).flatten() > M)
    x2_vals = np.delete(X2.flatten(), obj=(X1 + X2).flatten() > M)
    
    # 作図用のxの点を作成:(全ての組み合わせを描画する場合)
    #x1_vals = X1.flatten()
    #x2_vals = X2.flatten()
    x3_vals = np.where(x1_vals+x2_vals <= M, M - (x1_vals+x2_vals), 0.0)
    
    # 計算用のxの点を作成
    x_points = np.stack([x1_vals, x2_vals, x3_vals], axis=1)
    
    # 分布を計算
    probability = multinomial.pmf(x=x_points, n=M, p=phi_v)
    
    # xがとり得ない値の要素をマスク:(xがとり得ない値をマスクする場合)
    #probability = np.ma.masked_where(x1_vals + x2_vals > M, probability)
    
    # 多項分布を作図
    ax.bar3d(x=x1_vals - 0.45, y=x2_vals - 0.45, z=np.zeros_like(x1_vals), 
             dx=0.9, dy=0.9, dz=probability, 
             color=cm(probability / p_max), alpha=0.5, shade=True) # 3D棒グラフ
    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')
    ax.set_zlabel('probability')
    ax.set_title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + ')' +
                 ', M=' + str(M) + '$', loc='left')
    ax.set_zlim(0.0, p_max) # z軸の表示範囲
    #ax.view_init(elev=0, azim=315) # 表示アングル

# gif画像を作成
anime_prob = FuncAnimation(fig, update, frames=M_max, interval=100)

# gif画像を保存
anime_prob.save('ProbabilityDistribution/Multinomial_prob_M.gif')

#%%

### 乱数の生成

## 乱数の可視化

# パラメータを指定
phi_v = np.array([0.3, 0.5, 0.2])

# 試行回数を指定
M = 10

# データ数を指定
N = 1000

# 多項分布に従う乱数を生成
x_nv = np.random.multinomial(n=M, pvals=phi_v, size=N)


# 作図用のxの値を作成
x_vals = np.arange(M + 1)

# 格子点を作成
X1, X2 = np.meshgrid(x_vals, x_vals)

# 作図用のxの点を作成:(xがとり得ない値を削除する場合)
delete_idx = (X1 + X2).flatten() > M
x1_vals = np.delete(X1.flatten(), delete_idx)
x2_vals = np.delete(X2.flatten(), delete_idx)

# 作図用のxの点を作成:(全ての組み合わせを描画する場合)
#x1_vals = X1.flatten()
#x2_vals = X2.flatten()
x3_vals = np.where(x1_vals + x2_vals <= M, M - (x1_vals + x2_vals), 0)

# 計算用のxの点を作成
x_points = np.stack([x1_vals, x2_vals, x3_vals], axis=1)


# 重複するサンプルをカウント
uni_sample, uni_freq = np.unique(x_nv, return_counts=True, axis=0)

# サンプルにないxの点と結合
frequency = np.zeros(len(x_points))
for j, x_v in enumerate(uni_sample):
    for i, x_point in enumerate(x_points):
        # 作図用の点とサンプルが一致すれば頻度を代入
        if all(x_point == x_v):
            frequency[i] = uni_freq[j]
            break

# 乱数を集計:(総当たり)
frequency = np.zeros(len(x_points))
for i, x_point in enumerate(x_points):
    frequency[i] = np.sum([all(x_point == x_v) for x_v in x_nv])

# xがとり得ない値の要素をマスク:(xがとり得ない値をマスクする場合)
#frequency = np.ma.masked_where(x1_vals + x2_vals > M, frequency)

#%%

# カラーマップの設定
cm = plt.get_cmap('jet')

# サンプルのヒストグラムを作成
fig = plt.figure(figsize=(12, 9)) # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.bar3d(x=x1_vals - 0.45, y=x2_vals - 0.45, z=np.zeros_like(x1_vals), 
         dx=1.0, dy=1.0, dz=frequency, 
         color=cm(frequency / np.max(frequency)), alpha=0.5, shade=True) # ヒストグラム
ax.set_xlabel('$x_1$')
ax.set_ylabel('$x_2$')
ax.set_zlabel('frequency')
fig.suptitle('Maltinomial Distribution', fontsize=20)
ax.set_title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + ')' + 
             ', M=' + str(M) + ', N=' + str(N) + '$', loc='left')
#ax.view_init(elev=0, azim=315) # 表示アングル
plt.show()

#%%

# サンプルの構成比を計算
proportion = frequency / N

# サンプルの構成比を作図
fig = plt.figure(figsize=(12, 9)) # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.bar3d(x=x1_vals - 0.45, y=x2_vals - 0.45, z=np.zeros_like(x1_vals), 
         dx=1.0, dy=1.0, dz=proportion, 
         color=cm(proportion / np.max(proportion)), alpha=0.5, shade=True) # 構成比
ax.set_xlabel('$x_1$')
ax.set_ylabel('$x_2$')
ax.set_zlabel('proportion')
fig.suptitle('Maltinomial Distribution', fontsize=20)
ax.set_title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + ')' + 
             ', M=' + str(M) + ', N=' + str(N) + '$', loc='left')
#ax.view_init(elev=0, azim=315) # 表示アングル
plt.show()

#%%

## アニメーションによる可視化

# フレーム数を指定
N_frame = 150

# 図を初期化
fig = plt.figure(figsize=(12, 9))
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle('Maltinomial Distribution', fontsize=20)

# カラーマップの設定
cm = plt.get_cmap('jet')

# 頻度の最大値を取得
uni_arr, uni_freq = np.unique(x_nv[:N_frame], return_counts=True, axis=0)
z_max = np.max(uni_freq)

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # n個のサンプルをカウント
    uni_arr, uni_freq = np.unique(x_nv[:(n+1)], return_counts=True, axis=0)
    
    # サンプルにないxの点と結合
    frequency = np.zeros(len(x_points))
    for i, x_v in enumerate(uni_arr):
        for j, x_point in enumerate(x_points):
            # 作図用の点とサンプルが一致すれば頻度を代入
            if all(x_point == x_v):
                frequency[j] = uni_freq[i]
                break
    
    # xが取らない値をマスク
    frequency = np.ma.masked_where(x1_vals + x2_vals > M, frequency)
    
    # n番目のサンプルの値を取得
    x1, x2 = x_nv[n, [0, 1]]
    
    # n番目のサンプルの頻度を取得
    x_idx = np.where(np.all(x_points == x_nv[n], axis=1) == True)[0][0]
    z = frequency[x_idx]
    
    # サンプルのヒストグラムを作成
    ax.bar3d(x=x1_vals - 0.5, y=x2_vals - 0.5, z=np.zeros_like(x1_vals), 
             dx=1.0, dy=1.0, dz=frequency, 
             color=cm(frequency / z_max), alpha=0.5, shade=True) # ヒストグラム
    ax.scatter(x1, x2, z + 0.1, color='orange', s=100) # サンプル
    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')
    ax.set_zlabel('frequency')
    ax.set_title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + ')' + 
                 ', M=' + str(M) + ', N=' + str(n) + '$', loc='left')
    ax.set_zlim(0.0, z_max) # z軸の表示範囲
    #ax.view_init(elev=0, azim=300) # 表示アングル:(横から)
    #ax.view_init(elev=90, azim=270) # 表示アングル:(上から)

# gif画像を作成
anime_freq = FuncAnimation(fig, update, frames=N_frame, interval=100)

# gif画像を保存
anime_freq.save('ProbabilityDistribution/Multinomial_freq.gif')

#%%

# フレーム数を指定
N_frame = 150

# 図を初期化
fig = plt.figure(figsize=(12, 9))
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle('Maltinomial Distribution', fontsize=20)

# カラーマップの設定
cm = plt.get_cmap('jet')

# 構成比の最大値を取得
uni_arr, uni_freq = np.unique(x_nv[:N_frame], return_counts=True, axis=0)
p_max = np.max(uni_freq) / N
p_max = 0.2

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # n個のサンプルをカウント
    uni_arr, uni_freq = np.unique(x_nv[:(n+1)], return_counts=True, axis=0)
    
    # サンプルにないxの点と結合
    frequency = np.zeros(len(x_points))
    for i, x_v in enumerate(uni_arr):
        for j, x_point in enumerate(x_points):
            # 作図用の点とサンプルが一致すれば頻度を代入
            if all(x_point == x_v):
                frequency[j] = uni_freq[i]
                break
    
    # サンプルの構成比を計算
    proportion = frequency / (n + 1)
    
    # xが取らない値をマスク
    proportion = np.ma.masked_where(x1_vals + x2_vals > M, proportion)
    
    # n番目のサンプルの値を取得
    x1, x2 = x_nv[n, [0, 1]]
    
    # n番目のサンプルの頻度を取得
    x_idx = np.where(np.all(x_points == x_nv[n], axis=1) == True)[0][0]
    z = proportion[x_idx]
    
    # サンプルの構成比を作成
    ax.bar3d(x=x1_vals - 0.5, y=x2_vals - 0.5, z=np.zeros_like(x1_vals), 
             dx=1.0, dy=1.0, dz=proportion, 
             color=cm(proportion / p_max), alpha=0.5, shade=True) # 構成比
    ax.scatter(x1, x2, z, color='orange', s=100) # サンプル
    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')
    ax.set_zlabel('proportion')
    ax.set_title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + ')' + 
                 ', M=' + str(M) + ', N=' + str(n) + '$', loc='left')
    ax.set_zlim(0.0, p_max) # z軸の表示範囲
    #ax.view_init(elev=0, azim=300) # 表示アングル:(横から)
    ax.view_init(elev=90, azim=270) # 表示アングル:(上から)

# gif画像を作成
anime_prop = FuncAnimation(fig, update, frames=N_frame, interval=100)

# gif画像を保存
anime_prop.save('ProbabilityDistribution/Multinomial_prop.gif')

#%%

