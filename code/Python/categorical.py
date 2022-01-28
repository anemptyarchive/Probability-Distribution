# カテゴリ分布

# 利用するライブラリ
import numpy as np
from scipy.stats import multinomial # 多項分布
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

#%%

### 確率の計算

# パラメータを指定
phi_v = np.array([0.2, 0.4, 0.1, 0.3])

# 確率変数の値を指定
x_v = np.array([0, 1, 0, 0])


# 定義式により確率を計算
prob = np.prod(phi_v**x_v)
print(prob)

# 対数をとった定義式により確率を計算
log_prob = np.sum(x_v * np.log(phi_v))
prob = np.exp(log_prob)
print(prob, log_prob)

# 多項分布の関数により確率を計算
prob = multinomial.pmf(x=x_v, n=1, p=phi_v)
print(prob)

# 多項分布の対数をとった関数により確率を計算
log_prob = multinomial.logpmf(x=x_v, n=1, p=phi_v)
prob = np.exp(log_prob)
print(prob, log_prob)

# インデックスにより確率を抽出
v = np.where(x_v == 1)[0][0]
prob = phi_v[v]
print(prob)

#%%

### 統計量の計算

# パラメータを指定
phi_v = np.array([0.2, 0.4, 0.1, 0.3])

# クラス番号を指定
v = 1

# クラスvの平均を計算
E_x = phi_v[v]
print(E_x)

# クラスvの分散を計算
V_x = phi_v[v] * (1.0 - phi_v[v])
print(V_x)

#%%

### グラフの作成

# パラメータを指定
phi_v = np.array([0.2, 0.4, 0.1, 0.3])

# クラス数を取得
V = len(phi_v)

# 作図用のクラス番号を作成
v_vals = np.arange(1, V + 1)

# 分布を計算
probability = phi_v.copy()

# 多項分布の関数により分布を計算
probability = multinomial.pmf(x=np.identity(V), n=1, p=phi_v)

#%%

# カテゴリ分布を作図
plt.figure(figsize=(12, 8)) # 図の設定
plt.bar(x=v_vals, height=probability, color='#00A968') # 棒グラフ
plt.xlabel('v') # x軸ラベル
plt.ylabel('probability') # y軸ラベル
plt.suptitle('Categorical Distribution', fontsize=20) # 図タイトル
plt.title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + ')$', loc='left') # タイトル
plt.xticks(ticks=v_vals) # x軸目盛
plt.grid() # グリッド線
plt.show() # 描画

#%%

### パラメータと分布の形状の関係

# 作図用のphi_1の値を作成
phi_vals = np.arange(start=0.0, stop=1.0, step=0.01)

# クラス数を指定
V = 3

# 作図用のクラス番号を作成
v_vals = np.arange(1, V + 1)

# 図を初期化
fig = plt.figure(figsize=(12, 8))

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i回目のphi_1の値を取得
    phi_1 = phi_vals[i]
    
    # phi_1以外の割り当てを指定
    phi_v = np.array([phi_1, (1.0 - phi_1) * 0.6, (1.0 - phi_1) * 0.4])
    
    # カテゴリ分布を作図
    plt.bar(x=v_vals, height=phi_v, color='#00A968', zorder=1) # 棒グラフ
    plt.xlabel('v') # x軸ラベル
    plt.ylabel('Probability') # y軸ラベル
    plt.suptitle('Categorical Distribution', fontsize=20) # 図タイトル
    plt.title('$\phi=(' + ', '.join([str(phi) for phi in np.round(phi_v, 2)]) + ')$', loc='left') # タイトル
    plt.xticks(ticks=v_vals) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(-0.1, 1.1) # y軸の表示範囲

# gif画像を作成
anime_prob = FuncAnimation(fig, update, frames=len(phi_vals), interval=100)

# gif画像を保存
anime_prob.save('ProbabilityDistribution/Categorical_prob.gif')

#%%

### 乱数の生成

## 乱数の可視化

# パラメータを指定
phi_v = np.array([0.2, 0.4, 0.1, 0.3])

# データ数を指定
N = 1000

# カテゴリ分布に従う乱数を生成
x_nv = np.random.multinomial(n=1, pvals=phi_v, size=N)

# クラス番号を抽出
x_n = np.where(x_nv == 1)[1]

# 乱数を集計
frequency = np.sum(x_nv, axis=0)

#%%

# サンプルのヒストグラムを作成
plt.figure(figsize=(12, 8)) # 図の設定
plt.bar(x=v_vals, height=frequency, color='#00A968') # ヒストグラム
plt.xlabel('v') # x軸ラベル
plt.ylabel('frequency') # y軸ラベル
plt.suptitle('Bernoulli Distribution', fontsize=20) # 図タイトル
plt.title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + ')' + 
          ', N=' + str(N) +'=(' + ', '.join([str(f) for f in frequency]) + ')$', loc='left') # タイトル
plt.xticks(ticks=v_vals) # x軸目盛
plt.grid() # グリッド線
plt.show() # 描画

# サンプルの構成比を作図
plt.figure(figsize=(12, 8)) # 図の設定
plt.bar(x=v_vals, height=probability, color='white', edgecolor='green', linestyle='--') # 分布
plt.bar(x=v_vals, height=frequency / N, color='#00A968', alpha=0.8) # 構成比
plt.xlabel('v') # x軸ラベル
plt.ylabel('proportion') # y軸ラベル
plt.suptitle('Bernoulli Distribution', fontsize=20) # 図タイトル
plt.title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + ')' + 
          ', N=' + str(N) +'=(' + ', '.join([str(f) for f in frequency]) + ')$', loc='left') # タイトル
plt.xticks(ticks=v_vals) # x軸目盛
plt.grid() # グリッド線
plt.show() # 描画

#%%

## アニメーションによる可視化

# フレーム数を指定
N = 100

# 図を初期化
fig = plt.figure(figsize=(12, 8))

# 頻度の最大値を取得
y_max = np.max(np.sum(x_nv[:N], axis=0))

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # n個の乱数を集計
    frequency = np.sum(x_nv[:(n+1)], axis=0)
    
    # n番目の乱数のクラスを取得
    x_val = np.where(x_nv[n] == 1)[0][0] + 1
    
    # サンプルのヒストグラムを作成
    plt.bar(x=v_vals, height=frequency, color='#00A968', zorder=1) # ヒストグラム
    plt.scatter(x=x_val, y=0.0, color='orange', s=100, zorder=2) # サンプル
    plt.xlabel('v') # x軸ラベル
    plt.ylabel('frequency') # y軸ラベル
    plt.suptitle('Categorical Distribution', fontsize=20) # 図タイトル
    plt.title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + ')' + 
              ', N=' + str(n + 1) +'=(' + ', '.join([str(f) for f in frequency]) + ')$', loc='left') # タイトル
    plt.xticks(ticks=v_vals) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(-1.0, y_max + 1.0) # y軸の表示範囲

# gif画像を作成
anime_freq = FuncAnimation(fig, update, frames=N, interval=100)

# gif画像を保存
anime_freq.save('ProbabilityDistribution/Categorical_freq.gif')

#%%

# 図を初期化
fig = plt.figure(figsize=(12, 8))

# 作図処理を関数として定義
def update(n):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # n個の乱数を集計
    frequency = np.sum(x_nv[:(n+1)], axis=0)
    
    # n番目の乱数のクラスを取得
    x_val = np.where(x_nv[n] == 1)[0][0] + 1
    
    # サンプルの構成比を作成
    plt.bar(x=v_vals, height=probability, color='white', edgecolor='green', linestyle='--', zorder=1) # 分布
    plt.bar(x=v_vals, height=frequency / (n + 1), color='#00A968', alpha=0.8, zorder=2) # 構成比
    plt.scatter(x=x_val, y=0.0, color='orange', s=100, zorder=3) # サンプル
    plt.xlabel('v') # x軸ラベル
    plt.ylabel('proportion') # y軸ラベル
    plt.suptitle('Categorical Distribution', fontsize=20) # 図タイトル
    plt.title('$\phi=(' + ', '.join([str(phi) for phi in phi_v]) + ')' + 
              ', N=' + str(n + 1) +'=(' + ', '.join([str(f) for f in frequency]) + ')$', loc='left') # タイトル
    plt.xticks(ticks=v_vals) # x軸目盛
    plt.grid() # グリッド線
    plt.ylim(-0.1, 1.1) # y軸の表示範囲

# gif画像を作成
anime_prop = FuncAnimation(fig, update, frames=N, interval=100)

# gif画像を保存
anime_prop.save('ProbabilityDistribution/Categorical_prop.gif')

#%%

print('end')

