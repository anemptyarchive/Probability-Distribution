# 確率分布に利用する関数

# 利用するライブラリ
import numpy as np
from scipy.special import gamma, gammaln, digamma, beta # ガンマ関数, 対数ガンマ関数, ディガンマ関数, ベータ関数
import matplotlib.pyplot as plt

#%%

### ガンマ関数の計算

# 変数の値を指定
x = 4.0

# ガンマ関数の計算
y = gamma(x)
print(y)

# 対数ガンマ関数による計算
log_y = gammaln(x)
z = np.exp(log_y)
print(y)


# ガンマ関数を計算:(発散)
print(gamma(171.0))
print(gamma(172.0))

# 対数ガンマ関数による計算
print(gammaln(172.0))


# ガンマ関数を計算:(計算できない)
print(gamma(0.0))
print(gamma(-3.0))


#%%

### ガンマ関数の作図

## 非負の値の場合

# xの値を指定:(x > 0)
x_vals = np.arange(start=0.01, stop = 6.0, step=0.01)

# ガンマ関数の計算
y_vals = gamma(x_vals)

# ガンマ関数を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(x_vals, y_vals, color='orange') # 折れ線グラフ
plt.xlabel('x') # x軸ラベル
plt.ylabel('$\Gamma(x)$') # y軸ラベル
plt.suptitle('Gamma Function', fontsize=20) # 全体のタイトル
plt.grid() # グリッド線
plt.show() # 描画

#%%

## 負の値を含む場合

# xの値を指定:(0と負の整数は計算できない)
x_vals = np.arange(start=-5.0, stop=5.0, step=0.0005)

# ガンマ関数の計算
y_vals = gamma(x_vals)

# 閾値を指定
threshold = 10.0

# 閾値外の値をマスク
y_mask_vals = np.ma.masked_where((y_vals < -threshold) | (y_vals > threshold), y_vals)

# ガンマ関数を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(x_vals, y_mask_vals, color='orange') # 折れ線グラフ
plt.xlabel('x') # x軸ラベル
plt.ylabel('$\Gamma(x)$') # y軸ラベル
plt.suptitle('Gamma Function', fontsize=20) # 全体のタイトル
plt.xticks(ticks=np.arange(start=np.floor(x_vals.min()), stop=np.ceil(x_vals.max())+1.0)) # x軸目盛
plt.ylim(ymin=-threshold, ymax=threshold) # y軸の表示範囲
plt.grid() # グリッド線
plt.show() # 描画


#%%

### ディガンマ関数の計算

# 変数の値を指定
x = 4.0

# ディガンマ関数の計算
y = digamma(x)
print(y)


# ディガンマ関数を計算:(計算できない)
print(digamma(0.0))
print(digamma(-3.0))


#%%

### ディガンマ関数の作図

## 非負の値の場合

# x軸の値を指定:(x > 0)
x_vals = np.arange(start=0.01, stop = 5.0, step=0.01)

# ディガンマ関数の計算
y_vals = digamma(x_vals)

# ディガンマ関数を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(x_vals, y_vals, color='orange') # 折れ線グラフ
plt.xlabel('x') # x軸ラベル
plt.ylabel('$\psi(x)$') # y軸ラベル
plt.suptitle('Digamma Function', fontsize=20) # 全体のタイトル
plt.grid() # グリッド線
plt.show() # 描画

#%%

## 負の値を含む場合

# x軸の値を指定:(0と負の整数は計算できない)
x_vals = np.arange(start=-5.0, stop=5.0, step=0.002)

# ディガンマ関数の計算
y_vals = digamma(x_vals)

# 閾値を指定
threshold = 10.0

# 閾値外の値をマスク
y_mask_vals = np.ma.masked_where((y_vals < -threshold) | (y_vals > threshold), y_vals)

# ディガンマ関数を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(x_vals, y_mask_vals, color='orange') # 折れ線グラフ
plt.xlabel('x') # x軸ラベル
plt.ylabel('$\psi(x)$') # y軸ラベル
plt.suptitle('Digamma Function', fontsize=20) # 全体のタイトル
plt.xticks(ticks=np.arange(start=np.floor(x_vals.min()), stop=np.ceil(x_vals.max())+1.0)) # x軸目盛
plt.ylim(ymin=-threshold, ymax=threshold) # y軸の表示範囲
plt.grid() # グリッド線
plt.show() # 描画


#%%

### ベータ関数の計算

# 変数の値を指定
a = 2.1
b = 3.2

# ガンマ関数によりベータ関数の計算
z = gamma(a) * gamma(b) / gamma(a + b)
print(z)

# 対数ガンマ関数によりベータ関数の計算
log_z = gammaln(a) + gammaln(b) - gammaln(a + b)
z = np.exp(log_z)
print(z)

# ベータ関数の計算
z = beta(a, b)
print(z)


# ベータ関数を計算:(計算できない)
print(beta(2.5, 0.0))
print(beta(-2.0, 2.5))


#%%

### ベータ関数の作図

## 非負の値の場合

# x軸とy軸の値を指定:(a > 0, b > 0)
a_vals = np.arange(start=0.01, stop = 3.0, step=0.01)
b_vals = np.arange(start=0.01, stop = 3.0, step=0.01)

# 格子状の点を作成
a_grid, b_grid = np.meshgrid(a_vals, b_vals)
print(a_grid.shape)

# ベータ関数の計算
z_grid = beta(a_grid, b_grid)

#%%

# ベータ関数を作図
fig = plt.figure(figsize=(9, 8))
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.plot_surface(a_grid, b_grid, z_grid, cmap='jet', alpha=0.6) # 曲面図
ax.contour(a_grid, b_grid, z_grid, cmap='jet', offset=z_grid.min()) # 等高線図
ax.set_xlabel('$\\alpha$') # x軸ラベル
ax.set_ylabel('$\\beta$') # y軸ラベル
ax.set_zlabel('$B(\\alpha, \\beta)$') # z軸ラベル
fig.suptitle('Beta Function', fontsize=20) # 全体のタイトル
plt.show() # 描画

#%%

## 非負の値を含むの場合

# x軸とy軸の値を指定:(0と負の整数は計算できない)
a_vals = np.arange(start=-2.0, stop=2.0, step=0.001)
b_vals = np.arange(start=-2.0, stop=2.0, step=0.001)# + 1e-4

# 格子状の点を作成
a_grid, b_grid = np.meshgrid(a_vals, b_vals)
print(a_grid.shape)

# ベータ関数の計算
z_grid = beta(a_grid, b_grid)

# 閾値を指定
threshold = 10.0

# 閾値外の値をマスク
z_mask_grid = np.ma.masked_where((z_grid < -threshold) | (z_grid > threshold), z_grid)

#%%

# ベータ関数を作図
fig = plt.figure(figsize=(9, 8))
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.plot_surface(a_grid, b_grid, z_mask_grid, cmap='jet', alpha=0.6) # 曲面図
ax.contour(a_grid, b_grid, z_mask_grid, cmap='jet', offset=z_mask_grid.min()) # 等高線図
ax.set_xlabel('$\\alpha$') # x軸ラベル
ax.set_ylabel('$\\beta$') # y軸ラベル
ax.set_zlabel('$B(\\alpha, \\beta)$') # z軸ラベル
fig.suptitle('Beta Function', fontsize=20) # 全体のタイトル
ax.set_zlim(zmin=z_mask_grid.min(), zmax=threshold) # z軸の表示範囲
ax.view_init(elev=90, azim=270) # 表示アングル
plt.show() # 描画

#%%

## 1変数:負の値を含む場合

# 1つ目の変数の値(x軸の値)を指定:(0と負の整数は計算できない)
a_vals = np.arange(start=-5.0, stop=5.0, step=0.01)

# 2つ目の変数の値(固定する値)を指定:(0と負の整数は計算できない)
b = -2.5

# ベータ関数の計算
z_vals = beta(a_vals, b)

# 閾値を指定
threshold = 10.0

# 閾値外の値をマスク
z_mask_vals = np.ma.masked_where((z_vals < -threshold) | (z_vals > threshold), z_vals)

# ベータ関数を作図
plt.figure(figsize=(12, 9)) # 図の設定
plt.plot(a_vals, z_mask_vals, color='orange') # 折れ線グラフ
plt.xlabel('$\\alpha$') # x軸ラベル
plt.ylabel('$(\\alpha, \\beta)$') # y軸ラベル
plt.suptitle('Beta Function', fontsize=20) # 全体のタイトル
plt.title('$\\beta=' + str(b) + '$', loc='left') # タイトル
plt.ylim(ymin=-threshold, ymax=threshold) # y軸の表示範囲
plt.grid() # グリッド線
plt.show() # 描画

#%%

