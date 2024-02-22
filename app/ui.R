
# 確率分布の可視化 ----------------------------------------------------------------

# 利用パッケージ
library(shiny)


# UI
fluidPage(
  
  # タイトル
  titlePanel(title = "probability distribution"), 
  
  # 出力
  fluidRow(
    column(
      width = 12, 
      
      # グラフ
      plotOutput(
        outputId = "plot"
      )
    )
  ), 
  
  # 入力
  fluidRow(
    
    # 変数
    column(
      width = 6, 
      "variable", 
      
      # 曲線用の変数の範囲
      sliderInput(
        inputId = "curve_variable", 
        label = "range: x", 
        min = -10, 
        max = 10, 
        value = c(-5, 5), 
        step = 0.1
      ), 
      
      # 点用の変数
      sliderInput(
        inputId = "point_variable", 
        label = "coord: x", 
        min = -10, 
        max = 10, 
        value = 0, 
        step = 0.1, 
        animate = animationOptions(
          interval = 500
        )
      )
      
    ), 
    
    # パラメータ
    column(
      width = 6, 
      "parameter", 
      
      # 平均パラメータ
      sliderInput(
        inputId = "mean", 
        label = "μ", 
        min = -10, 
        max = 10, 
        value = 0, 
        step = 0.1, 
        animate = animationOptions(
          interval = 500
        )
      ), 
      
      # 標準偏差パラメータ
      sliderInput(
        inputId = "sd", 
        label = "σ", 
        min = 0, 
        max = 5, 
        value = 1, 
        step = 0.1, 
        animate = animationOptions(
          interval = 500
        )
      )
      
    )
  )
  
)



