
# 確率分布の可視化 ----------------------------------------------------------------

# 利用パッケージ
library(shiny)
library(ggplot2)


# server
function(input, output) {
  
  output$plot <- renderPlot({
    
    # グラフサイズを設定
    x_size <- 10
    y_max <- 0.5
    
    # パラメータを取得
    mu <- input$mean
    sigma <- input$sd
    
    # 曲線の座標を作成
    curve_df <- tibble::tibble(
      x   = seq(from = input$curve_variable[1], to = input$curve_variable[2], length.out = 1000), 
      p_x = dnorm(x = x, mean = mu, sd = sigma)
    )
    
    # 点の座標を作成
    point_df <- tibble::tibble(
      x   = input$point_variable, 
      p_x = dnorm(x = x, mean = mu, sd = sigma)
    )
    
    # ラベル用の文字列を作成
    fml_label <- "p(list(x ~'|'~ mu, sigma))"
    param_label <- paste0(
      "list(", 
      "mu == ", round(mu, digits = 2), ", ", 
      "sigma == ", round(sigma, digits = 2), ", ", 
      "sigma^2 == ", round(sigma^2, digits = 2), ", ", 
      "(list(x, p(x))) == ", 
      "(list(", round(point_df[["x"]], digits = 2), ", ", round(point_df[["p_x"]], digits = 2), "))", 
      ")"
    )
    
    # ガウス分布を作図
    ggplot() + 
      geom_line(
        data    = curve_df, 
        mapping = aes(x = x, y = p_x, linetype = "dist")
      ) + # 確率密度の曲線
      geom_point(
        data    = point_df, 
        mapping = aes(x = x, y = p_x), 
        size = 5
      ) + # 曲線上の点
      scale_linetype_manual(
        breaks = "dist", 
        values = "solid", 
        labels = parse(text = fml_label), 
        name   = "function"
      ) + # 凡例表示用
      theme(
        legend.text.align = 0, 
        legend.position = c(0, 1), 
        legend.justification = c(0, 1), 
        legend.background = element_rect(fill = alpha("white", alpha = 0.8))
      ) + 
      coord_cartesian(
        xlim = c(-x_size, x_size), 
        ylim = c(0, y_max)
      ) + 
      labs(
        title = "Normal distribution", 
        subtitle = parse(text = param_label), 
        x = "x", 
        y = "density"
      )
  })
  
}
