metric_box <- function(title, output_id, color = "#023047") {
  div(
    class = "metric-box",
    style = sprintf(
      "background: white; border-left: 10px solid %s; border-radius: 5px; 
      padding: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); height: 100px;",
      color
    ),
    
    h4(title, style = "margin-top: 0; font-size: 20px; font-weight: bold; font-family: 'Times New Roman', serif;"),
    
    div(
      style = sprintf("font-size: 24px; font-weight: bold; font-family: 'Times New Roman', serif; color: %s;", color),
      textOutput(output_id)
    )
  )
}

