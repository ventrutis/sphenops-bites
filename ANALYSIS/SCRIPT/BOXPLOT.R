fish_box = function(pdf_path, 
                      single_run, 
                      data, 
                      x_val, 
                      y_val, 
                      z_val, 
                      col_val, 
                      main = FALSE, 
                      ylab = FALSE, 
                      xlab = FALSE, 
                      legend_position = FALSE, 
                      ylim = FALSE, 
                      no_outline = FALSE) {
  
  # Open a new PDF file 
  pdf(pdf_path)
  
  # Check if single_run is FALSE, plot all data without filtering by single_run
  if (single_run==F) {
    # Get the unique values of z_val
    unique_z = unique(data[[z_val]])
    
    # Factor x_val to ensure correct order and levels
    data[[x_val]] = factor(data[[x_val]])
    levels_x = levels(data[[x_val]])
    
    # Create an index for each value of z_val
    index = seq(2, length(unique_z) * 2, by = 2)
    
    # Define outline conditions based on no_outline
    outline_conditions = if (no_outline) c(TRUE, FALSE) else c(TRUE)
    
    # Create boxplot based on outline conditions
    for (outline_val in outline_conditions) {
      # Set titles and labels, use variable names if not specified
      plot_main = ifelse(main == FALSE, paste(y_val, "~", x_val, "*", z_val, "Outline:", outline_val), main)
      plot_ylab = ifelse(ylab == FALSE, y_val, ylab)
      plot_xlab = ifelse(xlab == FALSE, z_val, xlab)
      
      # Determine ylim: use specified values if not FALSE
      plot_ylim = if (isFALSE(ylim)) NULL else ylim
      
      # Create the boxplot for the entire data
      boxplot(data[[y_val]] ~ data[[x_val]] * data[[z_val]], 
              data = data, 
              outline = outline_val,
              col = col_val, 
              main = plot_main, 
              ylab = plot_ylab, 
              xlab = plot_xlab,
              ylim = plot_ylim)
      
      # Add an abline for each value of z_val, except the last one
      for (i in index[-length(index)]) {
        abline(v = i + 0.5, lty = 2, col = "grey")
      }
      
      # Add legend if legend_position is not FALSE
      if (legend_position != FALSE) {
        # Directly use levels of the factor for colors in the legend
        legend(legend_position, legend = levels_x, 
               fill = col_val[seq_along(levels_x)], title = x_val)
      }
    }
  } else {
    # Single runs
    unique_runs = unique(data[[single_run]])
    
    # Create a boxplot for each unique "single_run"
    for (run_val in unique_runs) {
      # Filter the data for the current value of "single_run"
      current_data = data[data[[single_run]] == run_val,]
      
      # Get the unique values of z_val
      unique_z = unique(current_data[[z_val]])
      
      # Factor x_val to ensure correct order and levels
      current_data[[x_val]] = factor(current_data[[x_val]])
      levels_x = levels(current_data[[x_val]])
      
      # Create an index for each value of z_val
      index = seq(2, length(unique_z) * 2, by = 2)
      
      # Define outline conditions based on no_outline
      outline_conditions = if (no_outline) c(TRUE, FALSE) else c(TRUE)
      
      # Create a boxplot for the current data based on outline conditions
      for (outline_val in outline_conditions) {
        # Set titles and labels, use variable names if not specified
        plot_main = ifelse(main == FALSE, paste(y_val, "~", x_val, "*", z_val, single_run, ":", run_val, "Outline:", outline_val), main)
        plot_ylab = ifelse(ylab == FALSE, y_val, ylab)
        plot_xlab = ifelse(xlab == FALSE, z_val, xlab)
        
        # Determine ylim: use specified values if not FALSE
        plot_ylim = if (isFALSE(ylim)) NULL else ylim
        
        # Create the boxplot for the current data
        boxplot(current_data[[y_val]] ~ current_data[[x_val]] * current_data[[z_val]], 
                data = current_data, 
                outline = outline_val,
                col = col_val, 
                main = plot_main, 
                ylab = plot_ylab, 
                xlab = plot_xlab,
                ylim = plot_ylim)
        
        # Add an abline for each value of z_val, except the last one
        for (i in index[-length(index)]) {
          abline(v = i + 0.5, lty = 2, col = "grey")
        }
        
        # Add legend if legend_position is not FALSE
        if (legend_position != FALSE) {
          # Directly use levels of the factor for colors in the legend
          legend(legend_position, legend = levels_x, 
                 fill = col_val[seq_along(levels_x)], title = x_val)
        }
      }
    }
  }
  
  # Close the PDF file
  dev.off()
  
} # END FUNCTION 
