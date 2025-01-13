# test ggstackplot() functionality

    Code
      plot_prep <- prepare_stackplot(mtcars, x = mpg, y = c(wt, qsec), palette = "Set1")

---

    Code
      plot_prep$plot[[2]] <- ggplot(mtcars) + aes(mpg, drat) + geom_point()
      plot_prep$theme[[2]] <- theme_bw()
      plot_prep
    Output
      # A tibble: 2 x 6
        .var  config           data               plot   theme   add   
        <chr> <list>           <list>             <list> <list>  <list>
      1 wt    <tibble [1 x 9]> <tibble [32 x 11]> <gg>   <theme> <NULL>
      2 qsec  <tibble [1 x 9]> <tibble [32 x 11]> <gg>   <theme> <NULL>

---

    Code
      x <- ggstackplot(mtcars, x = mpg, y = c(qsec, drat), debug = TRUE)
    Message
      
      [DEBUG] stackplot tibble
    Output
      # A tibble: 2 x 11
        .var  .xvar .yvar .color .axis_switch .shared_axis_min .shared_axis_max .first
        <chr> <fct> <fct> <lgl>  <lgl>                   <dbl>            <dbl> <lgl> 
      1 qsec  mpg   qsec  NA     TRUE                     10.4             33.9 TRUE  
      2 drat  mpg   drat  NA     FALSE                    10.4             33.9 FALSE 
      # i 3 more variables: .last <lgl>, .direction <chr>, data <list>
    Message
      
      [DEBUG] stackplot gtables
    Output
      # A tibble: 3 x 14
        .var  .direction  size size_adjust pos_adjust overlap   pos total_size rel_pos
        <chr> <chr>      <dbl>       <dbl>      <dbl>   <dbl> <dbl>      <dbl>   <dbl>
      1 qsec  vertical     1             0          0       0   1          2.2  0.545 
      2 drat  vertical     1             0          0       0   2          2.2  0.0909
      3 prim~ vertical     0.2           1          0       0   2.2        2.2  0     
      # i 5 more variables: rel_size <dbl>, x <dbl>, y <dbl>, width <dbl>,
      #   height <dbl>

