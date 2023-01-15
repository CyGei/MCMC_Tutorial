#' @export

clean_chain <- function(chain, thinning = 1, burnin = 0){
  chain %>% 
    filter((iter %in%  seq(1,nrow(.), thinning)) & iter >= burnin) %>% 
    pivot_longer(cols = c(mu_chain, sd_chain),
                 names_to = "param")
}

trace_plot <- function(chain){
  chain %>% 
    ggplot(aes(x = iter, y = value, col = param ))+
    geom_line() +
    facet_wrap(~param, scales = "free")+
    theme_fira()+
    scale_color_fira()+
    labs(x = "iteration")+
    theme(legend.position = "none")
}

acf_2_df <- function(chain, params){
  chain %>%  
    filter(param %in% c(params)) %>% 
    group_by(param) %>% 
    summarize(value = list(value)) %>% 
    mutate(value = map(value, ~ as.ts(., start = 1, frequency = 1))) %>% 
    mutate(value = map(value, stats::acf, plot = FALSE)) %>% 
    mutate(value = map(value, fortify)) %>% 
    unnest(value) %>% 
    as_tibble()
}

acf_plot <-  function(df){
  df %>% 
    ggplot()+
    aes(x = Lag, y = ACF, col = param) +
    geom_segment(aes(xend = Lag, y = 0, yend = ACF), col = "black")+
    geom_point(size = 3)+
    geom_hline(aes( yintercept = 0), lty = "solid")+
    geom_hline(aes( yintercept = lower), lty = "dotted")+
    geom_hline(aes( yintercept = upper), lty = "dotted")+
    facet_wrap(~param)+
    scale_color_fira()+
    labs(y = "cor")
}