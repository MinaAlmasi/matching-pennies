# script for plotting results of the simulations
pacman::p_load(tidyverse)

# load data
datafilepath <- file.path("out", "data.csv")
df <- read_csv(datafilepath)

# sort to only look at WSLS_RL for now (combination col)
dfs <- split(df, f = df$combination)

# compute average performance for each trial and agent
average_list <- lapply(dfs, function(x) {
    x %>% group_by(trial, role, agent_self_type, learning_rate) %>%
    summarise(average_performance = mean(feedback)) # mean corresponds to calculating proportion
})

# loop over the three elements in average_list and change to learning rate to factor and trial as integer
for (i in 1:3){
    average_list[[i]]$learning_rate <- as.factor(average_list[[i]]$learning_rate)
    average_list[[i]]$trial <- as.integer(average_list[[i]]$trial)
}

# create the plots
plot_RL_WSLS <- ggplot(average_list$RL_WSLS, aes(x = trial, y = average_performance, color = agent_self_type, group = agent_self_type)) +
    geom_hline(yintercept=0.5, linetype="dashed") +
    geom_line() +
    geom_point() +
    labs(x = "Trial", y = "Proportion of wins at trial", color = "Agent") +
    scale_color_manual(labels = c("RL (hider)", "WSLS* (picker)"), 
                       values = c("RL" = "#40A2E3", "WSLS" = "#EB4747")) +  
    scale_x_continuous(breaks = seq(0, 120, 10)) +
    ylim(0,1) +
    facet_wrap(.~learning_rate, ncol=1) +
    theme_bw()+
    theme(legend.key.size = unit(1, "cm"), 
          legend.text = element_text(size=12), 
          legend.title=element_text(size=12),
          strip.background = element_rect(fill="white"),
          strip.text = element_text(size = 16))

# save the plot
ggsave(file.path("out", "plot_RL_WSLS.jpeg"), plot_RL_WSLS, width = 10, height = 10)

plot_RL_RL <- ggplot(average_list$RL_RL, aes(x = trial, y = average_performance, color = role, group = role)) +
    geom_hline(yintercept=0.5, linetype="dashed") +
    geom_line() +
    geom_point() +
    labs(x = "Trial", y = "Proportion of wins at trial", color = "Agent") +
    scale_color_manual(labels = c("RL (hider)", "RL (picker)"), 
                       values = c("hider" = "#40A2E3", "picker" = "#EB4747")) +  
    scale_x_continuous(breaks = seq(0, 120, 10)) +
    ylim(0,1) +
    facet_wrap(.~learning_rate, ncol=1) +
    theme_bw()+
    theme(legend.key.size = unit(1, "cm"), 
          legend.text = element_text(size=12), 
          legend.title=element_text(size=12),
          strip.background = element_rect(fill="white"),
          strip.text = element_text(size = 16))

# save the plot
ggsave(file.path("out", "plot_RL_RL.jpeg"), plot_RL_RL, width = 10, height = 10)


plot_WSLS_WSLS <- ggplot(average_list$WSLS_WSLS, aes(x = trial, y = average_performance, color = role, group = role)) +
    geom_hline(yintercept=0.5, linetype="dashed") +
    geom_line() +
    geom_point() +
    labs(x = "Trial", y = "Proportion of wins at trial", color = "Agent") +
    scale_color_manual(labels = c("WSLS (hider)", "WSLS (picker)"), 
                       values = c("hider" = "#40A2E3", "picker" = "#EB4747")) +  
    scale_x_continuous(breaks = seq(0, 120, 10)) +
    ylim(0,1) +
    theme_bw()+
    theme(legend.key.size = unit(1, "cm"), 
          legend.text = element_text(size=12), 
          legend.title=element_text(size=12))


# save the plot
ggsave(file.path("out", "plot_WSLS_WSLS.jpeg"), plot_WSLS_WSLS, width = 10, height = 3.3)
