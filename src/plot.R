# script for plotting results of the simulations
pacman::p_load(tidyverse)

# load data
datafilepath = file.path("out", "data.csv")
df = read_csv(datafilepath)

# sort to only look at WSLS_RL for now (combination col)
RL_WSLS_df = df %>% filter(combination == "RL_WSLS")

# compute average performance for each trial and agent
average = RL_WSLS_df %>%
    group_by(trial, agent_self_type, learning_rate) %>%
    summarise(average_performance = mean(feedback)) # mean corresponds to calculating proportion

# convert trial to integer (for adjusting x-axis)
average$trial <- as.integer(average$trial)
average$learning_rate <- as.factor(average$learning_rate)

# create plot
plot <- ggplot(average, aes(x = trial, y = average_performance, color = agent_self_type, group = agent_self_type)) +
    geom_line() +
    geom_point() +
    labs(x = "Trial", y = "Proportion of wins at trial", color = "Agent") +
    scale_x_continuous(breaks = seq(0, 120, 10)) +
    facet_grid(learning_rate~.) +
    theme_bw()

# save the plot
ggsave(file.path("out", "plot_RL_WSLS.jpeg"), plot, width = 12, height = 6)