# script for plotting results of the simulations
pacman::p_load(tidyverse)

# load data
datafilepath = file.path("out", "data.csv")
df = read_csv(datafilepath)

# sort to only look at WSLS_RL for now (combination col)
RL_WSLS_df = df %>% filter(combination == "RL_WSLS")

# compute average performance for each trial and agent
average = RL_WSLS_df %>%
    group_by(trial, agent_self_type) %>%
    summarise(average_performance = mean(feedback))

# convert trial to integer (for adjusting x-axis)
average$trial <- as.integer(average$trial)

# create plot
plot <- ggplot(average, aes(x = trial, y = average_performance, color = agent_self_type, group = agent_self_type)) +
    geom_line() +
    geom_point() +
    labs(x = "Trial", y = "Average Performance", color = "Agent") +
    scale_x_continuous(breaks = seq(0, 120, 10)) +
    theme_bw()

# save the plot
ggsave(file.path("out", "plot_test.jpeg"), plot, width = 8, height = 6)