# script to simulate games defined in game.R with agents defined in agents.R

pacman::p_load(tidyverse, here)
source(here::here("src/agents.R"))
source(here::here("src/game.R"))

n_trials <- 120 
n_games <- 50 

# set seed for reproducibility
set.seed(2502)

# trial number, choice, feedback, value, agent_self_type, agent_other_type, role, combination, game_id
simulate_games <- function(n_trials, n_games){
    # define LR for RL agents
    learning_rate = 0.2

    # init games df
    games_df <- data.frame()

    # play WSLS-WSLS
    print("Playing WSLS-WSLS")
    for (i in 1:n_games){
        dfs <- play_game_WSLS(n_trials)

        hider_df <- dfs[[1]]
        picker_df <- dfs[[2]]

        # combine hider_df and picker_df
        combined_df <- bind_rows(hider_df, picker_df)

        # add game_id
        combined_df["game_id"] <- i
        
        # add combination
        combined_df["combination"] <- "WSLS_WSLS"
        
        # add to main games df
        games_df <- bind_rows(games_df, combined_df)
    }

    print("Playing RL-RL")
    for (i in 1:n_games){
        dfs <- play_game_RL(n_trials, learning_rate=learning_rate)

        hider_df <- dfs[[1]]
        picker_df <- dfs[[2]]

        # combine hider_df and picker_df
        combined_df <- bind_rows(hider_df, picker_df)

        # add game_id
        combined_df["game_id"] <- i
        
        # add combination
        combined_df["combination"] <- "RL_RL"
        
        # add to main games df
        games_df <- bind_rows(games_df, combined_df)
    }
    print("Playing RL-WSLS")
    for (i in 1:n_games){
        dfs <- play_game_RL_WSLS(n_trials, learning_rate=learning_rate)

        hider_df <- dfs[[1]]
        picker_df <- dfs[[2]]

        # combine hider_df and picker_df
        combined_df <- bind_rows(hider_df, picker_df)

        # add game_id
        combined_df["game_id"] <- i
        
        # add combination
        combined_df["combination"] <- "RL_WSLS"
        
        # add to main games df
        games_df <- bind_rows(games_df, combined_df)
    }
    
    return(games_df)

}

games_df <- simulate_games(120, 100)
#print(games_df %>% filter(combination == "WSLS_WSLS") %>% head())
#print(games_df %>% filter(combination == "RL_RL") %>% head())
#print(games_df %>% filter(combination == "RL_WSLS") %>% head())

# save to csv
savefile <- file.path("out", "data_lr_02.csv")
write_csv(games_df, savefile)