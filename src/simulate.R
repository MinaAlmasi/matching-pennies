pacman::p_load(tidyverse, here)
source(here::here("src/agents.R"))

play_game_WSLS <- function(n_trials){
    # init arrays
    choices_hider <- array(NA, n_trials)
    feedback_hider <- array(NA, n_trials)

    choices_picker <- array(NA, n_trials)
    feedback_picker <- array(NA, n_trials)

    # init first trial vals for both agents in 
    choices_hider[1] <- rbinom(1,1,0.5)
    choices_picker[1] <- rbinom(1,1,0.5)

    feedback_hider[1] <- ifelse(choices_hider[1] != choices_picker[1], 1, 0) 
    feedback_picker[1] <- ifelse(choices_hider[1] == choices_picker[1], 1, 0)
    
    for (i in 2:n_trials){
        # get choices for hider and picker
        choices_hider[i] <- WSHIFTLSTAY_Agent(previous_choice = choices_hider[i-1], feedback = feedback_hider[i-1]) 
        choices_picker[i] <- WSHIFTLSTAY_Agent(previous_choice = choices_picker[i-1], feedback = feedback_picker[i-1])

        # get feedback for the hider and picker (same as init first trials)
        feedback_hider[i] <- ifelse(choices_hider[i] != choices_picker[i], 1, 0)
        feedback_picker[i] <- ifelse(choices_hider[i] == choices_picker[i], 1, 0)

    }

    # bind the informaion to dataframes
    hider_df <- data.frame(choices_hider, feedback_hider)
    picker_df <- data.frame(choices_picker, feedback_picker)

    return(list(hider_df, picker_df))

}

# play 
dfs <- play_game_WSLS(10)

hider_df <- dfs[[1]]
picker_df <- dfs[[2]]

print(hider_df)
print(picker_df)

