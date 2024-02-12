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

play_game_RL <- function(n_trials) {
    # init arrays
    values_hider <- array(NA, n_trials)
    choices_hider <- array(NA, n_trials)
    feedback_hider <- array(NA, n_trials)

    values_picker <- array(NA, n_trials)
    choices_picker <- array(NA, n_trials)
    feedback_picker <- array(NA, n_trials)

    # init first trial vals for both agents
    values_hider[1] <- 0
    values_picker[1] <- 0
    
    choices_hider[1] <- rbinom(1,1,0.5) # 0.5 = sigmoid(0)
    choices_picker[1] <- rbinom(1,1,0.5)

    feedback_hider[1] <- ifelse(choices_hider[1] != choices_picker[1], 1, 0)
    feedback_picker[1] <- ifelse(choices_hider[1] == choices_picker[1], 1, 0)

    for (i in 2:n_trials){
        # make agents
        hider <- REINFORCEMENT_Agent(previous_choice = choices_hider[i-1], previous_value = values_hider[i-1], feedback = feedback_hider[i-1], alpha = 0.1)
        picker <- REINFORCEMENT_Agent(previous_choice = choices_picker[i-1], previous_value = values_picker[i-1], feedback = feedback_picker[i-1], alpha = 0.1)

        # get choices for hider and picker
        choices_hider[i] <- hider[[1]]
        choices_picker[i] <- picker[[1]]

        # get value 
        values_hider[i] <- hider[[2]]
        values_picker[i] <- picker[[2]]

        feedback_hider[i] <- ifelse(choices_hider[i] != choices_picker[i], 1, 0)
        feedback_picker[i] <- ifelse(choices_hider[i] == choices_picker[i], 1, 0)
    }
    # bind the informaion to dataframes
    hider_df <- data.frame(choices_hider, feedback_hider, values_hider)
    picker_df <- data.frame(choices_picker, feedback_picker, values_picker)

    return(list(hider_df, picker_df))
}