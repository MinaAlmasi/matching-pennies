## script with code for agents ##
pacman::p_load(tidyverse)

# WIN SHIFT LOSE STAY AGENT
WSHIFTLSTAY_Agent <- function(previous_choice, feedback){
    # if we are in the first round (no previous choice and feedback), make a random selection
    if (is.null(feedback) & is.null(previous_choice)){
        choice = rbinorm(1, 1, 0.5)
    }
    # if we had a win last round, we do the opposite this round!
    if (feedback == 1){
        choice = 1 - previous_choice
    }

    # if we had a loss last round, we stay with the same choice!
    if (feedback == 0) {
        choice = previous_choice
    }

    return(choice)
}

# REINFORCEMENT LEARNING AGENT
REINFORCEMENT_Agent <- function(previous_choice, previous_value, feedback, alpha){
    # if we are in the first round, define expected value 
    if (is.na(previous_value)){
        # set the 0.5 as that is a 50/50
        new_value = 0.5
    }

    else {
        # compute previous correct choice from feedback and previous choice (if feedback is 1, previous correct choice is previous choice, otherwise it is the opposite)
        previous_correct_choice = ifelse(feedback==1, previous_choice, 1-previous_choice)

        # calculate prediction error
        prediction_error = previous_correct_choice - previous_value

        # update value
        new_value = previous_value + alpha * prediction_error
    }
  
    # make choice based on value
    #choice = rbinorm(1, 1, new_value)
    choice = ifelse(new_value > 0.5, 1, 0)

    # return choice and value
    return(list(choice, new_value))
}

# testing the reinforcement agent
agent = REINFORCEMENT_Agent(previous_choice = 0, previous_value = 0.5, feedback = 0, alpha = 0.5)
print(agent[1])
print(agent[2])