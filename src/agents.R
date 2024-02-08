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
    if (is.null(previous_value)){
        # set the zero as that is a 50/50 when put through sigmoid
        new_value_unscaled = 0
    }

    else {
        # compute previous correct choice from feeddback and previous choice (if feedback is 1, previous correct choice is previous choice, otherwise it is the opposite)
        previous_correct_choice = ifelse(feedback==1, previous_choice, 1-previous_choice)

        # calculate prediction error
        prediction_error = previous_correct_choice - previous_value

        # update value
        new_value_unscaled = previous_value + alpha * prediction_error
    }

    # put value through a sigmoid using the formula
    new_value = 1/(1+exp(-new_value_unscaled))

    # make choice based on value
    #choice = rbinorm(1, 1, new_value)
    choice = ifelse(new_value > 0.5, 1, 0)

    # return choice and value
    return(list(choice, new_value))
}

# testing the reinforcement agent
agent = REINFORCEMENT_Agent(previous_choice = 1, previous_value = 0.9, feedback = 1, alpha = 0.8)
print(agent[1])
print(agent[2])