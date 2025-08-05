# ======================================================
# Reinforcement Learning - Gridworld with SARSA & Q-learning
# Author: Xiaoyan Jiang
# Date: 2025-08-04
# Description: Implement a Gridworld problem with SARSA and Q-learning algorithms
# ======================================================

# ------------------------------
# 1. Define the Gridworld Environment
# ------------------------------

# Grid dimensions
n_row <- 5
n_col <- 5 

# Define key grid coordinates
start_state <- c(1, 1)
terminal_states <- list(c(1, 5), c(5, 5))
red_states <- list(c(1, 3), c(2, 3), c(4, 3), c(5, 3))

# Actions: up, down, left, right
actions <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
action_names <- c("U", "D", "L", "R")
n_actions <- length(actions)

# Function to check if a state is terminal
is_terminal <- function(state) {
  any(sapply(terminal_states, function(ts) all(ts == state)))
}

# Check if a state is red
is_red <- function(state) {
  any(sapply(red_states, function(rs) all(rs == state)))
}

# ------------------------------
# 2. Define Environment Transition and Reward
# ------------------------------

step <- function(state, action) {
  new_state <- state + actions[[action]]
  # Stay in bounds
  if (any(new_state < 1) || new_state[1] > n_row || new_state[2] > n_col) {
    return(list(state = state, reward = -1))  # invalid move
  }
  # Red square
  if (is_red(new_state)) {
    return(list(state = start_state, reward = -20))
  }
  # Terminal
  if (is_terminal(new_state)) {
    return(list(state = new_state, reward = -1))
  }
  # Regular move
  return(list(state = new_state, reward = -1))
}

# ------------------------------
# 3. Helper Functions
# ------------------------------

# ε-greedy action selection
select_action <- function(Q, state, epsilon) {
  if (runif(1) < epsilon) {
    return(sample(1:n_actions, 1))  # random
  }
  key <- paste(state, collapse = ",")
  best_actions <- which(Q[[key]] == max(Q[[key]]))
  return(sample(best_actions, 1))  # break ties randomly
}

state_key <- function(state) paste(state, collapse = ",")

# ------------------------------
# 4. Initialize Q-table
# ------------------------------

init_Q <- function() {
  Q <- list()
  for (i in 1:n_row) {
    for (j in 1:n_col) {
      key <- paste(i, j, sep = ",")
      Q[[key]] <- rep(0, n_actions)
    }
  }
  return(Q)
}

# ------------------------------
# 5. SARSA Implementation
# ------------------------------

run_sarsa <- function(episodes = 1000, alpha = 0.1, gamma = 0.95, epsilon = 0.1) {
  Q <- init_Q()
  rewards <- numeric(episodes)
  
  for (ep in 1:episodes) {
    state <- start_state
    a <- select_action(Q, state, epsilon)
    ep_reward <- 0
    
    for (step_i in 1:100) {
      res <- step(state, a)
      next_state <- res$state
      r <- res$reward
      ep_reward <- ep_reward + r
      
      if (is_terminal(next_state)) {
        Q[[state_key(state)]][a] <- Q[[state_key(state)]][a] +
          alpha * (r - Q[[state_key(state)]][a])
        break
      }
      
      a_next <- select_action(Q, next_state, epsilon)
      Q[[state_key(state)]][a] <- Q[[state_key(state)]][a] +
        alpha * (r + gamma * Q[[state_key(next_state)]][a_next] - Q[[state_key(state)]][a])
      
      state <- next_state
      a <- a_next
    }
    
    rewards[ep] <- ep_reward
  }
  
  return(list(Q = Q, rewards = rewards))
}

# ------------------------------
# 6. Q-learning Implementation
# ------------------------------

run_qlearning <- function(episodes = 1000, alpha = 0.1, gamma = 0.95, epsilon = 0.1) {
  Q <- init_Q()
  rewards <- numeric(episodes)
  
  for (ep in 1:episodes) {
    state <- start_state
    ep_reward <- 0
    
    for (step_i in 1:100) {
      a <- select_action(Q, state, epsilon)
      res <- step(state, a)
      next_state <- res$state
      r <- res$reward
      ep_reward <- ep_reward + r
      
      if (is_terminal(next_state)) {
        Q[[state_key(state)]][a] <- Q[[state_key(state)]][a] +
          alpha * (r - Q[[state_key(state)]][a])
        break
      }
      
      Q[[state_key(state)]][a] <- Q[[state_key(state)]][a] +
        alpha * (r + gamma * max(Q[[state_key(next_state)]]) - Q[[state_key(state)]][a])
      
      state <- next_state
    }
    
    rewards[ep] <- ep_reward
  }
  
  return(list(Q = Q, rewards = rewards))
}

# ------------------------------
# 7. Extract a Trajectory (Greedy Policy)
# ------------------------------

get_trajectory <- function(Q) {
  traj <- list()
  state <- start_state
  traj[[1]] <- state
  
  for (t in 2:100) {
    key <- state_key(state)
    a <- which.max(Q[[key]])
    res <- step(state, a)
    state <- res$state
    traj[[t]] <- state
    if (is_terminal(state)) break
  }
  
  return(do.call(rbind, traj))
}

# ------------------------------
# 8. Run Experiments and Plot
# ------------------------------

set.seed(123)
res_sarsa <- run_sarsa()
res_q <- run_qlearning()

traj_sarsa <- get_trajectory(res_sarsa$Q)
traj_q <- get_trajectory(res_q$Q)

# ------------------------------
# 9. Visualization
# ------------------------------

library(ggplot2)

plot_trajectory <- function(traj, title) {
  df <- as.data.frame(traj)
  colnames(df) <- c("x", "y")
  df$step <- 1:nrow(df)
  
  ggplot(df, aes(x = y, y = 6 - x)) +  # flip y for visual clarity
    geom_path(arrow = arrow(length = unit(0.15, "cm")), size = 1.2) +
    geom_point(aes(color = step), size = 3) +
    scale_color_gradient(low = "blue", high = "red") +
    theme_minimal() +
    labs(title = title, x = "Column", y = "Row") +
    coord_fixed()
}

plot_trajectory(traj_sarsa, "SARSA Learned Trajectory")
plot_trajectory(traj_q, "Q-Learning Learned Trajectory")

# ------------------------------
# 10. Compare Reward over Episodes
# ------------------------------

plot_rewards <- function(rewards1, rewards2) {
  df <- data.frame(
    episode = 1:length(rewards1),
    SARSA = rewards1,
    QLearning = rewards2
  )
  library(reshape2)
  df_melt <- melt(df, id.vars = "episode")
  
  ggplot(df_melt, aes(x = episode, y = value, color = variable)) +
    geom_line(alpha = 0.7) +
    geom_smooth(se = FALSE) +
    theme_minimal() +
    labs(title = "Episode Reward Comparison", x = "Episode", y = "Total Reward")
}

plot_rewards(res_sarsa$rewards, res_q$rewards)
