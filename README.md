# Reinforcement Learning Assignment 3: Gridworld with SARSA & Q-learning

Author: Xiaoyan Jiang  
Date: August 2025

---

## Project Overview

This project implements and compares two reinforcement learning algorithms, **SARSA** and **Q-learning**, in a custom 5x5 Gridworld environment. The agent learns to navigate from a fixed start state to terminal goal states while avoiding trap states with heavy penalties. The main objectives are to learn optimal policies and compare trajectories and reward performance between the two algorithms.

---

## Files

- `rla3.R` — R script implementing the environment, SARSA and Q-learning algorithms, policy extraction, and plotting of trajectories and reward curves.  
- `RLa3.pdf` — Detailed project report including problem setup, methodology, results, and discussion.

---

## Environment Setup

- Grid size: 5 rows × 5 columns
- Start state: (1,1) (bottom-left)
- Terminal states: (1,5) and (5,5)
- Trap (red) states: (1,3), (2,3), (4,3), (5,3)
- Actions: Up, Down, Left, Right with equal movement cost (-1)
- Moving into trap resets agent to start with penalty (-20)
- Moving off-grid is penalized (-1) and agent stays put

---


## Code Structure Overview

- Section 1: Define environment and grid settings  
- Section 2: Define state transitions and reward function  
- Section 3: ε-greedy action selection helper  
- Section 4: Initialize Q-table  
- Section 5: SARSA training algorithm  
- Section 6: Q-learning training algorithm  
- Section 7: Extract greedy policy trajectories  
- Section 8: Run experiments  
- Section 9: Visualize learned trajectories  
- Section 10: Plot cumulative rewards

---




## How to Run

1. **Prerequisites:**  
   - R (version ≥ 4.0 recommended)  
   - R packages: `ggplot2`, `dplyr`, `tidyr`, `reshape2` (if not installed, install via `install.packages()`)

2. **Run the code:**  
   Open R or RStudio, then run the following command in the console:

   ```r
   source("gridworld_rl.R")
