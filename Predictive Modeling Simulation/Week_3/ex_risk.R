# Risk game aspect
# Attacker player throws a 5, 3 and 3

# As defender you may throw with one or 2 dice.

# You order the dice from high to low and compare them with those of the attacker. 
# If your throw is equal or higher than the one of the attacker, the attacker loses 1 army, otherwise you loose 1 army. 
# The same is done for the possible second die.

# Calculate the best option by repeatedly doing draws.


# Number of reps
n <- 50000

set.seed(1234)

# Metrics
balance1die <- numeric(n)
balance2dice <- numeric(n)

# Attack roll
attackRoll <- c(6,3,3)

for (i in 1:n)
{
    # Defender roll
    defenderRoll <- as.integer(runif(2, 1, 7))
    defenderRollSorted <- sort(defenderRoll, decreasing = TRUE)
    
    # Single die
    if (defenderRoll[1] >= attackRoll[1]) balance1die[i] = 1 else balance1die[i] = -1
    
    # Two dice
    # First die
    if (defenderRollSorted[1] >= attackRoll[1]) balance2dice[i] = 1 else balance2dice[i] = -1
    
    # Second die
    if (defenderRollSorted[2] >= attackRoll[2])
      balance2dice[i] = balance2dice[i] + 1 else 
      balance2dice[i] = balance2dice[i] -1
}

# Outcome metrics
meanBalance1Die <- mean(balance1die)
meanBalance2Dice <- mean(balance2dice)

meanBalance1Die
meanBalance2Dice

