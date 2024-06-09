# Quantik AI

## What is Quantik ?

Quantik is an abstract 1v1 board game. quantik.exe is a terminal version of this game, in which you play against my bot.

"On your turn, you will place a piece on an empty space of the board, respecting one single rule: You are not allowed to place a shape in a row, column, or region in which your opponent has a piece of the same shape. The first player to place the fourth different shape in a row, column, or region wins the game."

## How does this bot work ?

This is an implementation of the NegaMax algorithm (which is a variant of MinMax) with alpha–beta pruning, written in OCaml.

The bot thinks a few turns ahead, until the number of possible outcomes skyrockets. When it does, the bot tries to estimate its likelihood to win by using a Monte-Carlo-like heuristic. Its probabilistic approach is to calculate the winrate of the bot in multiple random endgames.

## Is this AI good at the game ?

Probably not, but it certainly plays way better than me.