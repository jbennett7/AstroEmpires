# Astro Empires R Program


Data Rdata structure is a set of lists
1. Current
2. Structures
3. Tables
4. Technologies

## What does it do?
input.R - several functions for inputing data into the Data structure.  
functions.R - experimental not sure yet what it will do.  
config - configuration file. Right now just holds the path to the Data structure
Rdata file.  
utils.R - utility functions for aiding the other stuff.  

The `Data` data structure:
* __Current__ - General current information from the accout.
* __Structures__ - Level tables for the structures.
* __Tables__ - General infromation from the game.
* __Technologies__ - Level tables for technologies.

Energy Technology benefit calculation:
```
round(sum(c(initial.energy=2,solar.plants,gas.plants,fusion.plants,
             antimatter.plants,orbital.plants)*energy.technology.bonus))
```
