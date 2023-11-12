# Puck-Man 
Developed by Jort Willemsen en Annemae van de Hoef 

# How To Play
To run the game use:

```stack build```

When running the game, you play as Puck-Man who you can control with the WASD keys. Your goal is to eat all the dots and adventure through the levels to get the highest score possible. Be sure to eat the energizers once in a while so you can take a break from running from the ghosts. 

Furthermore, you can go to the menu by pressing 'm', pause the game by pressing 'p', or view the current high score by pressing 'h'.

# Custom Levels 
There is an opportunity to create your own custom level. You can do this by adjusting one of the five .txt files in the level/custom folder. You can do this by using the following symbols:

    X - Wall 
    o - Floor with dot
    O - Floor with energizer 
    _ - Empty floor 
    G - Ghost spawn point 
    P - Player spawn point
    S - Scatter spawn point (this is where a ghost will flee to when frightened)
    T - Trapdoor which ghosts can use to leave their ghost house. 

It is important to follow the following rules when creating a custom level:

1. The level must be a rectangle whose outer edge consists entirely of X's.
2. There should be at least one player spawn (P), one trap door (T), four ghost spawns (G), and four scatter spawns (S) in the level.
3. Make sure there are no white spaces around your maze.

