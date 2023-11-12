# Puck-Man 
Developed by Jort Willemsen en Annemae van de Hoef 

# How To Play
To run the game use:

```stack build```

When running the game, you play as Puck-Man who you can control with the WASD keys. Your goal is to eat all the dots to up your score and advance to the next level. Try to beat your high score by scoring the most points with three lives. Make sure to eat an energizer once in a while to frighten the ghosts and give yourself some breathing room. 

## Controls
| Key  | Action |
|------|--------|
| W    | Go up  |   
| A    | Go left |
| S    | Go right |
| D    | Go down  |
| M    | Open Menu  |
| P    | Pause  |
|H    | Open High scores  |
| [1..5] | Open associated level |
| Ctrl + [1..5] | Open associated custom level |

# Custom Levels 
There is an opportunity to create your own custom level. You can do this by adjusting one of the 5 .txt files in the `level/custom` folder.

The file expects a visual representation of your level by using these symbols to indicate a tile in the maze:

    X - Wall 
    o - Floor with dot
    O - Floor with energizer 
    _ - Empty floor 
    G - Ghost spawn point 
    P - Player spawn point
    S - Scatter spawn point (this is where a ghost will flee to when frightened)
    T - Trapdoor which ghosts can use to leave their ghost house.

It is important to follow these rules when creating a custom level:

- 1. The level must always be a rectange (i.e. every line must have the same amount of symbols).
- 2. The outer egdes of the level must always consist of 'X' symbols.
- 3. There should be at least one player spawn (P), four ghost spawns (G), and four scatter spawns (S) in the level.
- 4. Make sure there are no white spaces around your level.

Using these tools you can very easily create something such as:

![image](https://github.com/JortWillemsen/FP-game/assets/32990828/17587645-5319-465c-979f-14d5b8fbfebe)

That represents:

![image](https://github.com/JortWillemsen/FP-game/assets/32990828/27945277-e41d-4335-a65e-b656c4fdce2e)




