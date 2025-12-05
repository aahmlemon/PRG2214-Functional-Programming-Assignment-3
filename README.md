# Tile Adventure Game (Haskell)

A terminal-based, turn-based dungeon crawler written entirely in Haskell. This project demonstrates functional programming concepts including custom Type Classes, Monoids for state management, and a custom Monad for statistical data analysis.

## Demo


https://github.com/user-attachments/assets/73427f8c-1d69-413c-a4ec-a5038010d49d


## 🎮 Features

* **Turn-Based Movement:** Navigate through grid-based maps.
* **Enemy AI:** Enemies patrol specific axes and reverse direction upon hitting walls.
* **Combat System:** Use bombs to clear enemies from adjacent tiles.
* **Multiple Levels:** Support for map transitions and progression.
* **Data Persistence:** Saves game statistics (moves, kills, score) to a CSV file.
* **Statistical Analysis:** In-game menu to analyze player performance using custom monadic calculations.

## 📋 Prerequisites

To build and run this project, you need:

1.  **GHC (Glasgow Haskell Compiler)**
2.  **Cabal** (Common Architecture for Building Applications and Libraries)

## 📦 Dependencies

The game relies on the following Haskell packages. You can install them using Cabal:

```bash
cabal install --lib text ansi-terminal directory tabular
```

* `text`: For efficient string manipulation.
* `ansi-terminal`: For screen clearing and cursor positioning.
* `directory`: For checking file existence and deletion.
* `tabular`: For rendering ASCII tables for high scores.

## ⚙️ Setup & Installation

**Crucial Step:** Because the code looks for map files in a specific directory structure, you must set up the folders exactly as described below or the game will crash.

1.  **Create the Project Directory:**
    ```bash
    mkdir TileAdventure
    cd TileAdventure
    ```

2.  **Create the Source File:**
    Save the Haskell code as `Main.hs` in the `TileAdventure` folder.

3.  **Create the Map Directory:**
    The game expects maps to be located in an `app/` folder.
    ```bash
    mkdir app
    ```

4.  **Create Map Files:**
    Create four text files (`map1.txt`, `map2.txt`, `map3.txt`, `map4.txt`) inside the `app/` folder.

    *Example content for `app/map1.txt`:*
    ```text
    ##########
    #@       #
    #   V    #
    #   #    #
    #   S    #
    #   O    #
    ##########
    ```

    *Note: Ensure maps are rectangular and completely enclosed by `#` walls.*

## 🚀 How to Run

You can run the game using `runghc` (interpreted) or by compiling it.

**Option 1: Run Interpreted**
```bash
runghc Main.hs
```

**Option 2: Compile and Run**
```bash
ghc -O2 Main.hs -o tilegame
./tilegame
```

## 🕹️ Controls & Legend

### Movement & Actions
The game is menu-driven via the console. Enter the number corresponding to the action:

| Input | Action | Description |
| :---: | :--- | :--- |
| **1** | Move Up | Move player North |
| **2** | Move Right | Move player East |
| **3** | Move Down | Move player South |
| **4** | Move Left | Move player West |
| **5** | Wait | Skip turn (Enemies still move) |
| **6** | Bomb Up | Destroy enemy above |
| **7** | Bomb Right | Destroy enemy to right |
| **8** | Bomb Down | Destroy enemy below |
| **9** | Bomb Left | Destroy enemy to left |

### Map Legend
| Symbol | Meaning |
| :---: | :--- |
| `@` | **Player** |
| `#` | **Wall** (Impassable) |
| `V` | **Enemy** (Moving Vertically) |
| `^` | **Enemy** (Moving Vertically) |
| `<` | **Enemy** (Moving Horizontally) |
| `>` | **Enemy** (Moving Horizontally) |
| `S` | **Score** (Collect for points) |
| `B` | **Bomb** (Collect for ammo) |
| `O` | **Transition** (Go to next map) |
| `E` | **End Game** (Finish line) |

## 🧠 Technical Highlights

* **Type Classes:** `Renderable`, `Movable`, and `Collidable` are used to treat Player and Enemies polymorphically.
* **Monoid:** `GameStats` is an instance of `Monoid`, allowing easy concatenation of game statistics using `<>`.
* **Custom Monad:** `MonadStats` is a custom Algebraic Data Type with instances for `Functor`, `Applicative`, and `Monad`. It handles data analysis tasks, encapsulating success (`StatsValue`) or failure (`StatsError`) logic during CSV parsing.
* **IO Management:** Uses `Data.Text.IO` for performant file reading/writing and strict I/O operations.
