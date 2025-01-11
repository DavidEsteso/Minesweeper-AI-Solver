# Haskell Minesweeper

An implementation of the classic Minesweeper game written in Haskell, featuring both traditional gameplay mechanics and advanced AI-driven solving capabilities. This project combines functional programming principles with an intuitive web-based interface, delivering a modern take on the beloved puzzle game.

## Features

- **Game Engine**: Fully functional game logic implemented in pure Haskell, ensuring type safety and immutable state management.
- **Intelligent Solver**: AI solver that employs multiple strategies including pattern recognition and probability-based decision making.
- **Interactive Web Interface**: Clean and responsive GUI built with Threepenny-GUI, replicating the classic Windows Minesweeper aesthetic.
- **Performance Tracking**: Dual-mode tracking system that monitors both elapsed time and number of moves.
- **Enhanced Gameplay Mechanics**: Support for all traditional Minesweeper operations including chord clicking and cell marking.
- **Real-time State Management**: G state handling with visual feedback.

## Skills Showcased
- Functional programming with Haskell
- Recursion and higher-order functions
- GUI development using Threepenny-GUI
- Data structures and pure functions
- Testing and debugging game logic
- Uses monads for concise management of IO and game state transitions.
- Employs efficient abstraction layers to handle real-time UI updates without compromising performance.

## Technical Overview

The project is architected around three core modules, each handling distinct aspects of the game:

### Core Game Engine (`Minesweeper.hs`)
The foundation of the game, implementing:
- Game state management using Haskell's type system.
- Pure functional board manipulation.
- Mine generation algorithm ensuring first-click safety. According to Wikipedia, "In some variants the first click is guaranteed to be safe, and some further guarantee that all adjacent cells are safe as well"1. This version ensures that the first click and its neighboring cells are safe.
- Cell revealing logic. 

### AI Solver (`MinesweeperSolver.hs`)
A solving engine featuring:
- Multiple solving strategies including:
    - Constraint satisfaction algorithms
    - Pattern recognition systems
    - Probability-based risk assessment

### User Interface (`Main.hs`)
A web-based interface implemented with Threepenny-GUI, providing:
- Responsive and intuitive user interactions.
- Classic Windows-style visual design.
- Real-time game state visualization.

## Prerequisites

The project requires the following environment setup:
- **GHC (Glasgow Haskell Compiler)** 8.10 or higher
- **Cabal 3.0+** or **Stack 2.7+**
- Required Haskell packages:
    - `threepenny-gui >= 0.9`
    - `containers >= 0.6`
    - `random >= 1.2`

### Using Stack

#### Setup the project:
- stack setup
- stack build

#### Launch the Game
- stack run
- The game will be accessible through your web browser at http://localhost:8023.

#### Difficulty Options
- You can execute the game with different difficulty levels or custom dimensions using the following commands:

- Easy (default constants)
        - stack run
- Specific difficulty levels
        - stack run Easy    # Uses default constants
        - stack run Medium  # 16x16 grid with 40 mines
        - stack run Hard    # 30x16 grid with 99 mines
- Custom dimensions
        - stack run 20 15 45  # width=20, height=15, mines=45

## Gameplay 
### Basic Controls
- Left Mouse Button on a cell: Reveal a cell.
- Left Mouse Button on a number: Perform a chord click (when the number of surrounding flags equals the cell number)
- Right Mouse Button: Cycle between flag and question mark.


### Interface Elements
- 🙂 Button: Reset the game/Start new game.
- 🤖 Button: Request AI move suggestion.
- Timer Display: Toggle between time and moves counter.
- Mine Counter: Displays remaining mines/flags. When there are as many flags as hidden mines, the cell turns blue and becomes clickable to check if the flags are correct. The game automatically determines if you win or lose.

## Acknowledgments
- Based on the classic Windows Minesweeper game.
- Built using Haskell and functional programming principles.
- Uses Threepenny-GUI for the web interface.

