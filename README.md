# NES Atlantico Game

This is a small game I completed as part of the [NES Programming with 6502 Assembly](https://pikuma.com/courses/nes-game-programming-tutorial) series at [Pikuma.com](https://pikuma.com/)[^1]. All visual assets in the game are provided by [Pikuma.com](https://pikuma.com/), as well as reference code which serves as the basis of my own code.

My code follows the reference code from the series, although it diverges a little in the following areas...

* Modularity: Procedures are stored in separate files and loaded with `.include` commands [^2].
* Macro use: Redundant blocks of code are converted to macros whenever possible.

While the game is "complete" with respect to the course, I may continue to tidy up the code as time permits.

## Building

To assemble the code, simply execute the makefile (assuming you already have CA65 installed) using `make` in the root directory. **Note:** the makefile also executes a small Javascript program which generates labels used by the FCEUX's 6502 debugger. This can be commented out if no labels are needed/node isn't installed on your machine/etc.

To hear the game's audio, you will have to download the FamiStudio sound engine from [FamiStudio.org](https://famistudio.org/). Note that my code uses FamiStudio 4.0.0, as I had issues getting audio to play with the latest version of the engine.

## Running

Execute `make run` in the root folder.

## Footnotes

[^1]All visual + audio assets in the game are provided by [Pikuma.com](https://pikuma.com/), as well as reference code for the game.

[^2]I'm aware that CA65 supports more sophisticated modularization via `.import`/`.export` commands but I'm trying to keep things simple for now.
