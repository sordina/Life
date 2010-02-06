# Conway's [Game of Life](http://en.wikipedia.org/wiki/Conway's_Game_of_Life) _in Haskell_

* Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
* Any live cell with more than three live neighbours dies, as if by overcrowding.
* Any live cell with two or three live neighbours lives on to the next generation.
* Any dead cell with exactly three live neighbours becomes a live cell.

***

Implementing Conway's classic game of life in haskell using the Opengl and GLUT libraries.

Core logic is performed by the successor function:

> game = iterate successor initial_frame

***

## To-do:

* Serialize and de-serialize state
* Change cell-state with mouse
* 3D version
* 3D camera options
* Colour
* Fuzzy logic
* Optimization
* Package as a screen-saver
