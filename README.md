# Elmsweep

The game is a minesweeper clone created with Elm and elm-html, using
icons from Font Awesome.

## Play

http://erikdh.github.io/elmsweep/

Left and right mouse buttons to explore and flag. The top left buttons
(- = +) changes board size and the central smiley restarts the
game. Double click explores surrounding spaces when the number of
flags matches the number of surrounding mines.

## Build

elm-make Elmsweep.elm --yes --output elm.js
