# speedy - A simple speedrun timer

## Installation

1. Get the nix package manager
2. `git clone https://github.com/talanis85/speedy.git`
3. `cd speedy`
4. `nix-shell`
5. `cabal build`

## Usage

For each run, create a directory containing a file called
`def`. This is the definition of the run containing game
name, goal and splits in JSON format. See the supplied
`example.def`.

Then, cd to that directory and run `speedy`. There are exactly
two keyboard commands:

`<SPACE>` starts the run and advances to the next split.
`<ESCAPE>` skips over the current split.

These keybindings will be registered globally, so you can
use them whatever window has focus.
