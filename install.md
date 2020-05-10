# Installation Instructions

Make sure you have ocaml, opam, ocamlfind and pkg-config installed using a 
package manager like Homebrew before running the following:

To install the dependencies you can run

 **make install**

to install the packages:

- yojson
- ANSITerminal
- oUnit
- Graphics

all in one.

For the graphical interface of the game, it is necessary to set the DISPLAY 
variable in your environment. To check what value the DISPLAY variable is set 
to, run

  **echo $DISPLAY** 

For Mac users, we recommend installing XQuartz from https://www.xquartz.org 
and playing the game from the xterm terminal. 
