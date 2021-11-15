# Having fun with Sketch

## Setup

Make sure .asd files in the current directory can be loaded by ASDF:

    * (pushnew '*default-pathname-defaults* asdf:*central-registry*)
    #P"/Users/matteolandi/Workspace/sketch-fun/build/setup.lisp"

Load the main system:

    * (ql:quickload "sketch-fun")
    To load "sketch-fun":
      Load 1 ASDF system:
        sketch-fun
    ; Loading "sketch-fun"

    ("sketch-fun")

## Experiments

### Quadtree

Load the system:

    * (ql:quickload "sketch-fun/quadtree")
    To load "sketch-fun/quadtree":
      Load 1 ASDF system:
        sketch-fun/quadtree
    ; Loading "sketch-fun/quadtree"
    ...
    ("sketch-fun/quadtree")

Start the experiment:

    * (quadtree:start)
    #<QUADTREE #x30200773D4ED>
