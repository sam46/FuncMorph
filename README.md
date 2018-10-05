# FuncMorph

You can create fancy looking things and linearly interpolate between them to make animations and gifs.. like you're directing a movie (not that I know what it's like XD)  
And, because this is haskell, everything is a one-liner!

#### Work in progress! Contributions are welcome!
## Dependencies
- [IHaskell](https://github.com/gibiansky/IHaskell)  (optional -- brings Jupyter Notebooks to Haskell)
- [Diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/)
- [Cairo](https://hackage.haskell.org/package/cairo)

## Examples
```Haskell
draw $ map (squiggly 3 10) (stdn 100)
``` 
![1.gif](https://github.com/sam46/FuncMorph/blob/master/gallery/1.png)
  

```Haskell
draw $ map (squiggly 15 3) (stdn 200)
``` 
![2.gif](https://github.com/sam46/FuncMorph/blob/master/gallery/2.png)
  

```Haskell
diagram $ draw $ map (squiggly 60 2) (stdn 300)
``` 
![3.gif](https://github.com/sam46/FuncMorph/blob/master/gallery/3.png)
  

```Haskell
draw $ map (squiggly 60 10) (stdn 300)
``` 
![4.gif](https://github.com/sam46/FuncMorph/blob/master/gallery/4.png)


```Haskell
shot3 = map (\x -> (rad*cos(2*pi*x), rad*sin(2*pi*x))) std -- circle
shot4 = rotatePts (pi / 4) shot3 -- rotate
shot5 = lerpPts 0.35 shot4 $ rotatePts (pi / 4) $ map squarify shot3 -- squircle
shot6 = map (squiggly 15 3) std
shot7 = rotatePts (-pi / 4) $ lerpPts 1.5 shot3 $ map squarify shot3 -- diamond
play [(shot3,2), (shot4,2), (shot5,2), (shot5,2), (shot6,2), (shot7,2)]
``` 
![anim.gif](https://github.com/sam46/FuncMorph/blob/master/gallery/anim.gif)

## Docs
Check out the accompanying [notebook](https://github.com/sam46/FuncMorph/blob/master/fmorph.ipynb)

#### Like what you see? 
Pythonistas can check out a [similar work in Python](https://github.com/sam46/Morph)

