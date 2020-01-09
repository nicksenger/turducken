# Turducken

Exposes the `Turducken` data type, which is a modified binary tree containing "group" nodes associated with a list of `Turducken`s in addition to a value. The `drawTurducken` function is also provided to print `Turducken`s to the terminal (requires group & node types be instances of the `Show` typeclass):

`````````````````````haskell
λ: basicTurducken = Node "Turkey" (Group "Outer Turducken" [(Node "Duck" (Group "Inner Turducken" [(Node "Chicken") Empty Empty] Empty Empty) Empty)] Empty Empty) Empty
λ: putStrLn $ (++) "\n" $ drawTurducken $ basicTurducken

"Turkey"
|
`- |`````"Outer Turducken"`````|
   |
   |  "Duck"
   |  |
   |  `- |"Inner Turducken"|
   |     |
   |     |  "Chicken"
   |     |_________________|
   |___________________________|

`````````````````````

`Turducken` is a member of the `Functor`, `Foldable`, and `Traversable` typeclasses. Additionally, if the turducken's group type is an instance of `Monoid`, that turducken is also a `Semigroup`, `Monoid`, `Applicative` and `Monad`:

``````````````````````````````haskell
λ: putStrLn $ (++) "\n" $ drawTurducken $ basicTurducken <> basicTurducken

|``````````````````""``````````````````|
|
|  "Turkey"
|  |
|  `- |`````"Outer Turducken"`````|
|     |
|     |  "Duck"
|     |  |
|     |  `- |"Inner Turducken"|
|     |     |
|     |     |  "Chicken"
|     |     |_________________|
|     |___________________________|
|
|  "Turkey"
|  |
|  `- |`````"Outer Turducken"`````|
|     |
|     |  "Duck"
|     |  |
|     |  `- |"Inner Turducken"|
|     |     |
|     |     |  "Chicken"
|     |     |_________________|
|     |___________________________|
|______________________________________|


λ: putStrLn $ (++) "\n" $ drawTurducken $ basicTurducken >>= (\x -> Node x (Node "Salt" Empty Empty) (Node "Pepper" Empty Empty))

"Turkey"
|
`- "Pepper"
|
`- |``````"Outer Turducken"``````|
   |
   |  "Salt"
   |
   |  "Duck"
   |  |
   |  `- "Pepper"
   |  |
   |  `- |"Inner Turducken"|
   |     |
   |     |  "Salt"
   |     |
   |     |  "Chicken"
   |     |  |
   |     |  `- "Pepper"
   |     |  |
   |     |  `- "Salt"
   |     |_________________|
   |_____________________________|

``````````````````````````````

## Commands

```sh
stack test # runs the tests
stack build # builds the project
```
