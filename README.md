# Turducken

Exposes the `Turducken` data type, which is a modified binary tree containing "group" nodes associated with a list of `Turducken`s in addition to a value. The `drawTurducken` function is also provided to print `Turducken`s to the terminal:


```haskell
λ: basicTurducken = Node "Turkey" (Group "Group 1" [(Node "Duck" (Group "Group 2" [(Node "Chicken") Empty Empty] Empty Empty) Empty)] Empty Empty) Empty
λ: putStrLn $ (++) "\n" $ drawTurducken $ basicTurducken

Turkey
|
`- |````````````````````|
   |
   |  Duck
   |  |
   |  `- |```````````|
   |     |
   |     |  Chicken
   |     |___________|
   |____________________|

```


`Turducken` is a member of the `Functor`, `Foldable`, and `Traversable` typeclasses. Additionally, if the turducken's group type is an instance of `Monoid`, that turducken is also a `Semigroup`, `Monoid`, `Applicative` and `Monad`:

```haskell
λ: putStrLn $ (++) "\n" $ drawTurducken $ basicTurducken <> basicTurducken

|`````````````````````````````|
|
|  Turkey
|  |
|  `- |````````````````````|
|     |
|     |  Duck
|     |  |
|     |  `- |```````````|
|     |     |
|     |     |  Chicken
|     |     |___________|
|     |____________________|
|
|  Turkey
|  |
|  `- |````````````````````|
|     |
|     |  Duck
|     |  |
|     |  `- |```````````|
|     |     |
|     |     |  Chicken
|     |     |___________|
|     |____________________|
|_____________________________|


λ: putStrLn $ (++) "\n" $ drawTurducken $ basicTurducken >>= (\x -> Node x (Node "Salt" Empty Empty) (Node "Pepper" Empty Empty))

Turkey
|
`- Pepper
|
`- |``````````````````````|
   |
   |  Salt
   |
   |  Duck
   |  |
   |  `- Pepper
   |  |
   |  `- |`````````````|
   |     |
   |     |  Salt
   |     |
   |     |  Chicken
   |     |  |
   |     |  `- Pepper
   |     |  |
   |     |  `- Salt
   |     |_____________|
   |______________________|

```

## Commands

```sh
stack test # runs the tests
stack build # builds the project
```
