# turducken

Exposes the `Turducken` data type, which is a modified binary tree containing "group" nodes associated with a list of `Turducken`s instead of a value. The `drawTurducken` function is also provided to print `Turducken`s to the terminal:


```haskell
λ: basicTurducken = Node "Turkey" (Group [(Node "Duck" (Group [(Node "Chicken") Empty Empty] Empty Empty) Empty)] Empty Empty) Empty
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


`Turducken` is a member of the `Semigroup`, `Monoid`, `Functor`, `Applicative`, `Monad`, `Foldable`, and `Traversable` typeclasses:

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
