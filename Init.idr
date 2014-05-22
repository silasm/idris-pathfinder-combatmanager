module Main
  import Effect.Random
  import Effect.State
  import Effect.System

  roll : { [RND] } Eff m Int
  roll = Effects.pure $ fromInteger !(rndInt 1 20)

  total
  snoc : {a : Type} -> List a -> a -> List a
  snoc [] x = [x]
  snoc (y :: xs) x = y :: snoc xs x

  total
  rotate : {a : Type} -> List a -> List a
  rotate [] = []
  rotate (x :: xs) = snoc xs x

  data Player = P String Int
  instance Show Player where
    show (P x y) = "P " ++ " " ++ show x ++ " " ++ show y


  next : { [STATE (List (Player,Int))] } Eff m ()
  next = update rotate

  total
  addbetween : {a : Type} -> (a, Int) -> List (a, Int) -> List (a,Int)
  addbetween (a, b) [] = [(a,b)]
  addbetween (a, b) ((x,c)::xs) = if c < b
                                  then (x,c)::takeWhile ((<b) . snd) xs ++ [(a,b)]
                                               ++ dropWhile ((<b) . snd) xs
                                  else (x,c)::takeWhile ((>b) . snd) xs ++ [(a,b)]
                                               ++ dropWhile ((>b) . snd) xs

  add : Player -> { [STATE (List (Player,Int)), RND] } Eff m ()
  add p = do initiative <- roll
             update $ addbetween (p,initiative)

  initiate : { [STATE (List (Player,Int))] } Eff m ()
  initiate = put []

  game : Integer -> { [STATE (List (Player,Int)), RND, SYSTEM] } Eff m (List (Player,Int))
  game x = do initiate
              srand x
              add (P "Silas" 28)
              add (P "Lasiti" 23)
              add (P "Red Zombie" 22)
              next
              add (P "Blue Zombie" 23)
              next
              get

  main : IO ()
  main = do seedInt <- run time
            let seed = prim__zextInt_BigInt seedInt
            result <- run $ game seed
            print result
