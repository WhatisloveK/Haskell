import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Exception
import Control.Monad (replicateM)

_Σ = ['a', 'b']       -- alphabet
_S = ['A', 'B', 'C']  -- states
s_0 = 'A'             -- initial state
_F = ['C']            -- final states
delta :: Map Char (Map Char Char)
delta = Map.fromList [('A', Map.fromList[('a', 'B')]),
                      ('B', Map.fromList[('a', 'C')]),
                      ('C', Map.fromList[('a', 'A')])]
eat :: Char -> String -> [Char] -> [Char]
eat curr_state word reachedStates = reachedStates ++ (foldl (\state letter -> delta ! state ! letter) curr_state word)

infi l v w = infi' 0 l v w 
infi' n l v w = do
  if n > l then return ()
  else do
    sequence_ $ map (\x -> do
      res <- try (evaluate $ eat s_0 (v ++ x ++ w ++ x)) :: IO (Either SomeException [Char])
      case res of
        Left ex -> return ()
        Right val -> if val == True
          then putStr $ "\n" ++ x ++ "\n"
          else return ()
      ) (replicateM n _Σ)
    infi' (n + 1) l v w

main = do 
  --infi 2 "ab" "cd"
   res <- try (evaluate $ eat s_0 (v ++ x ++ w ++ x)) :: IO (Either SomeException [Char])
      case res of
        Left ex -> return ()
        Right val -> if val == True
          then putStr $ "\n" ++ x ++ "\n"
          else return ()