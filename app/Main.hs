module Main where

import System.Environment
import Data.List

data Zet = X | O
data Vak = Bezet Zet | Leeg
data WisselVak = Success [Vak] | Fail String [Vak]

instance Show Zet where
  show X = "X"
  show O = "O"

instance Show Vak where
  show (Bezet X) = "X"
  show (Bezet O) = "O"
  show Leeg = " "

instance Eq Vak where
  Bezet X == Bezet X = True
  Bezet O == Bezet O = True
  Leeg == Leeg = True
  _ == _ = False

maakRij :: [Vak] -> String
maakRij rij = intercalate " | " $ fmap show rij

maakBord :: [Vak] -> IO ()
maakBord bord = do
  putStrLn $ maakRij eersteRij
  putStrLn "----------"
  putStrLn $ maakRij tweedeRij
  putStrLn "----------"
  putStrLn $ maakRij derdeRij
  where eersteRij  = take 3 bord
        tweedeRij = drop 3 . take 6 $ bord
        derdeRij  = drop 6 bord

getBordNummer :: String -> Maybe Int
getBordNummer "A1" = Just 0
getBordNummer "A2" = Just 1
getBordNummer "A3" = Just 2
getBordNummer "B1" = Just 3
getBordNummer "B2" = Just 4
getBordNummer "B3" = Just 5
getBordNummer "C1" = Just 6
getBordNummer "C2" = Just 7
getBordNummer "C3" = Just 8
getBordNummer _    = Nothing

isVakLeeg ::  [Vak] -> Int -> Maybe Int
isVakLeeg bord nummer = if bord !! nummer == Leeg then Just nummer else Nothing

setVak :: String -> Zet -> [Vak] -> WisselVak
setVak vaknummer zet bord =
  case getBordNummer vaknummer >>= isVakLeeg bord of
    Nothing -> Fail "ongeldige zet" bord
    Just i -> Success ((take i bord) ++ [Bezet zet] ++ (drop (i+1) bord))

volgendeZet :: Zet -> Zet
volgendeZet X = O
volgendeZet O = X

isWinnaar :: Zet -> [Vak] -> Bool
isWinnaar zet bord =
  or [
    -- check top rij
    bord !! 0 == (Bezet zet) && bord !! 1 == (Bezet zet) && bord !! 2 == (Bezet zet),
    -- check middelste rij
    bord !! 3 == (Bezet zet) && bord !! 4 == (Bezet zet) && bord !! 5 == (Bezet zet),
    -- check onderste rij
    bord !! 6 == (Bezet zet) && bord !! 7 == (Bezet zet) && bord !! 8 == (Bezet zet),
    -- check linker kolom
    bord !! 0 == (Bezet zet) && bord !! 3 == (Bezet zet) && bord !! 6 == (Bezet zet),
    -- check middelste kolom
    bord !! 1 == (Bezet zet) && bord !! 4 == (Bezet zet) && bord !! 7 == (Bezet zet),
    -- check rechter kolom
    bord !! 2 == (Bezet zet) && bord !! 5 == (Bezet zet) && bord !! 8 == (Bezet zet),
    -- check schuin eerste bovenste vak
    bord !! 0 == (Bezet zet) && bord !! 4 == (Bezet zet) && bord !! 8 == (Bezet zet),
    -- check schuin eerste onderste vak
    bord !! 6 == (Bezet zet) && bord !! 4 == (Bezet zet) && bord !! 2 == (Bezet zet)
  ]

isGelijkspel :: [Vak] -> Bool
isGelijkspel bord = elem Leeg bord

speelRonde :: Zet  -> [Vak] -> IO ()
speelRonde zet bord = do
  putStrLn $ (show zet) ++ " 's beurt."
  putStrLn "Kies vak"
  putStrLn "A1 | A2 | A3"
  putStrLn "B1 | B2 | B3"
  putStrLn "C1 | C2 | C3\n"
  maakBord bord
  putStr "\nKies Vak: "
  vak <- getLine
  case setVak vak zet bord of
    Fail err bord -> do
      putStrLn err
      speelRonde zet bord
    Success nieuwBord -> do
      if isWinnaar zet nieuwBord then do
          putStrLn $ ((show zet) ++ " heeft gewonnen!")
          maakBord nieuwBord
          return ()
      else if isGelijkspel nieuwBord == False then do
          putStrLn $ ("Gelijkspel!")
          maakBord nieuwBord
          return ()
      else speelRonde (volgendeZet zet) nieuwBord

main :: IO ()
main = do
  putStrLn $ "Het spel wordt gestart"
  let nieuwBord = replicate 9 Leeg
  speelRonde X nieuwBord
