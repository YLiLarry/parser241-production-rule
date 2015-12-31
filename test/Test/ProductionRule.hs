module Test.ProductionRule where

import Test.Hspec
import Parser241.Parser.ProductionRule
import Parser241.Parser.ProductionRule.Internal (Symbol(..))
import Data.Set as S (Set(..), fromList)
import Data.Map as M (fromList)
import Control.Arrow (second)

data TestNT = A0 | B0 | C0 deriving (Eq, Show, Ord)


tableA :: [Rule TestNT]
tableA = rules $ do {
      start --> A0 & B0 & C0
        |> A0 & B0
        |> null
    ; A0 --> B0 & C0 & A0
          |> A0 & C0
          |> null
  }
  where
    start = Start :: Symbol TestNT
    null = Null   :: Symbol TestNT


data MySym = A
           | B
           | C'
         deriving (Eq, Show, Ord)

tableB :: [Rule MySym]
tableB = rules $ do
    start --> A & C' & B  -- AC'B concatenation
           |> A
           |> C'

      ; A --> B
           |> null
           |> A & C'

      ; B --> C'
  where
    start = Start :: Symbol MySym
    null  = Null  :: Symbol MySym

tableC :: [Rule MySym]
tableC = rules $
   start --> null & C'
          |> C'
  where
    start = Start :: Symbol MySym
    null  = Null  :: Symbol MySym

ignoreOrder :: (Ord a) => [Rule a] -> Set (Symbol a, Set [Symbol a])
ignoreOrder = S.fromList . map (S.fromList `second`)

test :: IO ()
test = hspec $ do

   describe "sampleTableA" $
      it "rules" $
         ignoreOrder tableA `shouldBe` ignoreOrder [
              (Start, [
                        [NT A0, T B0, T C0, EOF],
                        [NT A0, T B0, EOF],
                        [Null, EOF] ])
            , (NT A0, [
                        [T B0, T C0, NT A0],
                        [NT A0, T C0],
                        [Null] ])
         ]

   describe "sampleTableB" $
      it "rules" $
         ignoreOrder tableB `shouldBe` ignoreOrder [
              (Start, [ [NT A, T C', NT B, EOF],
                        [NT A, EOF],
                        [T C', EOF] ] )
            , (NT A, [ [NT B],
                       [NT A, T C'],
                       [Null] ])
            , (NT B, [ [T C'] ])
         ]

   describe "sampleTableC" $
      it "rules" $
         ignoreOrder tableC `shouldBe` ignoreOrder [(Start, [ [Null, T C', EOF], [T C', EOF] ])]

