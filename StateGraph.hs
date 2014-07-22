{-# LANGUAGE DeriveGeneric #-}
{- StateGraph.hs

A draft solution for manipulating state graphs.  A state graph is a
computing device consisting of a some states arranged in a graph
(suprise) whose edges are labelled with conditions and operations.-}

module StateGraph
       (
         Instruction (..)
       , Condition (..)
       , Transition (..)
       , newTransition
       , StateGraph (..)
       , newStateGraph
       , stateCount
       , toDotHighlight
       , toDot
       , toFileHighlight
       , toFile
       , fromFile
       , listRegs
       ) where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.MultiSet as MultiSet
import qualified Text.Dot as Dot
import Data.List
import Data.Hashable
import GHC.Generics (Generic)

-- | A register machine instruction (Korec).
data Instruction = Dec | Inc
                 deriving (Eq, Ord, Show, Read, Generic)
instance Hashable Instruction

-- | The condition to verify on a register.
data Condition = Zero | NotZero
                 deriving (Eq, Ord, Show, Read, Generic)
instance Hashable Condition

-- We need these instances to be able to make 'StateGraph' hashable.
instance Hashable a => Hashable (IntMap.IntMap a) where
  hashWithSalt s = (s `hashWithSalt`) . IntMap.assocs
instance (Hashable a, Hashable b) => Hashable (Map.Map a b) where
  hashWithSalt s = (s `hashWithSalt`) . Map.assocs
instance (Hashable a) => Hashable (MultiSet.MultiSet a) where
  hashWithSalt s = (s `hashWithSalt`) . MultiSet.elems
instance (Hashable a) => Hashable (Set.Set a) where
  hashWithSalt s = (s `hashWithSalt`) . Set.elems

-- | Stores the information about a transition of the register
-- machine.
--
-- This datatype does not include any information about the states the
-- transition connects.  This is to avoid the redundancy due to the
-- fact that there may be more than one transition between the same
-- pair of states.
data Transition = Transition
                  (IntMap.IntMap (MultiSet.MultiSet Instruction)) -- ^ The operations on registers.
                  (IntMap.IntMap Condition)                       -- ^ The conditions on registers.
                  deriving (Eq, Ord, Show, Read, Generic)
instance Hashable Transition

newTransition :: [(Int, [Instruction])] -> [(Int, Condition)] -> Transition
newTransition plainOps plainConds = Transition ops conds
  where ops = IntMap.fromList $ [ (reg, MultiSet.fromList regOps) | (reg, regOps) <- plainOps ]
        conds = IntMap.fromList plainConds

-- | The state graph of a register machine.
--
-- This type is meant to store both the original state graph of the
-- register machine and the compressed state graph.
--
-- The first field of this datatype stores the information about
-- transitions between states.  The other two fields of this datatype
-- are only meant to facilitate internal use.
data StateGraph = StateGraph
                  (Map.Map (Int, Int) (Set.Set Transition)) -- ^ The adjacency matrix.
                  (IntMap.IntMap [Int])                     -- ^ The adjacency list (internal).
                  (IntMap.IntMap [Int])                     -- ^ The reverse adjacency list (internal).
                deriving (Eq, Show, Read, Generic)

instance Hashable StateGraph where
  hashWithSalt s (StateGraph mx _ _) = s `hashWithSalt` mx -- The other two components depend on the first one.

newStateGraph :: Map.Map (Int, Int) (Set.Set Transition) -> StateGraph
newStateGraph mx = StateGraph mx adj revAdj
  where adj    = buildXAdj (\(v, w) -> [(v, [w]), (w, [])])
        revAdj = buildXAdj (\(v, w) -> [(w, [v]), (v, [])])
        buildXAdj f = IntMap.map nub $ IntMap.fromListWith (++) $ (concatMap f) $ Map.keys mx

stateCount (StateGraph _ adj _) = IntMap.size adj

-- An utility function that translates the convenient list and tuple
-- representation of the state graph to the actual adjacency matrix.
buildMx :: [( (Int,Int), ( [(Int, [Instruction])], [(Int, Condition)] ) )] -> Map.Map (Int, Int) (Set.Set Transition)
buildMx = Map.fromListWith Set.union . map (\(e, (plainOps, plainConds)) ->
                                                     (e, Set.singleton $ newTransition plainOps plainConds))

toDotHighlight :: IntSet.IntSet -> StateGraph -> Dot.Dot ()
toDotHighlight highlight (StateGraph mx adj _) = do
  mapM_ (\state ->
          Dot.userNode (Dot.userNodeId state) $
          [("label", show state)]
          ++ if state `IntSet.member` highlight
             then [("color", "blue"), ("style", "bold")]
             else []
        ) $ IntMap.keys adj

  mapM_ (\((v, w), trans) ->
          Dot.edge (Dot.userNodeId v) (Dot.userNodeId w)
          $ [ ("label", printTrans trans)
            , ("decorate", "true")        ]
          ++ if v `IntSet.member` highlight && w `IntSet.member` highlight
             then [("color", "blue"), ("style", "bold")]
             else []
          ) $ [ (e, trans) | (e, allTrans) <- Map.assocs mx, trans <- Set.elems allTrans ]
  where printTrans (Transition ops conds) =
          let stringConds = case printConds conds of
                "" -> ""
                str -> "(" ++ str ++ ")"
          in stringConds ++ (printOps ops)

        printOps allOps = intercalate "," [ printOp reg op
                                          | (reg, ops) <- IntMap.assocs allOps
                                          , op <- MultiSet.toList ops ]

        printOp r Dec = show r ++ "-"
        printOp r Inc = show r ++ "+"

        printConds = intercalate "," . map (uncurry printOneCond) . IntMap.assocs

        printOneCond r Zero    = show r ++ "Z"
        printOneCond r NotZero = show r

toDot :: StateGraph -> Dot.Dot ()
toDot = toDotHighlight IntSet.empty

toFileHighlight :: IntSet.IntSet -> StateGraph -> FilePath -> IO ()
toFileHighlight highlight graph filename = writeFile filename $ (Dot.showDot . toDotHighlight highlight) graph

toFile :: StateGraph -> FilePath -> IO ()
toFile = toFileHighlight IntSet.empty

fromFile :: FilePath -> IO StateGraph
fromFile filename = readFile filename >>= return . read

listRegs :: StateGraph -> [Int]
listRegs (StateGraph mx _ _) = sort . nub . concat
                               $ [ IntMap.keys ops | allTrans <- Map.elems mx
                                                   , (Transition ops _) <- Set.elems allTrans ]
