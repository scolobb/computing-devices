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
       , transCount
       , states
       , halting
       , toDotHighlight
       , toDot
       , toFileHighlight
       , toFile
       , fromFile
       , listRegs
       , u22compressed
       , u20compressed
       , buildAdjTrans
       , fixMultipleOps
       , allCompressions
       , buildMx
       , finalStates
       , fromRM
       , compressIncs
       , printSGOrig
       , printSGPhD
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
import Data.Maybe (catMaybes)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Writer.Lazy as Writer
import qualified Control.Monad.Trans.State.Lazy as State
import qualified RegisterMachine as RM

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

stateCount :: StateGraph -> Int
stateCount (StateGraph _ adj _) = IntMap.size adj

transCount :: StateGraph -> Int
transCount (StateGraph mx _ _) = sum $ Map.elems $ Map.map Set.size mx

states :: StateGraph -> [Int]
states (StateGraph _ adj _) = IntMap.keys adj

halting :: StateGraph -> [Int]
halting (StateGraph _ adj _) = IntMap.keys $ IntMap.filter null adj

-- an utility function that translates the convenient list and tuple
-- representation of the state graph to the actual adjacency matrix.
buildMx :: [( (Int,Int), ( [(Int, [Instruction])], [(Int, Condition)] ) )] -> Map.Map (Int, Int) (Set.Set Transition)
buildMx = Map.fromListWith Set.union . map (\(e, (plainOps, plainConds)) ->
                                                     (e, Set.singleton $ newTransition plainOps plainConds))

-- | Maps vertices to lists of the form (targetVertex, transition).
buildAdjTrans :: StateGraph -> IntMap.IntMap [(Int, Transition)]
buildAdjTrans (StateGraph mx _ _) =
  -- Note that we not only drop the terminal states, but also all
  -- transitions leading into them.
  IntMap.fromListWith (++) [ (v, [(w, trans)])
                           | ((v, w), allTrans) <- Map.assocs mx
                           , trans <- Set.elems allTrans
                           ]

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

finalStates :: StateGraph -> [Int]
finalStates (StateGraph _ adj _) = IntMap.keys $ IntMap.filter null adj

-- | Checks if the supplied state can be compressed, i.e., thrown away
-- by merging the transitions going out of this state with the
-- transitions coming into it.
--
-- Such an operation can be done when none of the outgoing edges
-- checks the contents of a register that is modified by a dec
-- instruction.
compressible :: StateGraph -> Int -> Bool
compressible (StateGraph _ adj revAdj) v0
  | (adj IntMap.! v0 == []) || (revAdj IntMap.! v0 == []) = False -- Don't compress initial and final states.
compressible (StateGraph _ adj revAdj) v0
  | (v0 `elem` (adj IntMap.! v0)) = False -- Don't even try to compress states with loops.
compressible (StateGraph mx adj revAdj) v0 =
  let inTrans  = concat [ Set.elems $ mx Map.! (v, v0) | v <- revAdj IntMap.! v0 ]
      outTrans = concat [ Set.elems $ mx Map.! (v0, v) | v <- adj    IntMap.! v0 ]
  in and $ do
    (Transition inOps _   ) <- inTrans
    (Transition _ outConds) <- outTrans
    return $ not $ any (Dec `MultiSet.member`)
                        $ IntMap.elems $ IntMap.intersection inOps outConds

-- | Compresses the supplied state, i.e., removes it and merges all
-- its outgoing transitions with its ingoing transitions.
--
-- This function does NOT check whether the supplied state is indeed
-- compressible.
compress :: StateGraph -> Int -> StateGraph
compress (StateGraph mx adj revAdj) v0 =
  let preds = revAdj IntMap.! v0
      succs = adj    IntMap.! v0

      (oldInList, oldOutList, newTransMaybes) = unzip3 $ do
        pred <- preds
        succ <- succs
        inTrans  <- Set.elems $ mx Map.! (pred, v0)
        outTrans <- Set.elems $ mx Map.! (v0, succ)
        return (((pred, v0), ()), ((v0, succ), ()), maybeNewTrans (pred, succ) inTrans outTrans)
      newTransList = catMaybes newTransMaybes

      cleanMx = (mx `Map.difference` (Map.fromList $ oldInList ++ oldOutList))
      newMx = Map.unionWith Set.union cleanMx $ Map.fromListWith Set.union newTransList

  in newStateGraph newMx
  where maybeNewTrans e trans1@(Transition inOps inConds) trans2@(Transition outOps outConds) =
          -- According to the compressibility criterion we know that
          -- the incoming transition modifies a register that is
          -- checked by the outgoing transition, it can only be an
          -- 'Inc' operation.
          let detRegs = outConds `IntMap.intersection` inOps
              bothCheck = IntMap.intersectionWith (,) inConds outConds
          in if badConds bothCheck trans1 trans2 || badOutConds detRegs
                -- The incoming transition increments a register,
                -- while the outgoing one requires that the same
                -- register be zero.
             then Nothing
                  -- The incoming transition increments some
                  -- registers, and the outgoing transition requires
                  -- that these registers be nonzero.  Just forget
                  -- about these checks altogether.
             else let outConds' = outConds `IntMap.difference` detRegs
                      newOps = IntMap.unionWith MultiSet.union inOps outOps
                      newConds = inConds `IntMap.union` outConds'
                  in Just (e, Set.singleton $ Transition newOps newConds)

        -- This function analyses the four possible combinations of
        -- clashing conditions on a register.  See the paper for even
        -- more details.
        badConds bothCheck (Transition inOps inConds) (Transition outOps outConds) =
          any (\(reg, (inCond, outCond)) ->
                case (inCond, outCond) of
                  (Zero,    Zero   ) -> case IntMap.lookup reg inOps of
                    Nothing    -> False
                    Just regOp -> Inc `MultiSet.member`    regOp
                  (Zero,    NotZero) -> case IntMap.lookup reg inOps of
                    Nothing    -> True
                    Just regOp -> Inc `MultiSet.notMember` regOp
                  (NotZero, Zero   ) -> True
                  (NotZero, NotZero) -> False
              ) $ IntMap.assocs bothCheck

        -- This function analyses the interference between the
        -- operations of the ingoing transition and the conditions of
        -- the outgoing transition.  See the paper for even more
        -- details.
        badOutConds detRegs = Zero `elem` IntMap.elems detRegs

-- | Computes the length of a transition (the number of operations and
-- conditions associated with it).
transLen :: Transition -> Int
transLen (Transition ops conds) =
  let nops = sum $ map MultiSet.size $ IntMap.elems ops
      nconds = IntMap.size conds
  in nops + nconds

-- | Checks if the transitions in the state transition graph are too
-- long.
--
-- The point is that Rudi manages to get transitions of maximal weight
-- (number of (operations + conditions)) 6, while our best result so
-- far is 7.
transitionsTooLong :: StateGraph -> Int -> Bool
transitionsTooLong (StateGraph mx _ _) maxLen =
  any (> maxLen) $ map transLen $ concatMap Set.elems $ Map.elems mx

-- | Starting from a certain graph, does all possible compressions on
-- it and returns a list of compressed graphs and the corresponding
-- list of states that were chosen for compression.
allCompressions :: StateGraph -> [([Int], StateGraph)]
allCompressions graph0 = Writer.execWriter $ State.evalStateT (go [] graph0) IntSet.empty
  where go pickedStates graph = do
          seen <- State.get
          let graphHash = hash graph
          if (graphHash `IntSet.member` seen) || transitionsTooLong graph 6
            then return ()
            else do
            State.put $ graphHash `IntSet.insert` seen
            case allCompressible graph of
              [] -> lift $ Writer.tell [(reverse pickedStates, graph)]
              states -> mapM_ (\state -> do
                                  let graph' = compress graph state
                                  go (state:pickedStates) graph'
                                ) states

        allCompressible graph@(StateGraph _ adj _) = filter (compressible graph) $ IntMap.keys adj

-- Fixes transitions which increment and decrement on the same
-- register.
fixMultipleOpsTrans :: Transition -> Transition
fixMultipleOpsTrans (Transition ops conds) =
  let ops' = IntMap.map (\regOps -> let m = MultiSet.occur Dec regOps
                                        p = MultiSet.occur Inc regOps
                                    in MultiSet.fromOccurList $ squash (p - m)
                        ) ops
      newOps = IntMap.filter ((/=) MultiSet.empty) ops'
  in Transition newOps conds
  where squash x | x > 0 = [(Inc,  x)]
                 | x < 0 = [(Dec, -x)]
        squash _ = []

-- | Applies 'fixMultipleOpsTrans' to all transitions in the given
-- state graph.
fixMultipleOps :: StateGraph -> StateGraph
fixMultipleOps (StateGraph mx adj radj) =
  let mx' = Map.map (Set.map fixMultipleOpsTrans) mx
  in (StateGraph mx' adj radj)

-- | The compressed state graph of the strongly universal register
-- machine with 22 instructions of type /RiZM/ and /RiP/.
--
-- This graph was generated automatically by 'allCompressions'.
u22compressed :: StateGraph
u22compressed = fixMultipleOps $ StateGraph
                (Map.fromList [ ((1,1),Set.fromList [ Transition (IntMap.fromList [(1,MultiSet.fromOccurList [(Dec,1)]),(7,MultiSet.fromOccurList [(Inc,1)])])
                                                      (IntMap.fromList [(1,NotZero)])])
                              , ((1,4),Set.fromList [ Transition (IntMap.fromList [(6,MultiSet.fromOccurList [(Inc,1)])])
                                                      (IntMap.fromList [(1,Zero)])] )
                              , ((4,4),Set.fromList [ Transition (IntMap.fromList []) (IntMap.fromList [(5,Zero),(6,Zero)])
                                                    , Transition (IntMap.fromList [(5,MultiSet.fromOccurList [(Dec,1)]),(6,MultiSet.fromOccurList [(Inc,1)])])
                                                      (IntMap.fromList [(5,NotZero)]) ])
                              , ((4,10),Set.fromList [ Transition (IntMap.fromList [(5,MultiSet.fromOccurList [(Inc,1)]),(6,MultiSet.fromOccurList [(Dec,1)])])
                                                       (IntMap.fromList [(5,Zero),(6,NotZero)]) ])
                              , ((10,1),Set.fromList [ Transition (IntMap.fromList []) (IntMap.fromList [(6,Zero),(7,Zero)])
                                                     , Transition (IntMap.fromList [(4,MultiSet.fromOccurList [(Dec,1)]),(6,MultiSet.fromOccurList [(Dec,1),(Inc,1)])])
                                                       (IntMap.fromList [(4,NotZero),(6,NotZero),(7,Zero)]) ])
                              , ((10,4),Set.fromList [ Transition (IntMap.fromList [(1,MultiSet.fromOccurList [(Inc,1)]),(7,MultiSet.fromOccurList [(Dec,1)])])
                                                       (IntMap.fromList [(6,Zero),(7,NotZero)]) ])
                              , ((10,10),Set.fromList [ Transition (IntMap.fromList [ (1,MultiSet.fromOccurList [(Inc,1)]),(5,MultiSet.fromOccurList [(Inc,1)])
                                                                                    , (6,MultiSet.fromOccurList [(Dec,1)]),(7,MultiSet.fromOccurList [(Dec,1)]) ])
                                                        (IntMap.fromList [(6,NotZero),(7,NotZero)])])
                              , ((10,16),Set.fromList [ Transition (IntMap.fromList [(6,MultiSet.fromOccurList [(Dec,1),(Inc,1)])])
                                                        (IntMap.fromList [(4,Zero),(6,NotZero),(7,Zero)]) ])
                              , ((16,1),Set.fromList [ Transition (IntMap.fromList [(0,MultiSet.fromOccurList [(Dec,1)])])
                                                       (IntMap.fromList [(0,NotZero),(2,Zero),(5,Zero)])
                                                     , Transition (IntMap.fromList [(2,MultiSet.fromOccurList [(Dec,1)]),(4,MultiSet.fromOccurList [(Dec,1)])])
                                                       (IntMap.fromList [(2,NotZero),(4,NotZero),(5,Zero)])
                                                     , Transition (IntMap.fromList [(4,MultiSet.fromOccurList [(Dec,1)])])
                                                       (IntMap.fromList [(0,Zero),(2,Zero),(4,NotZero),(5,Zero)]) ])
                              , ((16,18),Set.fromList [ Transition (IntMap.fromList [(5,MultiSet.fromOccurList [(Dec,1)])])
                                                        (IntMap.fromList [(5,NotZero)]) ])
                              , ((16,34),Set.fromList [ Transition (IntMap.fromList []) (IntMap.fromList [(0,Zero),(2,Zero),(4,Zero),(5,Zero)])
                                                      , Transition (IntMap.fromList [(2,MultiSet.fromOccurList [(Dec,1)])])
                                                        (IntMap.fromList [(2,NotZero),(4,Zero),(5,Zero)]) ])
                              , ((18,1),Set.fromList [ Transition (IntMap.fromList [(0,MultiSet.fromOccurList [(Inc,1)])])
                                                       (IntMap.fromList [(3,Zero),(5,Zero)])
                                                     , Transition (IntMap.fromList [(3,MultiSet.fromOccurList [(Dec,1)]),(4,MultiSet.fromOccurList [(Dec,1)])])
                                                       (IntMap.fromList [(3,NotZero),(4,NotZero),(5,Zero)]) ])
                              , ((18,20),Set.fromList [ Transition (IntMap.fromList [(5,MultiSet.fromOccurList [(Dec,1)])])
                                                        (IntMap.fromList [(5,NotZero)]) ])
                              , ((18,34),Set.fromList [ Transition (IntMap.fromList [(3,MultiSet.fromOccurList [(Dec,1)])])
                                                        (IntMap.fromList [(3,NotZero),(4,Zero),(5,Zero)]) ])
                              , ((20,1),Set.fromList [ Transition (IntMap.fromList [ (2,MultiSet.fromOccurList [(Inc,1)]),(3,MultiSet.fromOccurList [(Inc,1)])
                                                                                   , (4,MultiSet.fromOccurList [(Dec,1)])])
                                                       (IntMap.fromList [(4,NotZero),(5,Zero)]) ])
                              , ((20,16),Set.fromList [ Transition (IntMap.fromList [(4,MultiSet.fromOccurList [(Inc,1)]),(5,MultiSet.fromOccurList [(Dec,1)])])
                                                        (IntMap.fromList [(5,NotZero)]) ])
                              , ((20,34),Set.fromList [ Transition (IntMap.fromList [(2,MultiSet.fromOccurList [(Inc,1)]),(3,MultiSet.fromOccurList [(Inc,1)])])
                                                        (IntMap.fromList [(4,Zero),(5,Zero)])]) ])

                (IntMap.fromList [(1,[4,1]),(4,[10,4]),(10,[16,10,4,1]),(16,[34,18,1]),(18,[34,20,1]),(20,[34,16,1]),(34,[])])
                (IntMap.fromList [(1,[20,18,16,10,1]),(4,[10,4,1]),(10,[10,4]),(16,[20,10]),(18,[16]),(20,[18]),(34,[20,18,16])])

-- | The compressed state graph of the weakly universal register
-- machine with 20 instructions of type /RiZM/ and /RiP/.
--
-- This graph was generated automatically by 'allCompressions'.
u20compressed :: StateGraph
u20compressed = fixMultipleOps $ StateGraph
                (Map.fromList [ ((1,1),Set.fromList [ Transition (IntMap.fromList [(1,MultiSet.fromOccurList [(Dec,1)]),(7,MultiSet.fromOccurList [(Inc,1)])])
                                                      (IntMap.fromList [(1,NotZero)]) ])
                              , ((1,4),Set.fromList [ Transition (IntMap.fromList [(6,MultiSet.fromOccurList [(Inc,1)])])
                                                      (IntMap.fromList [(1,Zero)]) ])
                              , ((4,4),Set.fromList [ Transition (IntMap.fromList []) (IntMap.fromList [(5,Zero),(6,Zero)])
                                                    , Transition (IntMap.fromList [(5,MultiSet.fromOccurList [(Dec,1)]),(6,MultiSet.fromOccurList [(Inc,1)])])
                                                      (IntMap.fromList [(5,NotZero)]) ])
                              , ((4,10),Set.fromList [ Transition (IntMap.fromList [(5,MultiSet.fromOccurList [(Inc,1)]),(6,MultiSet.fromOccurList [(Dec,1)])])
                                                       (IntMap.fromList [(5,Zero),(6,NotZero)]) ])
                              , ((10,1),Set.fromList [ Transition (IntMap.fromList []) (IntMap.fromList [(6,Zero),(7,Zero)])
                                                     , Transition (IntMap.fromList [(4,MultiSet.fromOccurList [(Dec,1)]),(6,MultiSet.fromOccurList [(Dec,1),(Inc,1)])])
                                                       (IntMap.fromList [(4,NotZero),(6,NotZero),(7,Zero)]) ])
                              , ((10,4),Set.fromList [ Transition (IntMap.fromList [(1,MultiSet.fromOccurList [(Inc,1)]),(7,MultiSet.fromOccurList [(Dec,1)])])
                                                       (IntMap.fromList [(6,Zero),(7,NotZero)]) ])
                              , ((10,10),Set.fromList [ Transition (IntMap.fromList [ (1,MultiSet.fromOccurList [(Inc,1)]),(5,MultiSet.fromOccurList [(Inc,1)])
                                                                                    , (6,MultiSet.fromOccurList [(Dec,1)]),(7,MultiSet.fromOccurList [(Dec,1)]) ])
                                                        (IntMap.fromList [(6,NotZero),(7,NotZero)])])
                              , ((10,16),Set.fromList [ Transition (IntMap.fromList [(6,MultiSet.fromOccurList [(Dec,1),(Inc,1)])])
                                                        (IntMap.fromList [(4,Zero),(6,NotZero),(7,Zero)]) ])
                              , ((16,1),Set.fromList [ Transition (IntMap.fromList []) (IntMap.fromList [(0,Zero),(5,Zero)])
                                                     , Transition (IntMap.fromList [(0,MultiSet.fromOccurList [(Dec,1)]),(4,MultiSet.fromOccurList [(Dec,1)])])
                                                       (IntMap.fromList [(0,NotZero),(4,NotZero),(5,Zero)]) ])
                              , ((16,18),Set.fromList [ Transition (IntMap.fromList [(5,MultiSet.fromOccurList [(Dec,1)])])
                                                        (IntMap.fromList [(5,NotZero)]) ])
                              , ((16,34),Set.fromList [ Transition (IntMap.fromList [(0,MultiSet.fromOccurList [(Dec,1)])])
                                                        (IntMap.fromList [(0,NotZero),(4,Zero),(5,Zero)]) ])
                              , ((18,1),Set.fromList [ Transition (IntMap.fromList []) (IntMap.fromList [(2,Zero),(5,Zero)])
                                                     , Transition (IntMap.fromList [(2,MultiSet.fromOccurList [(Dec,1)]),(4,MultiSet.fromOccurList [(Dec,1)])])
                                                       (IntMap.fromList [(2,NotZero),(4,NotZero),(5,Zero)]) ])
                              , ((18,20),Set.fromList [ Transition (IntMap.fromList [(5,MultiSet.fromOccurList [(Dec,1)])])
                                                        (IntMap.fromList [(5,NotZero)]) ])
                              , ((18,34),Set.fromList [ Transition (IntMap.fromList [(2,MultiSet.fromOccurList [(Dec,1)])])
                                                        (IntMap.fromList [(2,NotZero),(4,Zero),(5,Zero)]) ])
                              , ((20,1),Set.fromList [ Transition (IntMap.fromList [ (0,MultiSet.fromOccurList [(Inc,1)]),(2,MultiSet.fromOccurList [(Inc,1)])
                                                                                   , (4,MultiSet.fromOccurList [(Dec,1)]) ])
                                                       (IntMap.fromList [(4,NotZero),(5,Zero)])])
                              , ((20,16),Set.fromList [ Transition (IntMap.fromList [(4,MultiSet.fromOccurList [(Inc,1)]),(5,MultiSet.fromOccurList [(Dec,1)])])
                                                        (IntMap.fromList [(5,NotZero)]) ])
                              , ((20,34),Set.fromList [ Transition (IntMap.fromList [(0,MultiSet.fromOccurList [(Inc,1)]),(2,MultiSet.fromOccurList [(Inc,1)])])
                                                        (IntMap.fromList [(4,Zero),(5,Zero)])]) ])

                (IntMap.fromList [(1,[4,1]),(4,[10,4]),(10,[16,10,4,1]),(16,[34,18,1]),(18,[34,20,1]),(20,[34,16,1]),(34,[])])
                (IntMap.fromList [(1,[20,18,16,10,1]),(4,[10,4,1]),(10,[10,4]),(16,[20,10]),(18,[16]),(20,[18]),(34,[20,18,16])])

-- | Builds a state graph from the given register machine.
fromRM :: RM.RegisterMachine -> StateGraph
fromRM (RM.RegisterMachine _ prog) =
  newStateGraph $ buildMx $ concatMap mapper $ IntMap.toList prog
  where mapper (p, (RM.RiZM r q q')) = [( (p,q ),  ( [(r, [Dec])], [(r, NotZero)] ) )
                                       ,( (p,q'),  ( [          ], [(r, Zero   )] ) )]
        mapper (p, (RM.RiP  r q   )) = [( (p,q ),  ( [(r, [Inc])], [            ] ) )]
        mapper (_,  RM.HALT        ) = []

-- | Compresses all compressible states which only increment registers
-- in all outgoing transitions.
compressIncs :: StateGraph -> StateGraph
compressIncs graph@(StateGraph mx adj _) =
  let toCompress = [ v
                   | (v, vAdj) <- IntMap.assocs adj,
                     and' [ onlyIncs trans && compressible graph v
                          | w <- vAdj, trans <- Set.elems $ mx Map.! (v, w) ]
                   ]
  in case toCompress of
    [] -> graph
    (state:_) -> compressIncs $ compress graph state
  where onlyIncs (Transition ops _) = all (Dec `MultiSet.notMember`) $ IntMap.elems ops

        and' [] = False
        and' xs = and xs

type PrintOpFunc = (Int, Instruction) -> String
type PrintCondFunc = (Int, Condition) -> String

-- | Prints an instruction of the state graph in Korec-like format.
printOpKorec :: PrintOpFunc
printOpKorec (reg, Inc) = "$R" ++ (show reg) ++ "P$"
printOpKorec (reg, Dec) = "$R" ++ (show reg) ++ "M$"

-- | Prints a condition of the state graph in plain format.
printCondPlain :: PrintCondFunc
printCondPlain (reg, Zero)    = "$R_{" ++ (show reg) ++ "}=0$"
printCondPlain (reg, NotZero) = "$R_{" ++ (show reg) ++ "}\\neq 0$"

-- | Prints the state graph as a LaTeX table.
printSG :: PrintOpFunc -> PrintCondFunc -> StateGraph -> String
printSG printOp printCond (StateGraph mx _ _) =
  let smx' = map (\((v,w), trans) ->
                  let (ops, conds) = printTrans trans
                  in (show v) ++ "& " ++ (show w) ++ "& " ++ ops ++ "& " ++ conds
                ) $ explodeMx mx

      lnbk = "\\\\\n"
      smx = intercalate lnbk smx'
  in "\\begin{tabular}{r|r||l|l}\n"
     ++ "$q_i$ & $q_j$ & Conditions & Operations" ++ lnbk
     ++ "\\hline\n"
     ++ smx
     ++ "\n\\end{tabular}"
  where explodeWith f g = concatMap (\(key, vals) -> zip (repeat key) $ f vals) . g
        explodeOps = explodeWith MultiSet.elems IntMap.toList
        explodeMx = explodeWith Set.elems Map.toList

        printTrans (Transition ops conds) =
          ( intercalate ", " $ map printCond $ IntMap.toList conds
          , intercalate ", " $ map printOp $ explodeOps ops )

-- | Prints the state graph as a LaTeX table using the original
-- notations.
printSGOrig = printSG printOpKorec printCondPlain

-- | Prints an instruction of the state graph in the format I use in
-- my PhD thesis.
printOpPhD :: PrintOpFunc
printOpPhD (reg, Inc) = "A(" ++ (show reg) ++ ")"
printOpPhD (reg, Dec) = "S(" ++ (show reg) ++ ")"

-- | Prints a condition of the state graph in the format I use in my
-- PhD thesis.
printCondPhD :: PrintCondFunc
printCondPhD (reg, Zero)    = "Z("  ++ (show reg) ++ ")"
printCondPhD (reg, NotZero) = "NZ(" ++ (show reg) ++ ")"

-- | Prints the state graph as a LaTeX table using the unified
-- notations I use in my PhD thesis.
printSGPhD = printSG printOpPhD printCondPhD
