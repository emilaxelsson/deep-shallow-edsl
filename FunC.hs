{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Slightly adapted code from the paper \"Combining Deep and Shallow Embedding for EDSL\"
-- (published in Trends in Functional Programming TFP 2013):
--
--   * <http://www.cse.chalmers.se/~emax/documents/svenningsson2013combining.pdf>
--
--   * <http://dx.doi.org/10.1007/978-3-642-40447-4_2>
--
-- The paper descibes a design pattern for embedded DSLs (EDSLs) where a combination of deep and
-- shallow embedding is used in order to get most advantages from both approaches.
--
-- In short, the idea is as follows:
--
--   * The deep embedding 'FunC' is designed with respect to the code we want to generate, not what
--     we want the user to write.
--
--   * High-level libraries (e.g. 'Vector' and its operations) are implemented as shallow embeddings
--     on top of 'FunC'.
--
--   * The 'Syntactic' class is used to connect the deep and the shallow embeddings.
--
-- The approach has several advantages:
--
--   * /Simplicity/ -- Moving functionality to shallow embeddings helps keep the AST small without
--     sacrificing expressiveness.
--
--   * /Extensibility/ -- New shallow embeddings can often be added without changing the deep
--     embedding, or by just changing is slightly.
--
--   * /Abstraction/ -- The shallow embeddings are based on /abstract data types/ leading to better
--     programming interfaces (more like ordinary APIs than constructs of a language). This has
--     important side-effects:
--     (1) The shallow interfaces can have properties not possessed by the deep embedding. For
--     example, the 'Vector' interface guarantees removal of intermediate structures.
--     (2) The abstract types can sometimes be made instances of standard Haskell type classes, such
--     as 'Functor' and 'Monad' (see the 'Option' and 'Vector' types), even when the deep embedding
--     cannot.
--
-- This style of EDSL design is supported by the Syntactic library:
--
--   * <http://hackage.haskell.org/package/syntactic>
--
-- Syntactic has generic functionality that gives some of the things defined in this code for free.
--
-- The Feldspar EDSL is implemented using the deep/shallow technique:
--
--   * <http://hackage.haskell.org/package/feldspar-language>

module FunC where



import Prelude hiding ((==), (<), not, min, zipWith, sum)
import qualified Prelude as P

import Control.Monad.State
import Data.Array
import Data.Tree hiding (drawTree)

import Data.Tree.View



----------------------------------------------------------------------------------------------------
-- * Deep embedding
--
-- Designed to be easily translated to a low-level language such as C
----------------------------------------------------------------------------------------------------

-- | Deep embedding of a low-level pure functional language
--
-- The type parameter to 'FunC' allows us to track the type of expressions. As the type of 'eval'
-- shows, an expression @`FunC` a@ computes a value of type @a@.
--
-- 'FunC' is a GADT, which means that its constructors can constrain the result types. For example,
-- the 'LitI' constructor constrains the type parameter in the result to be 'Int'. These constraints
-- go two ways:
--
--   * Only well-typed 'FunC' expressions can be constructed.
--
--   * When pattern matching on an expression, we are allowed to assume that the constraints hold
--     (local assumptions; see e.g. 'eval').
data FunC a
  where
    LitI     :: Int  -> FunC Int                                                 -- Integer literals
    LitB     :: Bool -> FunC Bool                                                -- Boolean literals
    If       :: FunC Bool -> FunC a -> FunC a -> FunC a                          -- Condition
    While    :: (FunC s -> FunC Bool) -> (FunC s -> FunC s) -> FunC s -> FunC s  -- While loop
    Pair     :: FunC a -> FunC b -> FunC (a,b)                                   -- Pairs
    Fst      :: FunC (a,b) -> FunC a                                             -- Pair projection
    Snd      :: FunC (a,b) -> FunC b                                             -- Pair projection
    Prim1    :: String -> (a -> b) -> FunC a -> FunC b                           -- Primitive 1-ary function
    Prim2    :: String -> (a -> b -> c) -> FunC a -> FunC b -> FunC c            -- Primitive 2-ary function
    Value    :: a -> FunC a                                                      -- Variable substitution ('eval')
    Variable :: String -> FunC a                                                 -- Variable substitution ('toTree')
    Undef    :: FunC a                                                           -- "Bottom"
    Arr      :: FunC Int -> (FunC Int -> FunC a) -> FunC (Array Int a)           -- Array construction
    ArrLen   :: FunC (Array Int a) -> FunC Int                                   -- Array length
    ArrIx    :: FunC (Array Int a) -> FunC Int -> FunC a                         -- Array indexing

-- | Evaluation
--
-- Note how we make use of local assumptions to compute directly with Haskell values of different
-- type. For example, in the first case, we are allowed to return an 'Int', even though 'eval' is
-- polymorphic.
eval :: FunC a -> a
eval (LitI i)        = i
eval (LitB b)        = b
eval (If c t e)      = if eval c then eval t else eval e
eval (While c b i)   = head $ dropWhile (eval . c . Value) $ iterate (eval . b . Value) $ eval i
eval (Pair a b)      = (eval a, eval b)
eval (Fst p)         = fst (eval p)
eval (Snd p)         = snd (eval p)
eval (Prim1 _ f a)   = f (eval a)
eval (Prim2 _ f a b) = f (eval a) (eval b)
eval (Value a)       = a
eval Undef           = undefined
eval (Arr l ixf)     = listArray (0,lm1) $ map (eval . ixf . Value) [0..lm1]
                         where lm1 = eval l - 1
eval (ArrLen a)      = (1 +) $ uncurry (flip (-)) $ bounds $ eval a
eval (ArrIx a i)     = eval a ! eval i

-- | Conversion to tree
toTree :: FunC a -> Tree String
toTree = flip evalState 0 . toTree'

-- | Generate a fresh variable name
freshVar :: State Int Int
freshVar = do
    v <- get
    put (succ v)
    return v

-- | Show a variable as a string
showVar :: Int -> String
showVar v = 'v' : show v

toTree' :: FunC a -> State Int (Tree String)
toTree' (LitI i)        = return $ Node (show i) []
toTree' (LitB b)        = return $ Node (show b) []
toTree' (If c t e)      = fmap (Node "If") $ sequence [toTree' c, toTree' t, toTree' e]
toTree' (While c b i)   = do v <- freshVar
                             let var = Variable (showVar v)
                             fmap (Node ("While " ++ showVar v)) $
                               sequence [toTree' (c var), toTree' (b var), toTree' i]
toTree' (Pair a b)      = fmap (Node "Pair") $ sequence [toTree' a, toTree' b]
toTree' (Fst p)         = fmap (Node "Fst") $ sequence [toTree' p]
toTree' (Snd p)         = fmap (Node "Snd") $ sequence [toTree' p]
toTree' (Prim1 f _ a)   = fmap (Node f) $ sequence [toTree' a]
toTree' (Prim2 f _ a b) = fmap (Node f) $ sequence [toTree' a, toTree' b]
toTree' (Variable v)    = return $ Node v []
toTree' Undef           = return $ Node "Undef" []
toTree' (Arr l ixf)     = do v <- freshVar
                             let var = Variable (showVar v)
                             fmap (Node ("Arr " ++ showVar v)) $
                               sequence [toTree' l, toTree' (ixf var)]
toTree' (ArrLen a)      = fmap (Node "ArrLen") $ sequence [toTree' a]
toTree' (ArrIx a i)     = fmap (Node "ArrIx") $ sequence [toTree' a, toTree' i]

-- | Draw an AST on the terminal
drawAST :: FunC a -> IO ()
drawAST = drawTree . toTree



-- | Boolean literals
true, false :: FunC Bool
true        =  LitB True
false       =  LitB False

-- | Reuse Haskell's 'Num' class for 'FunC'. This gives support for integer literals.
instance Num (FunC Int)
  where
    fromInteger = LitI . fromInteger
    (+)         = Prim2 "(+)" (+)
    (-)         = Prim2 "(-)" (-)
    (*)         = Prim2 "(*)" (*)
    abs         = Prim1 "abs" abs
    signum      = Prim1 "signum" signum

-- | Equality (overrides 'P.==' from "Prelude")
(==) :: Ord a => FunC a -> FunC a -> FunC Bool
(==) = Prim2 "(==)" (P.==)

-- | Less than (overrides 'P.<' from "Prelude")
(<) :: Ord a => FunC a -> FunC a -> FunC Bool
(<) = Prim2 "(<)" (P.<)

not :: FunC Bool -> FunC Bool
not = Prim1 "not" (P.not)

-- | Minimum (overrides 'P.min' from "Prelude")
min :: Ord a => FunC a -> FunC a -> FunC a
min = Prim2 "min" P.min

-- | While loop (non overloaded)
while' :: (FunC s -> FunC Bool) -> (FunC s -> FunC s) -> (FunC s -> FunC s)
while' c b i = While c b i



----------------------------------------------------------------------------------------------------
-- * Shallow embeddings
--
-- Designed with respect to the user
----------------------------------------------------------------------------------------------------

-- | Bridge between deep and shallow embeddings
class Syntactic a
  where
    type Internal a
    desugar :: a -> FunC (Internal a)
    sugar   :: FunC (Internal a) -> a

instance Syntactic (FunC a)
  where
    type Internal (FunC a) = a
    desugar = id
    sugar   = id

-- | "Desugar" an EDSL function with one argument
desugar1 :: (Syntactic a, Syntactic b) => (a -> b) -> FunC (Internal b)
desugar1 f = desugar $ f $ sugar $ Variable "x"

-- | "Desugar" an EDSL function with two arguments
desugar2 :: (Syntactic a, Syntactic b, Syntactic c) => (a -> b -> c) -> FunC (Internal c)
desugar2 f = desugar $ f (sugar $ Variable "x") (sugar $ Variable "y")

-- | "Desugar" an EDSL function with three arguments
desugar3 :: (Syntactic a, Syntactic b, Syntactic c, Syntactic d) =>
    (a -> b -> c -> d) -> FunC (Internal d)
desugar3 f = desugar $ f (sugar $ Variable "x") (sugar $ Variable "y") (sugar $ Variable "z")

-- | Condition
ifC :: Syntactic a => FunC Bool -> a -> a -> a
ifC c t e = sugar (If c (desugar t) (desugar e))

-- | Condition
(?) :: Syntactic a => FunC Bool -> (a,a) -> a
c ? (t,e) = ifC c t e

-- | While loop
while :: Syntactic s => (s -> FunC Bool) -> (s -> s) -> s -> s
while c b i = sugar (While (c . sugar) (desugar . b . sugar) (desugar i))

instance (Syntactic a, Syntactic b) => Syntactic (a,b)
  where
    type Internal (a,b) = (Internal a, Internal b)
    desugar (a,b)       = Pair (desugar a) (desugar b)
    sugar p             = (sugar (Fst p), sugar (Snd p))

-- | For loop
forLoop :: Syntactic s => FunC Int -> s -> (FunC Int -> s -> s) -> s
forLoop len init step = snd $ while
    (\(i,s) -> i<len)
    (\(i,s) -> (i+1, step i s))
    (0,init)

-- | Bottom
undef :: Syntactic a => a
undef = sugar Undef



----------------------------------------------------------------------------------------------------
-- * Optional values
----------------------------------------------------------------------------------------------------

-- | Optional value (corresponds to 'Maybe')
data Option a = Option { isSome :: FunC Bool, fromSome :: a }

instance Syntactic a => Syntactic (Option a)
  where
    type Internal (Option a) = (Bool,Internal a)
    sugar m                  = fmap sugar $ Option (Fst m) (Snd m)
    desugar (Option b a)     = Pair (desugar b) (desugar a)

-- | Corresponds to 'Just'
some :: a -> Option a
some a = Option true a

-- | Corresponds to 'Nothing'
none :: Syntactic a => Option a
none = Option false undef

-- | Corresponds to 'maybe'
option :: Syntactic b => b -> (a -> b) -> Option a -> b
option noneCase someCase opt =
    ifC (isSome opt)
      (someCase (fromSome opt))
      noneCase

instance Functor Option
  where
    fmap f (Option b a) = Option b (f a)

instance Monad Option
  where
    return a  = some a
    opt >>= k = b { isSome = isSome opt ? (isSome b, false) }
      where
        b = k (fromSome opt)

-- | Integer division, only defined for @b/=0@
divO :: FunC Int -> FunC Int -> Option (FunC Int)
divO a b = Option (not (b==0)) (Prim2 "div" P.div a b)

divTest :: FunC Int -> FunC Int -> FunC Int -> Option (FunC Int)
divTest a b c = do r1 <- divO a b
                   r2 <- divO a c
                   return (r1+r2)



----------------------------------------------------------------------------------------------------
-- * Vectors
----------------------------------------------------------------------------------------------------

-- | Array length
len :: FunC (Array Int a) -> FunC Int
len arr = ArrLen arr

-- | Array indexing
(<!>) :: FunC (Array Int a) -> FunC Int -> FunC a
arr <!> ix = sugar (ArrIx arr ix)

-- | Virtual vector type (only generates an in-memory array when explicitly forced)
data Vector a
  where
    Indexed :: FunC Int -> (FunC Int -> a) -> Vector a

-- | Vector length
vlen :: Vector a -> FunC Int
vlen (Indexed l _) = l

-- | Vector indexing
(<!!>) :: Vector a -> FunC Int -> a
Indexed _ ixf <!!> i = ixf i

instance Syntactic a => Syntactic (Vector a)
  where
    type Internal (Vector a) = Array Int (Internal a)
    desugar (Indexed l ixf)  = Arr l (desugar . ixf)
    sugar arr                = Indexed (len arr) (\ix -> sugar (arr <!> ix))

-- | Overrides 'P.zipWith' from "Prelude"
zipWith :: (Syntactic a, Syntactic b) => (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f (Indexed l1 ixf1) (Indexed l2 ixf2) =
    Indexed (min l1 l2) (\ix -> f (ixf1 ix) (ixf2 ix))

-- | Overrides 'P.sum' from "Prelude"
sum :: (Syntactic a, Num a) => Vector a -> a
sum (Indexed l ixf) = forLoop l 0 (\ix s -> s + ixf ix)

instance Functor Vector
  where
    fmap f (Indexed l ixf) = Indexed l (f . ixf)

-- | Scalar product
scalarProd :: Vector (FunC Int) -> Vector (FunC Int) -> FunC Int
scalarProd a b = sum $ force (zipWith (*) a b)

-- | Force a vector to generate an array in memory
force  :: Syntactic a => Vector a -> Vector a
force (Indexed l ixf) = Indexed l (\n -> sugar (Arr l (desugar . ixf) <!> n))

forEach = flip fmap

type Matrix a = Vector (Vector (FunC a))

-- | Matrix transpose
transpose :: Matrix a -> Matrix a
transpose a = Indexed (vlen (a<!!>0)) $ \k -> Indexed (vlen a) $ (\l -> (a <!!> l) <!!> k)

-- | Matrix multiplication
matMul :: Matrix Int -> Matrix Int -> Matrix Int
matMul a b = forEach a $ \a' ->
               forEach (transpose b) $ \b' ->
                 scalarProd a' b'

test1 = drawAST $ desugar3 divTest
test2 = drawAST $ desugar2 scalarProd
test3 = drawAST $ desugar2 matMul

