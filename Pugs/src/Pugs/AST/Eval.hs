{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -funbox-strict-fields -fallow-undecidable-instances #-}

module Pugs.AST.Eval where
import Pugs.Internals
import Pugs.Cont hiding (resetT)
import System.IO.Error (tryIOError, IOError)
import Control.Exception (SomeException)
import Control.Applicative (Applicative(..))
import Control.Monad (ap)

import Pugs.AST.SIO
import {-# SOURCE #-} Pugs.AST.Internals

{- Eval Monad -}

newtype Eval a = Eval (Env -> (a -> SIO Val) -> (Val -> SIO Val) -> SIO Val) deriving (Typeable)

evalT :: ContT (EvalResult Val) (ReaderT Env SIO) (EvalResult a) -> Eval a
evalT m = Eval $ \env _ left -> do
  let cont :: EvalResult a -> ReaderT Env SIO (EvalResult Val)
      cont (RException exp) = lift $ fmap RException (left exp)
  fmap unEvalResult $ runReaderT (runContT m cont) env

newtype EvalResult a = RException { unEvalResult :: Val } deriving (Typeable)

runEvalSTM :: Env -> Eval Val -> STM Val
runEvalSTM env (Eval f) = runSTM $ f (enterAtomicEnv env) return return

runEvalIO :: Env -> Eval Val -> IO Val
runEvalIO env (Eval f) = runIO $ f env return return

tryIO :: a -> IO a -> Eval a
tryIO err = io . (`catchIO` (\(e :: SomeException) -> return err))

{-|
'shiftT' is like @callCC@, except that when you activate the continuation
provided by 'shiftT', it will run to the end of the nearest enclosing 'resetT',
then jump back to just after the point at which you activated the continuation.

Note that because control eventually returns to the point after the 
subcontinuation is activated, you can activate it multiple times in the 
same block. This is unlike @callCC@'s continuations, which discard the current
execution path when activated.

See 'resetT' for an example of how these delimited subcontinuations actually
work.
-}
shiftT :: ((a -> Eval Val) -> Eval Val)
       -- ^ Typically a lambda function of the form @\\esc -> do ...@, where
       --     @esc@ is the current (sub)continuation
       -> Eval a
shiftT = error "[BUG] shiftT" -- _ = fail "shiftT not yet implemented in Eval"

{-|
Create an scope that 'shiftT'\'s subcontinuations are guaranteed to eventually
exit out the end of.

Consider this example:

> resetT $ do
>     alfa
>     bravo
>     x <- shiftT $ \esc -> do
>        charlie
>        esc 1
>        delta
>        esc 2
>        return 0
>     zulu x

This will:

  1) Perform @alfa@
  
  2) Perform @bravo@
  
  3) Perform @charlie@
  
  4) Bind @x@ to 1, and thus perform @zulu 1@
  
  5) Fall off the end of 'resetT', and jump back to just after @esc 1@
  
  6) Perform @delta@
  
  7) Bind @x@ to 2, and thus perform @zulu 2@
  
  8) Fall off the end of 'resetT', and jump back to just after @esc 2@
  
  6) Escape from the 'resetT', causing it to yield 0

Thus, unlike @callCC@'s continuations, these subcontinuations will eventually
return to the point after they are activated, after falling off the end of the
nearest 'resetT'.
-}
resetT :: Eval Val -- ^ An evaluation, possibly containing a 'shiftT'
       -> Eval Val
resetT = error "[BUG] resetT" -- (EvalT e) = EvalT (lift (e `runContT` return))

tryT :: Eval Val -- ^ An evaluation, possibly containing an exception
     -> Eval Val
-- tryT e = catchError e return
tryT e = catchError e return

instance Monad Eval where
    return a = Eval $ \_ r _ -> r a
    (Eval m) >>= k = Eval $ \env r l -> do
      let r' x = case k x of Eval m' -> m' env r l
      m env r' l
    fail str = do
      pos <- asks envPos'
      Eval $ \env r l -> l (errStrPos (cast str) pos)

instance Applicative Eval where
    pure  = return
    (<*>) = ap

instance Functor Eval where
    fmap = liftM

instance MonadIO Eval where
    liftIO = liftSIO . io

instance MonadError Val Eval where
    throwError err = error "[BUG] throwError" -- do
        -- pos <- asks envPos'
        -- EvalT $ return (RException (errValPos err pos))
    (Eval m) `catchError` h = Eval $ \env r l -> do
      m env r (\x -> case h x of (Eval m') -> m' env r l)

{-|
Perform an IO action and raise an exception if it fails.
-}
guardIO :: IO a -> Eval a
guardIO x = do
    rv <- io $ tryIOError x
    case rv of
        Left e -> fail (show e)
        Right v -> return v

{-|
Like @guardIO@, perform an IO action and raise an exception if it fails.

If t
supress the exception and return an associated value instead.
-}
guardIOexcept :: MonadIO m => [((IOError -> Bool), a)] -> IO a -> m a
guardIOexcept = error "[BUG] guardIOexceptT" -- safetyNet x = do
    -- rv <- io $ tryIOError x
    -- case rv of
    --     Right v -> return v
    --     Left  e -> catcher e safetyNet
    -- where
    -- catcher e [] = fail (show e)
    -- catcher e ((f, res):safetyNets)
    --     | f e       = return res
    --     | otherwise = catcher e safetyNets

guardSTM :: STM a -> Eval a
guardSTM x = do
    rv <- stm $ fmap Right x `catchSTM` (\(e :: SomeException) -> return (Left e))
    case rv of
        Left e -> fail (show e)
        Right v -> return v
    
instance MonadSTM Eval where
    liftSIO m = Eval $ \env n l -> m >>= n
    liftSTM m = do
        atom <- asks envAtomic
        if atom
            then Eval $ \env n l -> stm m >>= n
            else Eval $ \env n l -> (io . stm) m >>= n

instance MonadReader Env Eval where
    ask = Eval $ \env right _ -> right env
    local f (Eval m) = Eval $ \env right left -> m (f env) right left

instance MonadCont Eval where
    callCC f = Eval $ \env right left ->
      case f (\x -> Eval $ \env' right' left' -> right x) of
        Eval ev -> ev env right left

{-
instance MonadEval Eval

class (MonadReader Env m, MonadCont m, MonadIO m, MonadSTM m) => MonadEval m
--     askGlobal :: m Pad
-}

