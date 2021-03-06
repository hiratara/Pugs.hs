{-# LANGUAGE BangPatterns #-}
module DrIFT.YAML where
import Data.Yaml.Syck
import Data.Ratio
import GHC.Exts hiding (toList)
import Data.Typeable
import Data.Char
import Control.Exception
import Control.Concurrent.STM
import Foreign.Ptr
import Control.Monad.Reader
import GHC.PArr
import System.IO.Unsafe
import Data.IORef
import Data.Bits
import Data.Sequence (Seq)
import Data.Foldable (toList)
import Data.List	( foldl' )
import Data.Int		( Int32, Int64 )
import Codec.Binary.UTF8.String (encodeString, decodeString)
-- import Pugs.Internals (addressOf, safeMode)
import Data.HashTable.IO (BasicHashTable)
import qualified Data.IntSet as IntSet
import qualified Data.HashTable.IO as Hash
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Sequence as Seq
import Data.Hashable (Hashable(..))

type Buf = S.ByteString

type YAMLClass = String
type YAMLKey = String
type YAMLVal = YamlNode
type SeenCache = IORef IntSet.IntSet

toYamlString :: YAML a => a -> IO String
toYamlString = showYaml

fromYamlString :: YAML a => String -> IO a
fromYamlString str = do
    yaml <- parseYaml str
    fromYAML yaml

toYamlNode :: YAML a => a -> IO YamlNode
toYamlNode x = do
    cache   <- newIORef IntSet.empty 
    runReaderT (asYAML x) cache

showYaml :: YAML a => a -> IO String
showYaml x = do
    node <- toYamlNode x
    emitYaml node

showYamlCompressed :: YAML a => a -> IO String
showYamlCompressed x = do -- if safeMode then showYaml x else do
    node    <- toYamlNode x
    node'   <- compressYamlNode node
    emitYaml node'

type EmitAs = ReaderT SeenCache IO

class Typeable a => YAML a where
    asYAML :: a -> EmitAs YamlNode
    asYAML x = lift $ do
        ty <- Control.Exception.handle (\(e :: SomeException) -> return "()") $
            evaluate (show (typeOf x))
        case ty of
            "()" -> return nilNode
            _    -> return $ mkTagNode (tagHs ty) ENil
    fromYAML :: YamlNode -> IO a
    fromYAML = fromYAMLElem . n_elem
    fromYAMLElem :: YamlElem -> IO a
    fromYAMLElem e = do
        fail $ "unhandled element: " ++ (show e) ++ ", expecting " ++ show (typeOf (undefined :: a))

asYAMLseq :: YAMLClass -> [EmitAs YAMLVal] -> EmitAs YamlNode
asYAMLseq c ps = do
    ps' <- sequence ps
    return $ mkTagNode (tagHs c) (ESeq ps')

asYAMLmap :: YAMLClass -> [(YAMLKey, EmitAs YAMLVal)] -> EmitAs YamlNode
asYAMLmap c ps = do
    ps' <- mapM asYAMLpair ps
    return $ mkTagNode (tagHs c) (EMap ps')
    where
    asYAMLpair (k, v) = do
        k' <- asYAML k
        v' <- v
        return (k', v')

asYAMLmapBuf :: YAMLClass -> [(S.ByteString, EmitAs YAMLVal)] -> EmitAs YamlNode
asYAMLmapBuf c ps = do
    ps' <- mapM asYAMLpair ps
    return $ mkTagNode (tagHs c) (EMap ps')
    where
    asYAMLpair (k, v) = do
        k' <- asYAML k
        v' <- v
        return (k', v')

fromYAMLseq :: forall a. YAML a => YamlNode -> IO [a]
fromYAMLseq MkNode{n_elem=ESeq m} = mapM fromYAML m
fromYAMLseq e = fail $ "no parse: " ++ show e ++ ", expecting seq of " ++ show (typeOf (undefined :: a))

fromYAMLmap :: forall a. YAML a => YamlNode -> IO [(String, a)]
fromYAMLmap MkNode{n_elem=EMap m} = mapM fromYAMLpair m
    where
    fromYAMLpair (MkNode{n_elem=EStr k}, v) = do
        v' <- fromYAML v
        return (unpackBuf k, v')
    fromYAMLpair e = fail $ "no parse: " ++ show e
fromYAMLmap e = fail $ "no parse: " ++ show e ++ ", expecting map of " ++ show (typeOf (undefined :: a))

fromYAMLmapBuf :: forall a. YAML a => YamlNode -> IO [(S.ByteString, a)]
fromYAMLmapBuf MkNode{n_elem=EMap m} = mapM fromYAMLpair m
    where
    fromYAMLpair (MkNode{n_elem=EStr k}, v) = do
        v' <- fromYAML v
        return (k, v')
    fromYAMLpair e = fail $ "no parse: " ++ show e ++ ", expecting pair of " ++ show (typeOf (undefined :: a))
fromYAMLmapBuf e = fail $ "no parse: " ++ show e ++ ", expecting mapping of " ++ show (typeOf (undefined :: a))

asYAMLcls :: YAMLClass -> EmitAs YamlNode
asYAMLcls c = return $ mkTagStrNode (tagHs c) c

tagHs :: YAMLClass -> String
tagHs = ("tag:hs:" ++)

deTag :: YamlNode -> YAMLClass
deTag MkNode{n_tag=Just s} =
    case s' of
        't':'a':'g':':':'h':'s':':':tag -> tag
        tag                             -> error $ "not a Haskell tag: " ++ tag
    where s' = unpackBuf s
deTag n = error $ "missing tag: " ++ show n

instance YAML () where
    asYAML _ = return nilNode
    fromYAMLElem _ = return ()

instance YAML Int where
    asYAML x = return $ mkTagStrNode "int" $ show x
    fromYAMLElem (EStr x) = return $ read $ S.unpack x
    fromYAMLElem e = failWith e

instance YAML Word where
    asYAML x = return $ mkTagStrNode "int" $ show x
    fromYAMLElem (EStr x) = return $ read $ S.unpack x
    fromYAMLElem e = failWith e

instance YAML Buf where
    asYAML = return . mkNode . EStr
    fromYAMLElem (EStr str) = return str
    fromYAMLElem e = failWith e

instance YAML String where
    asYAML = return . mkTagNode "str" . EStr . S.pack . encodeString
    fromYAMLElem (EStr str) = return . decodeString $ S.unpack str
    fromYAMLElem e = failWith e

instance YAML Bool where
    asYAML True = return $ mkTagStrNode "bool#yes" "1"
    asYAML False = return $ mkTagStrNode "bool#no" "0"
    fromYAML MkNode{n_tag=Just s} | s == packBuf "bool#yes" = return True
    fromYAML MkNode{n_tag=Just s} | s == packBuf "bool#no"  = return False
    fromYAML MkNode{n_elem=x} = fromYAMLElem x
    fromYAMLElem (EStr x) = return (x /= packBuf "0")
    fromYAMLElem e = failWith e

instance YAML Integer where 
    asYAML x = return $ mkTagStrNode "int" $ show x
    fromYAMLElem (EStr x) = return $ read $ S.unpack x
    fromYAMLElem e = failWith e

instance YAML Rational where 
    asYAML r = asYAML (x, y)
        where
        x = numerator r
        y = denominator r
    fromYAMLElem (ESeq [MkNode{n_elem=EStr x}, MkNode{n_elem=EStr y}]) =
        return $ (read $ S.unpack x) % (read $ S.unpack y)
    fromYAMLElem e = failWith e
    
instance YAML Double where 
    asYAML num
        | show num == "Infinity"  = return $ mkTagStrNode "float#inf"    ".Inf"
        | show num == "-Infinity" = return $ mkTagStrNode "float#neginf" "-.Inf"
        | show num == "NaN"       = return $ mkTagStrNode "float#nan"    "-.NaN"
        | otherwise               = return $ mkTagStrNode "float"        $ show num
    fromYAML MkNode{n_tag=Just s} | s == packBuf "float#inf"    = return $  1/0 -- "Infinity" 
    fromYAML MkNode{n_tag=Just s} | s == packBuf "float#neginf" = return $ -1/0 -- "-Infinity" 
    fromYAML MkNode{n_tag=Just s} | s == packBuf "float#nan"    = return $  0/0 -- "NaN" 
    fromYAML MkNode{n_elem=x} = fromYAMLElem x
    fromYAMLElem (EStr x) = return $ read $ S.unpack x
    fromYAMLElem e = failWith e

instance (YAML a) => YAML (Maybe a) where
    asYAML (Just x) = asYAML x
    asYAML Nothing = return $ nilNode
    fromYAML MkNode{n_elem=ENil} = return Nothing
    fromYAML x = return . Just =<< fromYAML x
    fromYAMLElem ENil = return Nothing
    fromYAMLElem x = return . Just =<< fromYAMLElem x

instance (YAML a) => YAML [a] where
    asYAML xs = do -- asYAMLanchor xs $ do
        xs' <- mapM asYAML xs
        (return . mkNode . ESeq) xs'
    fromYAML MkNode{n_elem=(ESeq s)} = mapM fromYAML s
    fromYAML n = fail $ "no parse: " ++ show n ++ ", expecting list of " ++ show (typeOf (undefined :: a))
    fromYAMLElem (ESeq s) = mapM fromYAML s
    fromYAMLElem e = fail $ "no parse: " ++ show e ++ ", expecting list of " ++ show (typeOf (undefined :: a))

instance (YAML a) => YAML (Seq a) where
    asYAML xs = do -- asYAMLanchor xs $ do
        xs' <- mapM asYAML $ toList xs
        (return . mkNode . ESeq) xs'
    fromYAMLElem (ESeq s) = fmap Seq.fromList (mapM fromYAML s)
    fromYAMLElem e = fail $ "no parse: " ++ show e ++ ", expecting array of " ++ show (typeOf (undefined :: a))

instance (YAML a, YAML b) => YAML (a, b) where
    asYAML (x, y) = do
        x' <- asYAML x
        y' <- asYAML y
        return $ mkNode (ESeq [x', y'])
    fromYAMLElem (ESeq [x, y]) = do
        x' <- fromYAML x
        y' <- fromYAML y
        return (x', y')
    fromYAMLElem e = fail $ "no parse: " ++ show e ++ ", expecting " ++ show (typeOf (undefined :: (a, b)))

instance (YAML a, YAML b, YAML c) => YAML (a, b, c) where
    asYAML (x, y, z) = do
        x' <- asYAML x
        y' <- asYAML y
        z' <- asYAML z
        return $ mkNode (ESeq [x', y', z'])
    fromYAMLElem (ESeq [x, y, z]) = do
        x' <- fromYAML x
        y' <- fromYAML y
        z' <- fromYAML z
        return (x', y', z')
    fromYAMLElem e = fail $ "no parse: " ++ show e ++ ", expecting " ++ show (typeOf (undefined :: (a, b, c)))

{-
{-# NOINLINE seen #-}
seen :: Hash.HashTable SYMID Any
seen = unsafePerformIO (Hash.new (==) fromIntegral)

cleanSeen :: IO ()
cleanSeen = do
    kvs <- Hash.toList seen
    mapM_ (Hash.delete seen . fst) kvs
-}

instance (Typeable a, YAML a) => YAML (TVar a) where
    asYAML x = do -- asYAMLanchor x $ do
        -- asYAMLseq "TVar" . (:[]) $ do
            content <- (lift . atomically . readTVar) x
            asYAML content
    fromYAML = (newTVarIO =<<) . fromYAML
    fromYAMLElem = (newTVarIO =<<) . fromYAMLElem
    {-
    fromYAML n@MkNode{n_id=nid} = do
        -- If this node is seen, then don't bother -- just read from it.
        rv  <- Hash.lookup seen nid
        case rv of
            Just x  -> do
                -- print ("hit", nid)
                return (unsafeCoerce# x)
            _       -> do
                -- print ("stored", nid)
                tv  <- newTVarIO (error $ "value of TV demanded before cycle completes: " ++ show (typeOf (undefined :: a)))
                Hash.insert seen nid (unsafeCoerce# tv)
                j   <- fromYAML n
                atomically (writeTVar tv j)
                return tv
    -}
{-
    fromYAML node = do
        fail $ "Want (TVar " ++ show node ++ "|" ++ show (typeOf (undefined :: a)) ++ "), got moose: " ++ show node
-}


asYAMLanchor :: a -> EmitAs YamlNode -> EmitAs YamlNode
asYAMLanchor x m = do
    cache   <- ask
    seen    <- liftIO $ readIORef cache
    let ptr = 1000 + fromEnum (addressOf x)
    if IntSet.member ptr seen
        then return nilNode{ n_anchor = AReference ptr } 
        else do
            liftIO $ modifyIORef cache (IntSet.insert ptr)
            rv  <- m
            return rv{ n_anchor = AAnchor ptr }
    where
    {-# INLINE addressOf #-}
    addressOf :: a -> Word
    addressOf !x = W# (unsafeCoerce# x)

asYAMLwith :: (YAML a, YAML b) => (a -> EmitAs b) -> a -> EmitAs YamlNode
asYAMLwith f x = asYAMLanchor x (asYAML =<< f x)

failWith :: forall a. YAML a => YamlElem -> IO a
failWith e = fail $ "no parse: " ++ show e ++ " as " ++ show typ
    where
    typ :: TypeRep
    typ = typeOf (undefined :: a)


type SeenHash = BasicHashTable SYMID (Maybe YamlNode)
type DuplHash = BasicHashTable YamlNode Int

-- Compress a YAML tree by finding common subexpressions.
compressYamlNode :: YamlNode -> IO YamlNode
compressYamlNode node = do
    -- Phase 1: Update YamlNode to fill in SYMID based on hashing its values.
    --          First time a SYMID is seen, firstTime (Hash from SYMID to (Maybe YamlNode))
    --          is inserted.  Next time, both YamlNodes are written to the dupNode
    --          hash (Hash from YamlNode to Int), and firstTime now contains Nothing.
    seen    <- Hash.new
    dupl    <- Hash.new
    count   <- newIORef 1

    let ?seenHash = seen
        ?duplHash = dupl
        ?countRef = count
    
    node' <- markNode node

    -- Phase 2: Revisit YamlNode and lookup dupNode; if it's 0 then increment curId
    --          and mark this as AAnchor; otherwise retrieve and mark this as AReference.
    visitNode node'

eqNode :: YamlNode -> YamlNode -> Bool
eqNode x y = (n_tag x == n_tag y) && eqElem (n_elem x) (n_elem y)

eqElem :: YamlElem -> YamlElem -> Bool
eqElem ENil         ENil        = True
eqElem (EStr x)     (EStr y)    = x == y
eqElem (ESeq xs)    (ESeq ys)   = and ((length xs == length ys):zipWith eqNode xs ys)
eqElem (EMap xs)    (EMap ys)   = and ((length xs == length ys):zipWith eqPair xs ys)
    where
    eqPair (kx, vx) (ky, vy)    = eqNode kx ky && eqNode vx vy
eqElem _            _           = False


visitNode :: (?countRef :: IORef Int, ?duplHash :: DuplHash) => YamlNode -> IO YamlNode
visitNode node = do
    rv  <- Hash.lookup ?duplHash node
    case rv of
        Just 0  -> do
            i   <- readIORef ?countRef
            hashUpdate ?duplHash node i 
            writeIORef ?countRef (i+1)
            elem'   <- visitElem (n_elem node)
            return node{ n_anchor = AAnchor i, n_elem = elem' }
        Just i  -> return nilNode{ n_anchor = AReference i }
        _       -> do
            elem'   <- visitElem (n_elem node)
            return node{ n_elem = elem' }

visitElem :: (?countRef :: IORef Int, ?duplHash :: DuplHash) => YamlElem -> IO YamlElem
visitElem (ESeq ns)      = fmap ESeq (mapM visitNode ns)
visitElem (EMap ps)      = fmap EMap (mapM visitPair ps)
    where
    visitPair (k, v) = do
        k'  <- visitNode k
        v'  <- visitNode v
        return (k', v')
visitElem e             = return e

markNode :: (?seenHash :: SeenHash, ?duplHash :: DuplHash) => YamlNode -> IO YamlNode
markNode node = do
    (symid32, elem')    <- markElem (n_elem node)
    let node' = node{ n_id = symid }
        symid = fromIntegral (iterI32s tagid symid32)
        tagid = maybe 0 hashByteString (n_tag node)
    rv  <- Hash.lookup ?seenHash symid
    case rv of
        Just (Just prevNode)   -> do
            hashUpdate ?duplHash node' 0
            hashUpdate ?duplHash prevNode 0
            hashUpdate ?seenHash symid Nothing
        Just _  -> hashUpdate ?duplHash node' 0
        _       -> hashUpdate ?seenHash symid (Just node')
    return node'{ n_elem = elem' }

markElem :: (?seenHash :: SeenHash, ?duplHash :: DuplHash) => YamlElem -> IO (Int32, YamlElem)
markElem ENil           = return (10000, ENil)
markElem n@(EStr buf)   = return (hashByteString buf, n)
markElem (ESeq ns)      = do
    ns' <- mapM markNode ns
    return (hashIDs (map n_id ns'), ESeq ns')
markElem (EMap ps)      = do
    (symid, ps') <- foldM markPair (20000, []) ps
    return (symid, EMap ps')
    where
    markPair (symid, ps) (k, v) = do
        k'  <- markNode k
        v'  <- markNode v
        return (iterIDs (iterIDs symid (n_id k')) (n_id v'), ((k', v'):ps))

hashIDs :: [SYMID] -> Int32
hashIDs = foldl' iterIDs 30000

iterIDs :: Int32 -> SYMID -> Int32
iterIDs m c = fromIntegral (c + 1) * golden + mulHi m golden

iterI32s :: Int32 -> Int32 -> Int32
iterI32s m c = (c + 1) * golden + mulHi m golden

golden :: Int32
golden = -1640531527

mulHi :: Int32 -> Int32 -> Int32
mulHi a b = fromIntegral (r `shiftR` 32)
    where
    r :: Int64
    r = fromIntegral a * fromIntegral b :: Int64

hashByteString :: S.ByteString -> Int32
hashByteString = BS.foldl' f golden
    where
    f m c = fromIntegral c * magic + hashInt32 m
    magic = 0xdeadbeef
    golden :: Int32
    golden = 1013904242 -- = round ((sqrt 5 - 1) * 2^32) :: Int32
    hashInt32 :: Int32 -> Int32
    hashInt32 x = mulHi x golden + x
    mulHi a b = fromIntegral (r `shiftR` 32)
        where
        r :: Int64
        r = fromIntegral a * fromIntegral b

hashUpdate :: (Eq k, Hashable k) => BasicHashTable k v -> k -> v -> IO Bool
hashUpdate h k v = do
    v' <- Hash.lookup h k
    Hash.insert h k v
    case v' of
        Just _ -> return True
        Nothing -> return False

instance Hashable YamlNode where
    hashWithSalt salt i = let i' = fromIntegral . n_id $ i :: Int
                           in hashWithSalt salt i'

instance Hashable SYMID where
    hashWithSalt salt i = let i' = fromIntegral i :: Int
                           in hashWithSalt salt i'
