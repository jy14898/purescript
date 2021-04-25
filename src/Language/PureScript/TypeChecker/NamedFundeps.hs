module Language.PureScript.TypeChecker.NamedFundeps
  ( replaceAllNamedFundeps
  ) where

import           Prelude.Compat

import Data.List (groupBy, sortBy)
import Control.Arrow ((&&&))
import Data.Traversable (for)
import Data.Function (on)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.State
import           Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.List.NonEmpty as NEL
import           Language.PureScript.Environment
import           Language.PureScript.TypeClassDictionaries
import           Language.PureScript.Errors
import           Language.PureScript.Names
import           Language.PureScript.TypeChecker.Monad
import           Language.PureScript.Types
import Language.PureScript.PSString (PSString)

type TypeClassMap = M.Map (Qualified (ProperName 'ClassName)) TypeClassData
type TypeClassDictionaryMap = M.Map (Maybe ModuleName) (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict)))
type KindMap = M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)

type NamedFundepMap = M.Map (ProperName 'TypeName) (Qualified (ProperName 'ClassName))


-- maps fundeps to the class they belong to
namedFundeps :: TypeClassMap -> NamedFundepMap
namedFundeps classes = foldMap (\(cn, tc) -> foldMap (go cn) (fst <$> typeClassDependencies tc)) (M.toList classes)
  where

  go :: Qualified (ProperName 'ClassName) -> Maybe (ProperName 'TypeName) -> NamedFundepMap
  go cn (Just name) = M.singleton name cn
  go _ _ = M.empty

-- TODO Work out if I should have qualified the fundep, or made the names desugar step do it???
replaceAllNamedFundeps'
  :: forall m e. (e ~ MultipleErrors{-, MonadState CheckState m-}, MonadError e m)
  => TypeClassMap
  -> TypeClassDictionaryMap
  -> KindMap
  -> SourceType
  -> m SourceType
replaceAllNamedFundeps' classes dicts kinds = everywhereOnTypesTopDownM try
  where
  try :: SourceType -> m SourceType
  try t = (fromMaybe t) <$> go (fst $ getAnnForType t) 0 [] [] t

  go :: SourceSpan -> Int -> [SourceType] -> [SourceType] -> SourceType -> m (Maybe SourceType)
  go ss c kargs args (TypeConstructor _ ctor)
    | Just cn <- M.lookup (disqualify ctor) (namedFundeps classes)
    , Just tc <- M.lookup cn classes
    , Just (FunctionalDependency synArgs [determined]) <- M.lookup (Just $ disqualify ctor) (M.fromList $ typeClassDependencies tc)
    , c == length synArgs
    , kindArgs <- lookupKindArgs ctor
    , length kargs == length kindArgs
    = do
        let chains = fromMaybe [] $ do
              mdicts <- M.lookup (getQual cn) dicts
              cdicts <- M.lookup cn mdicts
              let cdicts' = (fmap NamedInstance) <$> (join (NEL.toList <$> M.elems cdicts))
              return $
                groupBy ((==) `on` tcdChain) $
                sortBy (compare `on` (tcdChain &&& tcdIndex)) $
                cdicts'
            chains' = flip fmap chains $ fmap $
              \(TypeClassDictionaryInScope{..}) ->
                let instTypes  = zip [(0 :: Int)..] tcdInstanceTypes
                    dictArgs   = map (\i -> fromJust $ lookup i instTypes) synArgs
                    dictResult = fromJust $ lookup determined instTypes
                in (dictArgs, dictResult, tcdValue)
        case (unique $ instances chains' args) of
          Just (substs, (_,result,_)) -> do
            let subst = fmap head substs
            -- TODO If there's variables in tcdForAll that didn't have a substitution, throw an error
            --      Because we don't wanna introduce foralls automatically?
            --      But we've thrown out some of the types, we need to recaculate the forall?
            --  let ty = foldl (TypeApp ann) (TypeConstructor ann (fmap coerceProperName clsName)) args
            --      tyWithConstraints = foldr srcConstrainedType ty constraints
            --      freeVars = freeTypeVariables tyWithConstraints

            return $ Just $ replaceAllTypeVars (M.toList subst) result
          Nothing -> throwError . errorMessage $ NoInstanceFound (srcConstraint cn [] args Nothing) False

    | Just cn <- M.lookup (disqualify ctor) (namedFundeps classes)
    , Just tc <- M.lookup cn classes
    , Just (FunctionalDependency synArgs _) <- M.lookup (Just $ disqualify ctor) (M.fromList $ typeClassDependencies tc)
    , length synArgs > c
    = throwError . errorMessage' ss $ PartiallyAppliedSynonym ctor
  go ss c kargs args (TypeApp _ f arg) = go ss (c + 1) kargs (arg : args) f
  go ss c kargs args (KindApp _ f arg) = go ss c (arg : kargs) args f
  go _ _ _ _ _ = return Nothing

  lookupKindArgs :: Qualified (ProperName 'TypeName) -> [Text]
  lookupKindArgs ctor = fromMaybe [] $ fmap (fmap (fst . snd) . fst) . completeBinderList . fst =<< M.lookup ctor kinds

replaceAllNamedFundeps :: (e ~ MultipleErrors, MonadState CheckState m, MonadError e m) => SourceType -> m SourceType
replaceAllNamedFundeps d = do
  env <- getEnv
  replaceAllNamedFundeps' (typeClasses env) (typeClassDictionaries env) (types env) d
  -- either throwError return r


data Matched t
  = Match t
  | Apart
  | Unknown
  deriving (Eq, Show, Functor)

instance Semigroup t => Semigroup (Matched t) where
  (Match l) <> (Match r) = Match (l <> r)
  Apart     <> _         = Apart
  _         <> Apart     = Apart
  _         <> _         = Unknown

instance Monoid t => Monoid (Matched t) where
  mempty = Match mempty


instances :: [[([SourceType], b, c)]] -> [SourceType] -> [(Matching [SourceType], ([SourceType], b, c))]
instances chains tys = do
  chain <- chains
  -- process instances in a chain in index order
  let found = for chain $ \tcd@(args,_,_) ->
                -- Make sure the type unifies with the type in the type instance definition
                case matches args tys of
                  Apart        -> Right ()                  -- keep searching
                  Match substs -> Left (Just (substs, tcd)) -- found a match
                  Unknown      -> Left Nothing              -- can't continue with this chain yet, need proof of apartness
  case found of
    Right _               -> []          -- all apart
    Left Nothing          -> []          -- last unknown
    Left (Just substsTcd) -> [substsTcd] -- found a match

data Evidence
  -- | An existing named instance
  = NamedInstance (Qualified Ident)

  -- | Computed instances
  | WarnInstance SourceType -- ^ Warn type class with a user-defined warning message
  | IsSymbolInstance PSString -- ^ The IsSymbol type class for a given Symbol literal
  | EmptyClassInstance        -- ^ For any solved type class with no members
  deriving (Show, Eq)

type Matching a = M.Map Text a

matches :: [SourceType] -> [SourceType] -> Matched (Matching [SourceType])
matches instanceTys tys = foldr both (Match (M.empty)) $ zipWith typeHeadsAreEqual tys instanceTys
  where
    --
    -- Check whether the type heads of two types are equal (for the purposes of type class dictionary lookup),
    -- and return a substitution from type variables to types which makes the type heads unify.
    --
    typeHeadsAreEqual :: Type a -> Type a -> Matched (Matching [Type a])
    typeHeadsAreEqual (KindedType _  t1 _) t2                                  = typeHeadsAreEqual t1 t2
    typeHeadsAreEqual t1                     (KindedType _ t2 _)               = typeHeadsAreEqual t1 t2
    typeHeadsAreEqual (TUnknown _ u1)        (TUnknown _ u2)      | u1 == u2   = Match (M.empty)
    typeHeadsAreEqual (Skolem _ _ _ s1 _)      (Skolem _ _ _ s2 _)    | s1 == s2   = Match (M.empty)
    typeHeadsAreEqual t                      (TypeVar _ v)                     = Match (M.singleton v [t])
    typeHeadsAreEqual (TypeConstructor _ c1) (TypeConstructor _ c2) | c1 == c2 = Match (M.empty)
    typeHeadsAreEqual (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = Match (M.empty)
    typeHeadsAreEqual (TypeApp _ h1 t1)      (TypeApp _ h2 t2)                 =
      both (typeHeadsAreEqual h1 h2) (typeHeadsAreEqual t1 t2)
    typeHeadsAreEqual (KindApp _ h1 t1)      (KindApp _ h2 t2)                 =
      both (typeHeadsAreEqual h1 h2) (typeHeadsAreEqual t1 t2)
    typeHeadsAreEqual (REmpty _) (REmpty _) = Match (M.empty)
    typeHeadsAreEqual r1@RCons{} r2@RCons{} =
        foldr both (uncurry go rest) common
      where
        (common, rest) = alignRowsWith typeHeadsAreEqual r1 r2

        go :: ([RowListItem a], Type a) -> ([RowListItem a], Type a) -> Matched (Matching [Type a])
        go (l,  KindedType _ t1 _) (r,  t2)                            = go (l, t1) (r, t2)
        go (l,  t1)                (r,  KindedType _ t2 _)             = go (l, t1) (r, t2)
        go (l,  KindApp _ t1 k1)   (r,  KindApp _ t2 k2) | eqType k1 k2 = go (l, t1) (r, t2)
        go ([], REmpty _)          ([], REmpty _)                      = Match (M.empty)
        go ([], TUnknown _ u1)     ([], TUnknown _ u2)      | u1 == u2 = Match (M.empty)
        go ([], TypeVar _ v1)      ([], TypeVar _ v2)       | v1 == v2 = Match (M.empty)
        go ([], Skolem _ _ _ sk1 _)  ([], Skolem _ _ _ sk2 _) | sk1 == sk2 = Match (M.empty)
        go ([], TUnknown _ _)      _                                   = Unknown
        go (sd, r)                 ([], TypeVar _ v)                   = Match (M.singleton v [rowFromList (sd, r)])
        go _ _                                                         = Apart
    typeHeadsAreEqual (TUnknown _ _) _ = Unknown
    typeHeadsAreEqual _ _ = Apart

    both :: Matched (Matching [Type a]) -> Matched (Matching [Type a]) -> Matched (Matching [Type a])
    both (Match l) (Match r) = Match (M.unionWith (++) l r)
    both Apart     _         = Apart
    both _         Apart     = Apart
    both _         _         = Unknown

unique :: [(a, ([SourceType], SourceType, Evidence))] -> Maybe (a, ([SourceType], SourceType, Evidence))
unique [] = Nothing
unique [(a, dict)] = Just (a, dict)
-- For now, ignore superclass introductions etc
unique _ = Nothing
--  | pairwiseAny (\(_,_,v1) (_,_,v2) -> v1 /= v2) (map snd tcds) = Nothing
--  | otherwise = Just (minimumBy (compare `on` length . tcdPath . snd) tcds)

