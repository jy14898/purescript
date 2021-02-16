module Language.PureScript.TypeClassDictionaries where

import Prelude.Compat

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Text (Text, pack)

import Language.PureScript.Names
import Language.PureScript.Types

--
-- Data representing a type class dictionary which is in scope
--
data TypeClassDictionaryInScope v
  = TypeClassDictionaryInScope {
    -- | The instance chain
      tcdChain :: [Qualified Ident]
    -- | Index of the instance chain
    , tcdIndex :: Integer
    -- | The value with which the dictionary can be accessed at runtime
    , tcdValue :: v
    -- | How to obtain this instance via superclass relationships
    , tcdPath :: [(Qualified (ProperName 'ClassName), Integer)]
    -- | The name of the type class to which this type class instance applies
    , tcdClassName :: Qualified (ProperName 'ClassName)
    -- | Quantification of type variables in the instance head and dependencies
    , tcdForAll :: [(Text, SourceType)]
    -- | The kinds to which this type class instance applies
    , tcdInstanceKinds :: [SourceType]
    -- | The types to which this type class instance applies
    , tcdInstanceTypes :: [SourceType]
    -- | Type class dependencies which must be satisfied to construct this dictionary
    , tcdDependencies :: Maybe [SourceConstraint]
    }
    deriving (Show, Functor, Foldable, Traversable, Generic)

instance NFData v => NFData (TypeClassDictionaryInScope v)

-- this needs to be Maybe (Qualified Ident)
-- or
-- Qualified (Maybe Ident)
--
-- but do I change it for everyone or just for type checker?
-- I would say just for the type checker, and then add elsewhere as needed
--
-- I think I need it in Environment... which is a big portion of its uses
type NamedDict = TypeClassDictionaryInScope (Qualified Ident)
type NamedDict' = TypeClassDictionaryInScope (Qualified (Maybe Ident))

-- | Generate a name for a superclass reference which can be used in
-- generated code.
superclassName :: Qualified (ProperName 'ClassName) -> Integer -> Text
superclassName pn index = runProperName (disqualify pn) <> pack (show index)
