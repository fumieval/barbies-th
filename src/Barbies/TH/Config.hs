module Barbies.TH.Config
  ( DeclareBareBConfig(..)
  , classic
  , passthrough
  ) where
import Language.Haskell.TH

-- | Keep it in a separate module until NoFieldSelectors gets widespread
data DeclareBareBConfig = DeclareBareBConfig
  { friends :: [Name] -- ^ Members with these types won't be wrapped with 'Wear'
  , bareName :: String -> Maybe String
  , coveredName :: String -> Maybe String
  , barbieName :: String -> String
  }

classic :: DeclareBareBConfig
classic = DeclareBareBConfig
  { friends = []
  , bareName = const Nothing
  , coveredName = const Nothing
  , barbieName = id
  }

-- | Defines a synonym for the bare type with the same name.
-- The strippable definition is suffixed by B, and the covered type is suffixed by H.
passthrough :: DeclareBareBConfig
passthrough = DeclareBareBConfig
  { friends = []
  , bareName = Just
  , coveredName = Just . (++"H")
  , barbieName = (++"B")
  }
