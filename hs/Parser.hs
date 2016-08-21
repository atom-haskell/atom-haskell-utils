{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts #-}
module Parser where

import Data.Aeson
import Data.Maybe
import Text.Read (readMaybe)
import Control.Exception

import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Types

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Extension

instance ToJSON (ModuleHeadAndImports l) where
  toJSON (ModuleHeadAndImports _l _pragma mbModuleHead imports)
    = object [
        "name" .= mbName
      , "imports" .= imports
      ]
    where
      (mbName, _mbExports) = case mbModuleHead of
        Just (ModuleHead _l' (ModuleName _l mname) _warn mexports) -> (Just mname, Just mexports)
        Nothing -> (Just "Main", Nothing)

instance ToJSON (Namespace l) where
  toJSON (NoNamespace _) = toJSON ("none" :: String)
  toJSON (TypeNamespace _) = toJSON ("type" :: String)
  toJSON (PatternNamespace _) = toJSON ("pattern" :: String)

instance ToJSON (ImportDecl l) where
  toJSON ImportDecl{..}
    = object [
        "qualified" .= importQualified
      , "name" .= importModule
      , "alias" .= importAs
      , "hiding" .= hiding
      , "importList" .= importSpecs
      ]
    where
      hiding = case importSpecs of
        Just (ImportSpecList _ hiding' _) -> hiding'
        Nothing -> False

instance ToJSON (ModuleName l) where
  toJSON (ModuleName _ name) = toJSON name

instance ToJSON (ImportSpecList l) where
  toJSON (ImportSpecList _l _hiding list) = toJSON $ concatMap getImp list
    where
      getImp (IVar _ name) = [ toJSON name ]
      getImp (IAbs _ _ns name) = [ toJSON name ]
      getImp (IThingAll _ name) = [ toJSON name, object [ "parent" .= name ] ]
      getImp (IThingWith _ name cnames) = toJSON name : map toJSON cnames

instance ToJSON (Name l) where
  toJSON (Ident _ name) = toJSON name
  toJSON (Symbol _ name) = toJSON name

instance ToJSON (CName l) where
  toJSON (VarName _ name) = toJSON name
  toJSON (ConName _ name) = toJSON name

-- instance ToJSON (QName l) where
--   toJSON = undefined

pr :: JSVal -> ParseResult (NonGreedy (ModuleHeadAndImports SrcSpanInfo))
pr jsv = parseWithMode parseMode val
  where
    val = pFromJSVal jsv
    pragmas = let ListOf _ pragmas' = unNonGreedy . fromParseResult $ parse val in pragmas'
    langExts = concatMap getExts pragmas
    getExts (LanguagePragma _ exts) = map getName exts
    getExts _ = []
    getName :: Name SrcSpanInfo -> String
    getName (Ident _ n) = n
    getName (Symbol _ n) = n
    extensions = mapMaybe (fmap EnableExtension . readMaybe) langExts
    parseMode = defaultParseMode{extensions=extensions}

parseHsModuleImports :: JSVal -> IO JSVal
parseHsModuleImports = toJSVal . toJSON . unNonGreedy . fromParseResult . pr

parseHsModuleImportsNoThrow :: JSVal -> IO JSVal
parseHsModuleImportsNoThrow = (either enc1 return =<<) . try . parseHsModuleImports
  where
    enc1 (SomeException e) = toJSVal $
      object
        [ "error" .= show e ]
