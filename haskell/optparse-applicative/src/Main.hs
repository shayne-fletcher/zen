module Main where

import Options.Applicative
import Data.Version (showVersion)
import Data.Semigroup ((<>))
import Paths_optparse_scratch (version)

-- | A ghc-lib-gen target.
data GhclibgenTarget = Ghclib_parser | Ghclib

-- | The type of ghc-lib-gen options.
data GhclibgenOpts = GhclibgenOpts {
    ghclibgenOpts_root :: !FilePath -- ^ Path to a GHC git repository.
  , ghclibgenOpts_target :: !(Maybe GhclibgenTarget) -- ^ What target?
 }

-- | A parser of the "--ghc-lib" target.
ghclib :: Parser GhclibgenTarget
ghclib = flag' Ghclib
  ( long "ghc-lib"
  <> help "Generate a ghc-lib.cabal"
  )

-- | A parser of the "--ghc-lib-parser" target.
ghclibParser :: Parser GhclibgenTarget
ghclibParser = flag' Ghclib_parser
  ( long "ghc-lib-parser"
  <> help "Generate a ghc-lib-parser.cabal"
  )

-- | A parser of "--version".
ghclibgenVersion :: Parser (a -> a)
ghclibgenVersion =
  infoOption
  (showVersion version)
  (long "version" <> help "Show version")

-- | A parser of a ghc-lib-gen target: `target := | "--ghc-lib-parser"
-- | "--ghc-lib" | /* nothing */`.
ghclibgenTarget :: Parser (Maybe GhclibgenTarget)
ghclibgenTarget = optional (ghclibParser <|> ghclib)

-- | A parser of ghc-lib-gen options: `opts := STRING target`.
ghclibgenOpts :: Parser GhclibgenOpts
ghclibgenOpts = GhclibgenOpts
  <$> argument str (metavar "GHC_ROOT")
  <*> optional (ghclibParser <|> ghclib)

main :: IO ()
main = echo =<< execParser opts
  where
    opts =
      info
      (helper <*> ghclibgenVersion <*> ghclibgenOpts)
      (fullDesc
        <> header "ghc-lib-gen - ghc-lib cabal file generator"
        <> progDesc "Generate a ghc-lib target Cabal file"
      )

echo :: GhclibgenOpts -> IO ()
echo (GhclibgenOpts root target) =
  putStrLn $ root
    ++ " --"
    ++ case target of
         Just Ghclib_parser -> "ghc-lib-parser"
         Just Ghclib -> "ghc-lib"
         Nothing -> ""
