module Main where

import Options.Applicative
import Data.Version (showVersion)
import Data.Semigroup ((<>))
import Paths_optparse_scratch (version)

data Ghclibgen_target = Ghclib_parser | Ghclib

ghc_lib_parser :: Parser Ghclibgen_target
ghc_lib_parser = flag' Ghclib_parser
  ( long "ghc-lib-parser"
  <> help "Generate a ghc-lib-parser Cabal file"
  )

ghc_lib :: Parser Ghclibgen_target
ghc_lib = flag' Ghclib
  ( long "ghc-lib"
  <> help "Generate a ghc-lib Cabal file"
  )

ghclibgen_target :: Parser (Maybe Ghclibgen_target)
ghclibgen_target = optional (ghc_lib_parser <|> ghc_lib)

data Ghclibgen_opts = Ghclibgen_opts {
    opts_root :: FilePath
  , opts_component :: (Maybe Ghclibgen_target)
 }

ghclibgen_opts :: Parser Ghclibgen_opts
ghclibgen_opts =
  Ghclibgen_opts
  <$> argument str (metavar "GHC_ROOT")
  <*> optional (ghc_lib_parser <|> ghc_lib)

version_opt :: Parser (a -> a)
version_opt =
  infoOption
    (showVersion version)
    (long "version" <> help "Show version")

main :: IO ()
main = echo =<< execParser opts
  where
    opts =info
      (helper <*> version_opt <*> ghclibgen_opts)
      (fullDesc
        <> header "ghc-lib-gen - ghc-lib cabal file generator"
        <> progDesc "Generate a Cabal file for a ghc-lib component"
      )

echo :: Ghclibgen_opts -> IO ()
echo (Ghclibgen_opts root target) =
  putStrLn $ root
    ++ " --"
    ++ case target of
         Just Ghclib_parser -> "ghc-lib-parser"
         Just Ghclib -> "ghc-lib"
         Nothing -> ""
