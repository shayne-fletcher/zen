module Main where

import System
import System.Environment 
import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import System.Console.GetOpt
import Data.Time
import Data.Time.Calendar.OrdinalDate

import Date
import Curve
import Interpolation

data Flag = Version | Help | Src String | Dst String 
     deriving Show

options :: [OptDescr Flag]
options = 
  [ Option ['v']  
           ["version"] 
           (NoArg Version)      
           "Show version number"
  , Option ['h']   
           ["help"]    
           (NoArg Help)         
           "Show help message"
  ]

progOpts prog argv = 
    case getOpt Permute options argv of
      (o, n, [])   -> return (o, n)
      (_, _, errs) -> ioError (
                         userError (
                            concat errs ++ usageInfo header options))
  where header = "usage : " ++ prog ++ " [-h | -v]"

hasHelp [] = False
hasHelp (Help : _) = True
hasHelp (_ : fs) = hasHelp fs

hasVersion [] = False
hasVersion (Version : fs) = True
hasVersion (_ : fs) = hasVersion fs

bootstrapCurve d deps swaps interp = 
  (Curve [today] [0] [1] interp) `bootstrap` deps `bootstrap` swaps

factors_ crv start end day period add_duration
     | day < start || day >= end = []
     | otherwise = 
         (diffDays day start, (df crv (year_diff day today))):
           (factors_ crv start end 
                 (add_duration period day) period add_duration)
factors crv start end period res =  
    factors_ crv start end start period (add_duration_func res)

renderFactors [] = ""
renderFactors (x:xs) = 
  show ((fromIntegral (fst x))/365.0) 
           ++ " " ++ show (snd x) ++ "\n" ++ (renderFactors xs)

replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

main = do
  prog <- getProgName
  argv <- getArgs
  opts <- progOpts prog argv
  let (flags, _) = opts

  if hasHelp flags 
    then putStr (
           usageInfo ("Usage : " ++ prog ++ " [-h | -v]") options)
    else 
      if hasVersion flags
        then putStr $ "'" ++ prog ++ "' " ++ "Version 0.0.0"
      else do

        let today = fromGregorian 2004 8 24
        let deposits = [ 
                         mk_deposit 0.00040 (add_days  1 today) 1 DAYS     -- O/N
                       , mk_deposit 0.00040 (add_mnths 1 today) 1 MONTHS   --  1m
                       , mk_deposit 0.00054 (add_mnths 2 today) 2 MONTHS   --  2m
                       , mk_deposit 0.00054 (add_mnths 3 today) 3 MONTHS   --  3m
                       , mk_deposit 0.00066 (add_mnths 6 today) 6 MONTHS   --  6m
                       ]
        let swaps =    [
                         mk_swap 0.0007600 (add_years   1 today) 1 YEARS  --   1y
                       , mk_swap 0.0010400 (add_mnths  18 today) 6 MONTHS --  18m
                       , mk_swap 0.0015100 (add_years   2 today) 6 MONTHS --   2y
                       , mk_swap 0.0026400 (add_years   3 today) 6 MONTHS --   3y
                       , mk_swap 0.0039800 (add_years   4 today) 6 MONTHS --   4y
                       , mk_swap 0.0054900 (add_years   5 today) 6 MONTHS --   5y
                       , mk_swap 0.0071300 (add_years   6 today) 6 MONTHS --   6y
                       , mk_swap 0.0088000 (add_years   7 today) 6 MONTHS --   7y
                       , mk_swap 0.0104400 (add_years   8 today) 6 MONTHS --   8y
                       , mk_swap 0.0119400 (add_years   9 today) 6 MONTHS --   9y
                       , mk_swap 0.0132500 (add_years  10 today) 6 MONTHS --  10y
                       , mk_swap 0.0154000 (add_years  12 today) 6 MONTHS --  12y
                       , mk_swap 0.0177600 (add_years  15 today) 6 MONTHS --  15y
                       , mk_swap 0.0204500 (add_years  20 today) 6 MONTHS --  20y
                       , mk_swap 0.0219900 (add_years  25 today) 6 MONTHS --  25y
                       ]
        let crv = bootstrapCurve today deposits swaps loglinear_interpolation 

        tempdir <- catch(getTemporaryDirectory)(\_->return ".")
        (datfile, datH) <- openTempFile tempdir "dfs.dat"
        hPutStr datH (renderFactors $ factors crv today (add_years 25 today) 1  MONTHS)
        hClose datH

        (plotfile, plotH) <- openTempFile tempdir "plot.plt"
        hPutStrLn plotH "set terminal windows"
        hPutStrLn plotH "set nokey"
        hPutStrLn plotH "set pointsize 2"
        hPutStrLn plotH "set title \"Discount Factors (25y)\" font \"Lucida Console\""
        hPutStrLn plotH "set xlabel \"t (years)\" font \"Lucida Console\""
        hPutStrLn plotH "set ylabel \"P(t)\" font \"Lucida Console\""
        hPutStrLn plotH "set autoscale"
        hPutStrLn plotH "set grid"
        hPutStrLn plotH ("plot \""++(replace datfile "\\" "\\\\")++"\" smooth csplines")
        hPutStrLn plotH "pause -1"
        hClose plotH

        system $ "wgnuplot " ++ plotfile

        removeFile plotfile
        removeFile datfile

        return ()
