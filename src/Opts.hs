{-# LANGUAGE OverloadedStrings #-}

module Opts where

import Options.Applicative

data Opts = Opts {
        inputDataFilePath :: String,
        inputDataTake :: Int
    }

teasneezeOpts :: IO Opts
teasneezeOpts = execParser opts
    where opts = info ( helper <*> optsParser) ( fullDesc <> progDesc "TeaSneeze data visualizer" <> header "TeaSneeze" )

optsParser :: Parser Opts
optsParser = Opts <$> strOption ( long "input" <> metavar "PATH" <> help "Input data JSON file path" )
                  <*> option auto ( long "take" <> short 'n' <> metavar "NUMBER" <> value 100 <> help "Number of input values to use") 
