{-# LANGUAGE OverloadedStrings #-}

module Opts where

import Options.Applicative

data Opts = Opts {
        inputDataFilePath :: String
    }

teasneezeOpts :: IO Opts
teasneezeOpts = execParser opts
    where opts = info ( helper <*> optsParser) ( fullDesc <> progDesc "TeaSneeze data visualizer" <> header "TeaSneeze" )

optsParser :: Parser Opts
optsParser = Opts <$> strOption ( long "input" <> metavar "PATH" <> help "Input data JSON file path" )
