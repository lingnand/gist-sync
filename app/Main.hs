{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import           Control.Applicative ((<|>), Alternative(..))
import           Data.Default
import           Data.Semigroup
import qualified Data.Text as T
import           Data.Vinyl
  ((<<$>>), (<<*>>), (=:), ElField(..), (:::), Rec(..)
  , rcast, rtraverse, rvalf, rgetf)
import           Data.Vinyl.Functor (Const(..))
import qualified Filesystem.Path.CurrentOS as P
import qualified Options.Applicative as O
import           System.IO
import qualified Turtle as Ttl

#ifndef DISABLE_BRICK
import qualified App.Brick as BkApp
#endif
import qualified App.Text as TextApp
import qualified App.Core as Core
import           App.Types.Core (Config, mkFieldParser)
import qualified App.Types.Core as Core
import           Utils ((:.), Lift(..), Compose(..))

type Opts = ("config" ::: P.FilePath)
         ': Config
type CLIOpts f = Rec (f :. ElField) Opts

cliOptsParser
  :: forall f. Alternative f => O.Parser (CLIOpts f)
cliOptsParser =
  rtraverse getCompose $
    parseMapper <<$>> Core.configLabels <<*>>
    (  mkFieldParser (pure . (#config =:) . P.fromText . T.pack)
    :& Core.configParser
    )
  where
    pack3 = Compose . fmap Compose
    parseMapper
      :: Const String x  -- label name
      -> Lift (->)
           (Core.FieldParser O.ReadM)
           -- ^ parser function
           (O.Parser :. f :. ElField)
           -- ^ return the option parser, f is used to store empty value when
           -- flag unspecified (all flags are optional)
           x
    parseMapper clabel = Lift $ \parser -> pack3 $
          pure <$> O.option (O.str >>= Core.runFieldParser parser) (O.long label)
      <|> pure empty
      where label = getConst clabel

runApp :: IO ()
runApp =  do
  opts <- Ttl.options "The Gist synchronization client" cliOptsParser
  confFromFile <- either (return . const def)
     (\(Field f) ->
        either (error . (("Cannot load config file "++show f++": ")++) . show) id
        <$> Core.appConfigFromYaml f)
     (getCompose $ rgetf #config opts)
  conf <- either (fail . ("Config error: "++)) return
        . rtraverse getCompose $ rcast opts
                              <> confFromFile
                              <> def
  hSetBuffering stdout LineBuffering

  putStrLn "== Conf =="
  print conf

  -- communication chans
  (syncEnv, syncState0) <- Core.initSyncStates conf

  case rvalf #ui conf of
    Core.Brick -> do
#ifdef DISABLE_BRICK
      Ttl.eprintf "Cannot use Brick as it's not available in the current setup.\n"
      Ttl.exit $ Ttl.ExitFailure 2
#else
      BkApp.runApp conf syncEnv syncState0
#endif
    Core.Text ->
      TextApp.runApp conf syncEnv syncState0


main :: IO ()
main = runApp
{-# INLINABLE main #-}

foreign export ccall runApp :: IO ()
