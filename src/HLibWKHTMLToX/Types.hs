{-# LANGUAGE EmptyDataDecls #-}
module HLibWKHTMLToX.Types where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad
import Control.Monad.Trans.Maybe

type MaybeIO a = MaybeT IO a

data C_ENV
type ImageEnvironmentHandle = Ptr C_ENV
type ImageEnvironment = ForeignPtr C_ENV


data CallbackType = WarningCallback | ErrorCallback | FinishedCallback | PhaseChangedCallback | ProgressChangedCallback

data ConverterSetting = ConverterSetting    { param :: String
                                            , val   :: String
                                            } deriving (Show)

data WH2I_GlobalSettings
type SettingsHandle = Ptr WH2I_GlobalSettings
type Settings = ForeignPtr WH2I_GlobalSettings

data WH2I_Converter
type ConverterHandle = Ptr WH2I_Converter
type Converter = ForeignPtr WH2I_Converter

data FatConverter = FatConverter { settings :: Settings
                                 , handle   :: ForeignPtr WH2I_Converter
                                 , cb_err   :: (String -> IO ())
                                 , cb_warn  :: (String -> IO ())
                                 , cb_phase :: (String -> IO ())
                                 , cb_prog  :: (Int -> IO ())}