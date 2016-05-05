{-# LANGUAGE ForeignFunctionInterface #-}
module HLibWKHTMLToX.Examples where

import HLibWKHTMLToX.Image
import HLibWKHTMLToX.Types
import HLibWKHTMLToX.CBindings
import HLibWKHTMLToX.Utils

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans (liftIO)

import Data.ByteString as B
import Data.Maybe

import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable



c_example :: String -> String -> IO ()
c_example url output = do

    phase_changed_ptr       <- mkVoidCallback       phase_changed_c
    progress_changed_ptr    <- mkIntVoidCallback    progress_changed_c
    error_ptr               <- mkStrVoidCallback    error_composed
    warning_ptr             <- mkStrVoidCallback    warning_composed

    gs <- c_create_global_settings

    withCString "in" $ \param -> do
        withCString url $ \val -> do
            c_set_global_setting gs param val

    withCString "fmt" $ \param -> do
        withCString "jpeg" $ \val -> do
            c_set_global_setting gs param val

    
    c <- c_create_converter gs nullPtr

    c_set_phase_changed_callback c phase_changed_ptr
    c_set_progress_changed_callback c progress_changed_ptr
    c_set_warning_callback c warning_ptr
    c_set_error_callback c error_ptr

    conv_status <- c_convert c


    results <- alloca $ \ptr -> do
        size <- c_get_output c ptr
        modifiedPtr <- peek ptr
        packCStringLen (modifiedPtr, fromIntegral size)


    B.writeFile output results

    -- Bai to all those function pointers --
    freeHaskellFunPtr phase_changed_ptr
    freeHaskellFunPtr progress_changed_ptr
    freeHaskellFunPtr warning_ptr
    freeHaskellFunPtr error_ptr

    -- Bai, converter --
    dest_conv <- c_destroy_converter c

    return ()

hs_example :: ImageEnvironment -> String -> String -> (MaybeT IO) ()
hs_example env url output = do

    -- Uses ForeignPtr for automatic finilization

    let config = [  ConverterSetting "in" url,
                    ConverterSetting "fmt" "jpeg"  ]

    empty_settings <- createSettings env
    settings <- foldM (setGlobalSetting env) empty_settings config

    converter <- createConverter env settings

    let fc = FatConverter { handle = converter
                          , settings = settings
                          , cb_err = err
                          , cb_warn = warning
                          , cb_phase = phase
                          , cb_prog = prog }

    results <- convert env fc
    writeStatus <- liftIO $ B.writeFile output results

    MaybeT $ return (Just writeStatus)

run_c_example :: String -> String -> IO ()
run_c_example url output = do
    startup_code <- c_init 0
    c_example url output
    exit_code <- c_deinit
    return ()

run_hs_example :: String -> String -> IO ()
run_hs_example url output = do
    e <- runMaybeT $ createEnv
    case e of 
        Just env -> do
            runMaybeT $ hs_example env url output
            --runMaybeT $ destroyEnv env
            --since we're using foreignPtr now, the finalizer will automagically run
            return ()
        Nothing -> do
            liftIO $ print "Error creating imaging environment :-("
