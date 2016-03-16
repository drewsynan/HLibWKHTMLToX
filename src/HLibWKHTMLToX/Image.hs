module HLibWKHTMLToX.Image where

import HLibWKHTMLToX.Types
import HLibWKHTMLToX.CBindings
import HLibWKHTMLToX.Utils

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans (liftIO)

import Data.ByteString as B
import Data.Maybe

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable

createEnv :: (MaybeT IO) ImageEnv
createEnv = do
    status <- liftIO $ c_init 0
    MaybeT $ do
        case fromIntegral status of
            1 -> return $ Just ImageEnv
            _ -> return $ Nothing

destroyEnv :: ImageEnv -> (MaybeT IO) Bool
destroyEnv ImageEnv = do
    status <- liftIO $ c_deinit
    MaybeT $ do
        case fromIntegral status of
            1 -> return $ Just True
            _ -> return $ Nothing

createSettings :: ImageEnv -> (MaybeT IO) Settings
createSettings e = do
    gs_ptr <- liftIO $ c_create_global_settings
    MaybeT $ do
        if gs_ptr /= nullPtr
            then do
                --gs_foreign_ptr <- liftIO $ newForeignPtr hs_destroy_settings_handle gs_ptr
                gs_foreign_ptr <- liftIO $ newForeignPtr c_destroy_settings_handle gs_ptr
                return $ Just gs_foreign_ptr
            else return $ Nothing

setGlobalSetting :: ImageEnv -> Settings -> ConverterSetting -> (MaybeT IO) Settings
setGlobalSetting env settings (ConverterSetting {param = param, val = val}) = do
    MaybeT $ withForeignPtr settings $ \gs -> do
        status <- liftIO . withCString param $ \p -> do
            withCString val $ \v -> do
                c_set_global_setting gs p v

        case fromIntegral status of
            1 -> return $ Just settings
            _ -> return $ Nothing

createConverter :: ImageEnv -> Settings -> (MaybeT IO) Converter
createConverter e gs = do
    MaybeT $ withForeignPtr gs $ \gs_ptr -> do
        conv_ptr <- liftIO $ c_create_converter gs_ptr nullPtr
        if conv_ptr /= nullPtr
            then do
                conv_foreign_ptr <- liftIO $ newForeignPtr c_destroy_converter_handle conv_ptr
                return $ Just conv_foreign_ptr
            else return $ Nothing

convert :: ImageEnv -> FatConverter -> (MaybeT IO) ByteString
convert env fc = do
    MaybeT $ withForeignPtr (handle fc) $ \c -> do
        status <- doConversion c
        case fromIntegral status of
            1 -> do
                results <- liftIO $ getResults c
                return $ results
            _ -> return $ Nothing 
        where
            doConversion :: ConverterHandle -> IO CInt
            doConversion x = do
                phase_changed_ptr       <- mkVoidCallback phase_changed_c
                progress_changed_ptr    <- mkIntVoidCallback progress_changed_c
                error_ptr               <- mkStrVoidCallback . converter_wrap . string_wrap $ (cb_err fc)
                warning_ptr             <- mkStrVoidCallback . converter_wrap . string_wrap $ (cb_warn fc)

                c_set_phase_changed_callback     x   phase_changed_ptr
                c_set_progress_changed_callback  x   progress_changed_ptr
                c_set_warning_callback           x   warning_ptr
                c_set_error_callback             x   error_ptr

                converter_status <- c_convert x
                
                freeHaskellFunPtr progress_changed_ptr
                freeHaskellFunPtr phase_changed_ptr
                freeHaskellFunPtr error_ptr
                freeHaskellFunPtr warning_ptr

                return converter_status

            getResults :: ConverterHandle -> IO (Maybe ByteString)
            getResults x = do
                 alloca $ \ptr -> do
                    size <- liftIO $ c_get_output x ptr
                    modifiedPtr <- liftIO $ peek ptr

                    if modifiedPtr /= nullPtr then
                        fmap Just $ packCStringLen (modifiedPtr, fromIntegral size)
                    else
                        return $ Nothing