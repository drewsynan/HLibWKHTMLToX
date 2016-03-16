module HLibWKHTMLToX.Alt where

import HLibWKHTMLToX.Types
import HLibWKHTMLToX.Utils
import Control.Monad

setGlobalSetting' :: SettingsHandle -> ConverterSetting -> (MaybeT IO) SettingsHandle
setGlobalSetting' gs (ConverterSetting {param = param, val = val}) = do
    status <- liftIO . withCString param $ \p -> do
        withCString val $ \v -> do
            c_set_global_setting gs p v
    MaybeT $ do
        case fromIntegral status of
            1 -> return $ Just gs
            _ -> return $ Nothing

createSettings' :: ImageEnv -> (MaybeT IO) SettingsHandle
createSettings e = do
    gs_ptr <- liftIO $ c_create_global_settings
    MaybeT $ do
        if gs_ptr /= nullPtr
            then return $ Just gs_ptr
            else return $ Nothing 

createConverter' :: ImageEnv -> SettingsHandle -> (MaybeT IO) ConverterHandle
createConverter e gs = do
    conv_ptr <- liftIO $ c_create_converter gs nullPtr
    MaybeT $ do
        if conv_ptr /= nullPtr
            then return $ Just conv_ptr
            else return $ Nothing

destroyConverter' :: ConverterHandle -> (MaybeT IO) Bool
destroyConverter c = do
    status <- liftIO $ c_destroy_converter c -- void return is fucking useless
    MaybeT $ do
        return $ Just True

convert' :: ConverterHandle -> (MaybeT IO) ByteString
convert c = do
    status <- liftIO $ c_convert c

    MaybeT $ do {
                case fromIntegral status of
                    1 -> do
                        results <- liftIO $ doConversion c
                        return $ results
                    _ -> return $ Nothing 
        }
    where
        doConversion :: ConverterHandle -> IO (Maybe ByteString)
        doConversion x = do {
            alloca $ \ptr -> do
                size <- liftIO $ c_get_output x ptr
                modifiedPtr <- liftIO $ peek ptr

                if modifiedPtr /= nullPtr then
                    fmap Just $ packCStringLen (modifiedPtr, fromIntegral size)
                else
                    return $ Nothing -- mzero
        }