{-# LANGUAGE ForeignFunctionInterface #-}
module HLibWKHTMLToX.CBindings where

import HLibWKHTMLToX.Types
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_init"                      c_init                     :: CInt -> IO CInt

foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_deinit"                    c_deinit                   :: IO CInt
foreign import ccall safe "wkhtmltox/image.h &wkhtmltoimage_deinit"                   c_deinit_handle            :: FunPtr (IO CInt)

foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_extended_qt"               c_extended_qt              :: IO CInt
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_version"                   c_version                  :: CString

foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_create_global_settings"    c_create_global_settings   :: IO SettingsHandle
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_set_global_setting"        c_set_global_setting       :: SettingsHandle -> CString -> CString -> IO CInt
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_get_global_setting"        c_get_global_setting       :: SettingsHandle -> CString -> CString -> CInt -> IO CInt

-- foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_destroy_global_settings"   c_destroy_global_settings  :: SettingsHandle -> IO ()
-- ^ doesn't exist! sent a throwaway function for now until the finalizer code can be refactored
foreign import ccall safe "stdlib.h &abs"                                             c_destroy_settings_handle :: FunPtr(SettingsHandle -> IO ())

foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_create_converter"          c_create_converter         :: SettingsHandle -> Ptr a -> IO ConverterHandle
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_destroy_converter"         c_destroy_converter        :: ConverterHandle -> IO ()
foreign import ccall safe "wkhtmltox/image.h &wkhtmltoimage_destroy_converter"        c_destroy_converter_handle :: FunPtr (ConverterHandle -> IO ())

foreign import ccall safe "wrapper"                                                   mkStrVoidCallback          :: (ConverterHandle -> CString -> IO ()) -> IO (FunPtr (ConverterHandle -> CString -> IO()))
foreign import ccall safe "wrapper"                                                   mkVoidCallback             :: (ConverterHandle -> IO ()) -> IO (FunPtr (ConverterHandle -> IO ()))
foreign import ccall safe "wrapper"                                                   mkIntVoidCallback          :: (ConverterHandle -> CInt -> IO ()) -> IO (FunPtr (ConverterHandle -> CInt -> IO ()))

foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_set_warning_callback"      c_set_warning_callback     :: ConverterHandle -> FunPtr (ConverterHandle -> CString -> IO ()) -> IO ()
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_set_error_callback"        c_set_error_callback       :: ConverterHandle -> FunPtr (ConverterHandle -> CString -> IO ()) -> IO ()
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_set_phase_changed_callback"    c_set_phase_changed_callback       :: ConverterHandle -> FunPtr (ConverterHandle -> IO ()) -> IO ()
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_set_progress_changed_callback" c_set_progress_changed_callback    :: ConverterHandle -> FunPtr (ConverterHandle -> CInt -> IO ()) -> IO ()
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_set_finished_callback"     c_set_finished_callback    :: ConverterHandle -> FunPtr (ConverterHandle -> CInt -> IO ()) -> IO ()

foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_convert"                   c_convert                  :: ConverterHandle -> IO CInt

foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_current_phase"             c_current_phase            :: ConverterHandle -> IO CInt
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_phase_count"               c_phase_count              :: ConverterHandle -> IO CInt
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_phase_description"         c_phase_description        :: ConverterHandle -> CInt -> IO CString
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_progress_string"           c_progress_string          :: ConverterHandle -> IO CString
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_http_error_code"           c_http_error_code          :: ConverterHandle -> IO CInt
foreign import ccall safe "wkhtmltox/image.h wkhtmltoimage_get_output"                c_get_output               :: ConverterHandle -> Ptr CString -> IO CLong

progress_changed_c :: ConverterHandle -> CInt -> IO ()
progress_changed_c c p = print p

phase_changed_c :: ConverterHandle -> IO ()
phase_changed_c c = do
    p <- c_current_phase c
    print =<< peekCString =<< c_phase_description c p

err_c :: ConverterHandle -> CString -> IO ()
err_c c m = do
    print "ERROR:"
    print =<< peekCString m

--- Destroyers (need to rewrite to use Foreign.Concurrent.newForeignPtr to all callback to haskell) ---
hs_destroy_globalsetting :: SettingsHandle -> IO ()
hs_destroy_globalsetting s = do
    --print "Global Settings Finalizer"
    --print s
    return ()

hs_destroy_converter :: ConverterHandle -> IO ()
hs_destroy_converter c = do
    --print "Converter Finalizer"
    --print c
    c_destroy_converter c

foreign import ccall safe "wrapper" mkSettingsDestroyer     :: (SettingsHandle -> IO ())    -> IO (FunPtr (SettingsHandle -> IO ()))
foreign import ccall safe "wrapper" mkConverterDestroyer    :: (ConverterHandle -> IO ())   -> IO (FunPtr (ConverterHandle -> IO ()))

-- T__________T o god
hs_destroy_settings_handle      = unsafePerformIO   $   mkSettingsDestroyer     hs_destroy_globalsetting
hs_destroy_converter_handle     = unsafePerformIO   $   mkConverterDestroyer    hs_destroy_converter