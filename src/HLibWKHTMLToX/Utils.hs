module HLibWKHTMLToX.Utils where

import HLibWKHTMLToX.Types
import Foreign.C.String

converter_wrap :: (a -> b) -> (ConverterHandle -> a -> b)
converter_wrap f = \c -> f

string_wrap :: (String -> IO b) -> CString -> IO b
string_wrap f = \s -> do
    hs_string <- peekCString s
    f hs_string

prog :: Int -> IO ()
prog p = do
    print p

phase :: String -> IO ()
phase p = do
    print "Phase (hs):"
    print p

err :: String -> IO ()
err msg = do
    print "Error (hs):"
    print msg

warning :: String -> IO ()
warning msg = do
    print "Warning (hs):"
    print msg

warning_composed :: ConverterHandle -> CString -> IO ()
warning_composed c = converter_wrap (string_wrap warning) c

error_composed :: ConverterHandle -> CString -> IO ()
error_composed c = converter_wrap (string_wrap err) c