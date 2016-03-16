import HLibWKHTMLToX.Examples

main :: IO ()
main = do
    run_hs_example "http://www.amazon.com" "module.jpg"
    return ()