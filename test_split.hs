import qualified Data.ByteString as BS
import Data.Word (Word8)

main :: IO ()
main = do
  let testData = BS.pack [65, 13, 10, 66, 13, 10, 67]  -- "A\r\nB\r\nC"
      pieces = BS.split 10 testData  -- Split on LF (byte 10)
  putStrLn $ "Test data: A<CR><LF>B<CR><LF>C"
  putStrLn $ "Split on LF (byte 10):"
  mapM_ (\(i, p) -> putStrLn $ "  Piece " ++ show i ++ ": " ++ show (BS.unpack p)) (zip [1..] pieces)
