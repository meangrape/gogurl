import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad.Managed
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import qualified Test.HUnit as HUnit

main :: IO ()
main = runManaged $ do
  tempdir <- managed (withSystemTempDirectory "gogurl")
  background ("gogurl -p 16789 -d " ++ tempdir ++ "/db")
  liftIO (threadDelay 1000000)

  -- Put: foo -> bar
  bash "curl -s localhost:16789/links -d '{\"name\":\"foo\",\"url\":\"bar\"}'"

  (@?= "Location: bar\r\n")
    =<< bash "curl -i -s localhost:16789/foo | grep Location"

  -- Update: foo -> qux
  bash "curl -s localhost:16789/links/foo/edit/qux"

  (@?= "Location: qux\r\n")
    =<< bash "curl -i -s localhost:16789/foo | grep Location"

  -- Delete: foo -> qux
  (@?= "{\"result\":\"success\",\"name\":\"foo\"}")
    =<< bash "curl -s localhost:16789/links/foo/delete"

background :: String -> Managed ()
background s =
  managed (\k -> bracket (spawnCommand s) terminateProcess (\_ -> k ()))

bash :: MonadIO m => String -> m String
bash s = liftIO (readProcess "bash" ["-c", s] "")

(@?=) x y = liftIO (x HUnit.@?= y)
