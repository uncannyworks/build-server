module Main where

import Data.Monoid ((<>))
import System.Environment

import Development.Bake
import Development.Shake.Command

putBlock :: String -> [String] -> String
putBlock title body = unlines $
  let s = "-- " ++ title ++ " --" in
  (s ++ replicate (70 - length s) '-') :
  body ++
  [replicate 70 '-']

data Test = Test String String deriving (Show, Read)

data Action
  = RunTests Test
  | BuildDocker Test String String String
    deriving (Show, Read)

instance Stringy Action where
  stringyTo   = show
  stringyFrom = read

runTests :: Action -> IO ()
runTests (RunTests (Test td tc)) = cmd (Cwd td) tc
runTests _ = return ()

execute :: Action -> TestInfo Action
execute rt@(RunTests _) =
  run $ do
    incrementalStart
    runTests rt
    incrementalDone

execute (BuildDocker ts dn dr dg) =
  depend [RunTests ts] $ run $ do
    Stdout l <- cmd "aws ecr get-login --region " [dg]
    unit $ cmd (l :: String) -- run returned login command
    Stdout t <- cmd "git rev-parse HEAD"
    unit $ cmd bld
    unit $ cmd (tag t)
    unit $ cmd (psh t)
    where
      p t = dr <> "/" <> dn <> ":" <> t
      bld = "docker build -t " <> dn <> " ."
      tag t = "docker tag " <> dn <> " " <> p t
      psh t = "docker push " <> p t

ovenNotifyChatWork :: String -> String -> Oven s p t -> Oven s p t
ovenNotifyChatWork t r =
  ovenNotifyAdd $ \a s b ->
    let m = putBlock "ChatWork" [ "Author: " <> a, "Result: " <> s, b]
        cwt = [ "X-ChatWorkToken: " <> t ]
        bdy = [ "body=" <> m ]
        lnk = ("https://api.chatwork.com/v1/rooms/" <> r <> "/messages")
    in unit $ cmd "curl -s -X POST -H" cwt "-d" bdy lnk

main :: IO ()
main = do
  ct <- getEnv "CHATWORK_TOKEN"
  cr <- getEnv "CHATWORK_ROOM"
  gr <- getEnv "GIT_REPO"
  gb <- getEnv "GIT_BRANCH"
  td <- getEnv "TEST_DIR"
  tc <- getEnv "TEST_CMD"
  dr <- getEnv "DOCKER_REPO"
  dn <- getEnv "DOCKER_NAME"
  dg <- getEnv "DOCKER_REGION"
  bake $
    ovenNotifyChatWork ct cr $
    ovenPretty $
    ovenIncremental . ovenGit gr gb Nothing $
    ovenNotifyStdout $
      let ts = Test td tc
          rt = RunTests ts
          bd = BuildDocker ts dn dr dg
          od = defaultOven { ovenServer = ("127.0.0.1", 5000) }
      in ovenTest (return [ rt, bd ]) execute od
