import Prelude hiding (init)
import Control.Applicative
import Control.Monad
import Control.Exception
import Options.Applicative

import System.Cmd (rawSystem)
import System.Directory
import System.Exit

maybeThrow e@(ExitFailure _) = throwIO e
maybeThrow ExitSuccess = return ExitSuccess

cd :: FilePath -> IO ()
cd dir = setCurrentDirectory dir

git :: String -> [String] -> IO ExitCode
git cmd args = (rawSystem "git" $ cmd:args) >>= maybeThrow

init :: IO ExitCode
init = git "init" []

branch :: String -> IO ExitCode
branch name = git "branch" [name]

checkout :: String -> IO ExitCode
checkout rev = git "checkout" [rev]

commit :: String -> IO ExitCode
commit msg = git "commit" ["-m", msg]

remoteAdd :: String -> String -> IO ExitCode
remoteAdd repo name = git "remote" ["add", "-f", name, repo]

mergeNoCommit :: String -> IO ExitCode
mergeNoCommit name = git "merge" ["-s", "ours", "--no-commit", name ++ "/master"]

readTreeUpdate :: String -> IO ExitCode
readTreeUpdate name = git "read-tree" ["--prefix=" ++ name ++ "/", "-u", name ++ "/master"]

fromName :: String -> String -> String
fromName base name = base ++ "/" ++ name ++ ".git"

subtreeMerge :: String -> String -> IO ExitCode
subtreeMerge base name = do
  remoteAdd repo name
  mergeNoCommit name
  readTreeUpdate name
  commit $ "Subtree merged in " ++ name
  where repo = base ++ "/" ++ name ++ ".git"

initRepo :: String -> IO ExitCode
initRepo dir = do
  createDirectoryIfMissing False dir
  cd dir
  init
  setupRepo

setupRepo :: IO ExitCode
setupRepo = do
  rawSystem "touch" [".gitignore"]
  git "add" [".gitignore"]
  commit "initial commit"

initSubtrees :: String -> [String] -> IO [ExitCode]
initSubtrees base repos = do
  branch "upstream-subtrees"
  checkout "upstream-subtrees"
  mapM (subtreeMerge base) repos

pull :: [String] -> IO ExitCode
pull args = git "pull" args

pullSubtree :: String -> IO ExitCode
pullSubtree repo = do
  pull ["-s", "subtree", repo, "master"]

pullSubtrees :: [String] -> IO [ExitCode]
pullSubtrees repos = do
  mapM pullSubtree repos

main :: IO ExitCode
main = let
  base = "https://github.com/oconnor0"
  repos = ["subtree-merges", "resume"] --, "learn-coq", "zero", "scheme-in-haskell"]
  --in do
  --  tmp <- getTemporaryDirectory
  --  let dir = tmp ++ "/test4"
  --  initRepo dir
  --  initSubtrees base repos
  --  >>= exitWith . maximum
  in do
    tmp <- getTemporaryDirectory
    let dir = tmp ++ "/test3"
    cd dir
    pullSubtrees repos
    >>= exitWith . maximum
