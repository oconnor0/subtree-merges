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

git :: [String] -> IO ExitCode
git args = rawSystem "git" args >>= maybeThrow

init :: IO ExitCode
init = git ["init"]

branch :: String -> IO ExitCode
branch name = git ["branch", name]

checkout :: String -> IO ExitCode
checkout rev = git ["checkout", rev]

commit :: String -> IO ExitCode
commit msg = git ["commit", "-m", msg]

remoteAdd :: String -> String -> IO ExitCode
remoteAdd repo name = git ["remote", "add", "-f", name, repo]

mergeNoCommit :: String -> IO ExitCode
mergeNoCommit name = git ["merge", "-s", "ours", "--no-commit", name ++ "/master"]

readTreeUpdate :: String -> IO ExitCode
readTreeUpdate name = git ["read-tree", "--prefix=" ++ name ++ "/", "-u", name ++ "/master"]

fromName :: String -> String -> String
fromName base name = base ++ "/" ++ name ++ ".git"

subtreeMerge :: String -> String -> IO ExitCode
subtreeMerge base name = do
  remoteAdd repo name
  mergeNoCommit name
  readTreeUpdate name
  commit $ "Subtree merged in " ++ name
  where
    repo = base ++ "/" ++ name ++ ".git"

setupRepo :: IO ExitCode
setupRepo = do
  rawSystem "touch" [".gitignore"]
  git ["add", ".gitignore"]
  commit "initial commit"

main :: IO ExitCode
main = let
  base = "https://github.com/oconnor0"
  repos = ["subtree-merges", "clever-algorithms", "resume", "learn-coq", "zero", "scheme-in-haskell"]
  in do
    temp <- getTemporaryDirectory
    let dest = temp ++ "/test"
    createDirectoryIfMissing False dest
    cd dest
    init
    setupRepo
    branch "upstream-subtrees"
    checkout "upstream-subtrees"
    mapM (subtreeMerge base) repos
    >>= exitWith . maximum
