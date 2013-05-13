import Prelude hiding (init)
import Control.Applicative
import Control.Monad
import Control.Exception
import Options.Applicative

import Data.List (intercalate)

import System.Cmd (rawSystem)
import System.Directory
import System.Exit

-- the commands here are from https://help.github.com/articles/working-with-subtree-merge

maybeThrow :: ExitCode -> IO ExitCode
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
readTreeUpdate name = git "read-tree"
  ["--prefix=" ++ name ++ "/", "-u", name ++ "/master"]

fromName :: String -> String -> String
fromName base name = base ++ "/" ++ name ++ ".git"

subtreeMerge :: String -> String -> IO ExitCode
subtreeMerge base name = do
  remoteAdd repo name
  mergeNoCommit name
  readTreeUpdate name
  commit $ "Subtree merged in " ++ name
  where repo = base ++ "/" ++ name ++ ".git"

initRepo :: FilePath -> IO ExitCode
initRepo dir = do
  createDirectoryIfMissing False dir
  cd dir
  init
  rawSystem "touch" [".gitignore"]
  git "add" [".gitignore"]
  commit "initial commit"
  setupRepo

setupRepo :: IO ExitCode
setupRepo = do
  branch "upstream-subtrees"

addSubtrees :: String -> [String] -> IO [ExitCode]
addSubtrees base repos = do
  mapM (subtreeMerge base) repos

pull :: [String] -> IO ExitCode
pull args = git "pull" args

pullSubtree :: String -> IO ExitCode
pullSubtree repo = do
  --pull ["-s", "subtree", repo, "master"]
  -- git pull -s recursive -X subtree=src/ external testBranchB
  pull ["-s", "recursive", "-X", "subtree=" ++ repo ++ "/", repo, "master"]

pullSubtrees :: [String] -> IO [ExitCode]
pullSubtrees repos = do
  mapM pullSubtree repos

data Command
  = InitRepository FilePath
  | SetupForSubtrees
  | PullSubtrees [String]
  | AddSubtrees String [String]
  deriving Show

data Options
  = Options {
    optCommand :: Command
  } deriving Show

options :: Parser Options
options = Options <$> commands

commands :: Parser Command
commands = subparser
  ( command "init" (info initOptions
    (progDesc "Init a repo in the provided directory and set up for subtrees"))
 <> command "setup" (info (pure SetupForSubtrees)
    (progDesc "Set up existing repo for subtree merges"))
 <> command "add" (info addOptions
    (progDesc "Add subtrees to remotes and merge them as subdirectories"))
 <> command "pull" (info pullOptions
    (progDesc "Pull subtrees from origins"))
  )

initOptions :: Parser Command
initOptions = InitRepository
  <$> argument str
    ( metavar "DIR"
   <> value "."
   <> help "Directory to create repository in; defaults to current directory")

pullOptions :: Parser Command
pullOptions =  PullSubtrees
  <$> arguments str
    ( metavar "SUBTREE..."
   <> help "List of subtrees to pull into this repo")

addOptions :: Parser Command
addOptions = AddSubtrees
  <$> argument str (metavar "BASE")
  <*> arguments1 str (metavar "NAME...")

run :: Options -> IO ExitCode
run (Options (InitRepository dir)) = runInit dir
run (Options SetupForSubtrees) = runSetup
run (Options (PullSubtrees [])) = putStrLn "pull all subtrees not yet implemented"
  >> (return $ ExitFailure 1)
run (Options (PullSubtrees repos)) = runPullSubtrees repos
run (Options (AddSubtrees base repos)) = runAddSubtrees base repos

runInit :: FilePath -> IO ExitCode
runInit dir = initRepo dir

runSetup :: IO ExitCode
runSetup = setupRepo

runAddSubtrees :: String -> [String] -> IO ExitCode
runAddSubtrees base names =
  checkout "upstream-subtrees" >> addSubtrees base names >>= return . maximum

runPullSubtrees :: [String] -> IO ExitCode
runPullSubtrees names =
  checkout "upstream-subtrees" >> pullSubtrees names >>= return . maximum

main :: IO ()
main = execParser opts >>= run
  -- >> test_main
  >> return ()
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> header "subtrees - tools to manage multiple subtrees in Git repos")

test_main :: IO ExitCode
test_main = let
  base = "https://github.com/oconnor0"
  repos = ["subtree-merges", "resume"] --, "learn-coq", "zero", "scheme-in-haskell"]
  in do
    tmp <- getTemporaryDirectory
    let dir = "/src/subtree-merges" ++ "/test"
    createDirectoryIfMissing False dir
    cd dir
    init
    setupRepo
    addSubtrees base repos
    >>= exitWith . maximum
  --in do
  --  tmp <- getTemporaryDirectory
  --  let dir = tmp ++ "/test3"
  --  cd dir
  --  pullSubtrees repos
  --  >>= exitWith . maximum
