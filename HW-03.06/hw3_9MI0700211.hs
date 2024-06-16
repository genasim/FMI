import Data.Char (toLower)
import Data.List (isPrefixOf, partition, sortBy)

type Command = String

type Size = Int

type Name = String

data FileSystem = Directory Name [FileSystem] | File Name Size
  deriving (Eq, Show)

-- Примерен списък с команди
commands :: [Command]
commands =
  [ "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"
  ]

-- Функция за парсиране на командите и резултатите
parseCommand :: Command -> (String, [String])
parseCommand cmd
  | "$ cd " `isPrefixOf` cmd = ("cd", [drop 5 cmd])
  | cmd == "$ ls" = ("ls", [])
  | "dir " `isPrefixOf` cmd = ("dir", [drop 4 cmd])
  | otherwise =
      let (size, name) = span (/= ' ') cmd
       in ("file", [size, drop 1 name])

-- Функция за сортиране на съдържанието на директорията
sortDirContent :: [FileSystem] -> [FileSystem]
sortDirContent content =
  let (dirs, files) = partition isDirectory content
   in sortBy compareFS dirs ++ sortBy compareFS files
  where
    isDirectory (Directory _ _) = True
    isDirectory _ = False
    compareFS (Directory name1 _) (Directory name2 _) = compare (map toLower name1) (map toLower name2)
    compareFS (File name1 _) (File name2 _) = compare (map toLower name1) (map toLower name2)
    compareFS (Directory _ _) (File _ _) = LT
    compareFS (File _ _) (Directory _ _) = GT

-- Функция за изграждане на файловата система
generateFileSystem :: [Command] -> FileSystem
generateFileSystem cmds = fst $ build cmds [Directory "/" []] []
  where
    build :: [Command] -> [FileSystem] -> [FileSystem] -> (FileSystem, [Command])
    build [] fs _ = (head fs, [])
    build (cmd : cmds) fs stack
      | cmd == "$ cd .." = build cmds (head stack : fs) (tail stack)
      | cmd == "$ ls" = let (newFS, restCmds) = processLS cmds (head fs) in build restCmds (newFS : tail fs) stack
      | otherwise =
          let (cmd, args) = parseCommand cmd
           in case cmd of
                "cd" ->
                  let targetDir = head args
                      (newFS, restStack) = navigateToDir fs targetDir
                   in build cmds (newFS : restStack) (head fs : stack)
                "dir" -> build cmds (addToCurrentDir (Directory (head args) []) fs) stack
                "file" -> build cmds (addToCurrentDir (File (args !! 1) (read $ head args)) fs) stack

    processLS :: [Command] -> FileSystem -> (FileSystem, [Command])
    processLS [] fs = (fs, [])
    processLS (cmd : cmds) (Directory name content)
      | "$ " `isPrefixOf` cmd = (Directory name (sortDirContent content), cmd : cmds)
      | otherwise =
          let (cmdType, args) = parseCommand cmd
           in case cmdType of
                "dir" -> processLS cmds (Directory name (Directory (head args) [] : content))
                "file" -> processLS cmds (Directory name (File (args !! 1) (read $ head args) : content))

    addToCurrentDir :: FileSystem -> [FileSystem] -> [FileSystem]
    addToCurrentDir item (Directory name content : rest) = Directory name (item : content) : rest

    navigateToDir :: [FileSystem] -> Name -> (FileSystem, [FileSystem])
    navigateToDir [] target = error $ "Directory " ++ target ++ " not found"
    navigateToDir (Directory name content : rest) target
      | map toLower name == map toLower target = (Directory name content, rest)
      | otherwise =
          let (found, remaining) = navigateToDir content target
           in (found, Directory name content : rest)

-- Тестова функция
main :: IO ()
main = do
--   print $ parseCommand "$ cd /"
--   print $ parseCommand "$ ls"
--   print $ parseCommand "dir a"
--   print $ parseCommand "14848514 b.txt"
--   print $ parseCommand "8504156 c.dat"
--   print $ parseCommand "dir d"
    print $ generateFileSystem commands
