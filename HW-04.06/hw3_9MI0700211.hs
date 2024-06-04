import Data.List (isPrefixOf, sortOn)

main :: IO ()
main = do
    let fs = generateFileSystem commands
    print fs
    print $ findDirectoriesContainingFile fs "i"
    print $ getParentSize fs "i"
    print $ getParentSize fs "g"
    print $ getParentSize fs "b.txt"
    print $ getParentSize fs "abc"
    

-- Task 1

type Command = String
type Size = Int
type Name = String

commands :: [Command]
commands = ["$ cd /", "$ ls", "dir a", "14848514 b.txt", "8504156 c.dat", "dir d", "$ cd a", "$ ls", "dir e", "29116 f", "2557 g", "62596 h.lst", "$ cd e", "$ ls", "584 i", "$ cd ..", "$ cd ..", "$ cd d", "$ ls", "4060174 j", "8033020 d.log", "5626152 d.ext", "7214296 k"]

data FileSystem = Directory Name [FileSystem] | File Name Size
  deriving (Eq, Show)

mapFSwithPaths' :: [Command] -> [String] -> [(Command, [String])]
mapFSwithPaths' [] _ = []
mapFSwithPaths' (command : commands) directories
  | "$ cd .." `isPrefixOf` command = (command, directories) : mapFSwithPaths' commands (tail directories)
  | "$ cd" `isPrefixOf` command = (command, directories) : mapFSwithPaths' commands ((words command !! 2) : directories)
  | otherwise = (command, directories) : mapFSwithPaths' commands directories

mapFSwithPaths :: [Command] -> [(Command, [String])]
mapFSwithPaths commands = filter (\(cmd, paths) -> not $ isPrefixOf "$ " cmd) $ mapFSwithPaths' commands []

sortMapped :: [(Command, [String])] -> [(Command, [String])]
sortMapped xs = sortOn (\(command, path) -> words command !! 1) (filter (\(command, path) -> head (words command) == "dir") xs) ++ sortOn (\(command, path) -> words command !! 1) (filter (\(command, path) -> head (words command) /= "dir") xs)

generateFileSystem :: [Command] -> FileSystem
generateFileSystem cmds = Directory "/" (generateFileSystem' cmds)

generateFileSystem' :: [Command] -> [FileSystem]
generateFileSystem' cmds = helper (sortMapped $ mapFSwithPaths cmds)
  where
    helper :: [(Command, [String])] -> [FileSystem]
    helper [] = []
    helper ((command, path) : fs)
      | head cs == "dir" = Directory (cs !! 1) (helper (filter (\(cNext, pNext) -> containedInOrder ((cs !! 1) : path) pNext) fs)) : helper (filter (\(cNext, pNext) -> not $ containedInOrder ((cs !! 1) : path) pNext) fs)
      | otherwise = File (cs !! 1) (read $ head cs) : helper fs
      where
        cs = words command

containedInOrder :: (Eq a) => [a] -> [a] -> Bool
containedInOrder xs ys
  | length xs > length ys = False
  | otherwise = xs == drop (length ys - length xs) ys


-- Task 2

calculateSize :: FileSystem -> Size
calculateSize (File _ size) = size
calculateSize (Directory _ contents) = sum (map calculateSize contents)

findDirectoriesContainingFile :: FileSystem -> Name -> [FileSystem]
findDirectoriesContainingFile (File _ _) _ = []
findDirectoriesContainingFile dir@(Directory _ contents) targetName =
    if any (fileExists targetName) contents
    then dir : concatMap (`findDirectoriesContainingFile` targetName) directories
    else concatMap (`findDirectoriesContainingFile` targetName) directories
  where
    directories = [d | d@(Directory _ _) <- contents]

    fileExists :: Name -> FileSystem -> Bool
    fileExists name (File fname _) = name == fname
    fileExists _ (Directory _ _) = False


getParentSize :: FileSystem -> Name -> Size
getParentSize fs fileName =
    let dirs = findDirectoriesContainingFile fs fileName
        dirSizes = map calculateSize dirs
    in if null dirSizes then -1 else minimum dirSizes
