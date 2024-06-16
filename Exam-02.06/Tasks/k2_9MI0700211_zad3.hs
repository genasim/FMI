import Data.List (isSuffixOf)

main :: IO ()
main = do
    let result = storageMaintenance t1 1 ".yaml"
    print result

data NTree = Nil | Node String [NTree]
  deriving (Show)

hasExtension :: String -> String -> Bool
hasExtension ext fname = ext `isSuffixOf` fname

storageMaintenance :: NTree -> Int -> String -> NTree
storageMaintenance Nil _ _ = Nil
storageMaintenance (Node name children) 0 ext = Node name (filter (not . isRemovable) children)
  where
    isRemovable (Node fname childs) = null childs && hasExtension ext fname
storageMaintenance (Node name children) depth ext = Node name (map (\child -> storageMaintenance child (depth - 1) ext) children)

t1 :: NTree
t1 = Node "stef" 
    [ Node "a" [Node "b.txt" [], Node ".yaml" [ Node "a.txt" [] ], Node "z.txt" []],
      Node "c" [Node "a.yaml" [], Node "a" [Node "d.yaml" []], Node "b.yaml" []],
      Node "z" [Node ".y.yml" []],
      Node "b" [Node "k.yaml" [], Node "e.yaml" [], Node "z.yaml" []],
      Node "root.yaml" []
    ]



