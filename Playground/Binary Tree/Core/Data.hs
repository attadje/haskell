module Core.Data where
    import Core.DataTypes

    binaryTree :: BTree Integer
    binaryTree = Node 9 (L $ Node 3 (L Empty) (R Empty)) (R $ Node 10 (L Empty) (R $ Node 14 (L $ Node 13 (L Empty) (R Empty)) (R Empty)))

    --   9    --
    --  / \   --
    -- 3  10  --
    --     \  --
    --     14 --
    --    /   --
    --  13    --
