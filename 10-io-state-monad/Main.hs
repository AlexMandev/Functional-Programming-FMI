import System.Environment (getArgs)

-- 1. Да се напише програма, която приема два имена на файлове - съответно за четене и за писане, криптира съдържанието на първия файл, 
-- като обръща реда на буквите във всяка от думите, и го записва във втория файл.

encrypt' :: String -> String
encrypt' str = unwords $ map reverse (words str)

encrypt :: FilePath -> FilePath -> IO ()
encrypt inFile outFile = do
    contents <- readFile inFile
    
    let encrypted = encrypt' contents

    writeFile outFile encrypted

main1 :: IO ()
main1 = do 
    args <- getArgs
    if length args /= 2
        then putStrLn "Invalid number of arguments"
        else encrypt (head args) (args !! 1)


-- 2. Да се напише програма, която приема име на файл като аргумент на командния ред и симулира UNIX командата wc върху файла 
-- (връща броя на думите, редовете и байтовете във файла).

wc' :: String -> IO (Int, Int, Int)
wc' contents = do
    let wcnt = length $ words contents
    let lcnt = length $ lines contents
    let bcnt = length contents

    pure (lcnt, wcnt, bcnt)

wc :: FilePath -> IO ()
wc file = do
    contents <- readFile file
    (lc, wc, bc) <- wc' contents

    putStrLn $ show lc ++ " " ++ show wc ++ " " ++ show bc ++ " " ++ file 

main2 :: IO ()
main2 = do
    args <- getArgs

    if length args /= 1
        then putStrLn "Wrong args bro"
        else wc $ head args

-- going from monads to the 'do' syntax is weird for me :D
-- so i wanted to test out stuff like this lmao
promptName :: IO ()
promptName = putStrLn "What's your name? " >> getLine
    >>= \line -> putStrLn $ "Hello, " ++ line ++ "!"


--3. Използвайки следния алгебричен тип данни за състояние:

newtype State s a = State {runState :: s -> (a, s)}

-- създайте инстанции на типовите класове Functor, Applicative и Monad.

-- Дефинирайте следните помощни функции за монадата за състояние:
    -- get :: State s s - връща състоянието в монадата;
    -- put :: s -> State s () - заменя състоянието в монадата с друго такова;
    -- modify :: (s -> s) -> State s () - променя състоянието в монадата;
    -- evalState :: State s a -> s -> a.

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State state1) = State $ \s -> let (x, s') = state1 s 
                                        in (f x, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a,s) 
    
    (<*>) :: State s (a -> b) -> State s a -> State s b
    State state1 <*> State state2 = State $ \s -> let (f, s') = state1 s
                                                      (x, s'') = state2 s'
                                                  in (f x, s'')

    liftA2 :: (a -> b -> c) -> State s a -> State s b -> State s c
    liftA2 f (State state1) (State state2) =
        State $ \s -> let (x, s') = state1 s
                          (y, s'') = state2 s'
                    in (f x y, s'')

instance Monad (State s) where
    return :: a -> State s a
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b 
    State state1 >>= f = State $ \s -> let (x, s') = state1 s
                                    in runState (f x) s 

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put newState = State $ \s -> ((), newState)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

evalState :: State s a -> s -> a
evalState st s = fst (runState st s)

-- 4. Нека е даден следният АТД за двоично наредено дърво:

data BST a = Empty | Node a (BST a) (BST a)
  deriving (Show, Eq, Ord)

-- Използвайки монадата за състояние от задача 03, дефинирайте функция, която намира k-тия най-голям елемент в двоично наредено дърво.

findKthSmallest :: Int -> BST a -> Maybe a
findKthSmallest k bst = if k <= 0 
    then Nothing
    else evalState (helper bst) k
        where
            helper :: BST a -> State Int (Maybe a) 
            helper Empty = pure Nothing
            helper (Node x left right) = do
                leftRes <- helper left

                case leftRes of
                    Just x -> pure $ Just x
                    Nothing -> do
                        modify (subtract 1)
                        cnt <- get

                        if cnt == 0
                            then pure $ Just x
                            else pure Nothing
                        


