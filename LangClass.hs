{-# LANGUAGE    DeriveGeneric, LambdaCase #-}


import  GHC.Exts                (Down(..))
import  GHC.Generics            (Generic)
import  Numeric                 (showFFloat)

import  qualified Data.Text     as T
import  Data.Text.Encoding      (decodeUtf8)
import  Data.ByteString         as B (readFile)

import  Data.List               (sort, sortBy, group, isSuffixOf)
import  Data.Char               (isSpace, isLetter, isPunctuation)
import  Data.Ord                (comparing)
import  Data.Either             (partitionEithers)
import  Data.Binary

import  Control.Monad.IO.Class
import  Control.Monad           ((>=>), when)
import  Control.Arrow           ((&&&), second)

import  System.Directory
import  System.Console.Haskeline


--
-- Types and static data
--

type BigramFreq = ((Char, Char), Double)


-- the langugage model consists of a name (ISO 639-1 code)
-- and a vector of bigram frequencies
data LanguageModel = LM !String ![BigramFreq]
    deriving Generic

instance Binary LanguageModel

-- custom Show instance for display in the REPL
instance Show LanguageModel where
    show (LM name freqs) = unlines
        [ getExtendedModelName name ++ ", its 30 most common bigrams being:"
        , "  " ++ (unwords . map (deTuple . fst) . take 30) freqs
        ]

      where
        deTuple (a, b) = [a, b]

modelName (LM name _) = name




-- similarity is more significant when the input data is
-- by at least dDistanceThreshold more similar to the best
-- fitting model than to the one next-to-best
dDistanceThreshold          = 0.01 :: Double

absentBigramPenaltyFactor   = 1.5 :: Double

-- special char for intermediate representation of word boundaries
wordBoundary                = '#' :: Char




--
-- Functions for bigram model creation, comparison and evaluation
--


-- leaves only letters, special chars and the word boundary for bigram
-- creation. bigrams containing a word boundary will be disregarded.
prepare :: T.Text -> T.Text
prepare =
    T.intercalate (T.singleton wordBoundary)
    . T.words
    . T.filter (\c -> isSpace c || isLetter c || retain c)
    . T.map punctuationToSpace
  
  where 
    punctuationToSpace c =
        if isPunctuation c && (not . retain) c then ' ' else c
    retain = (`elem` "'")                                               -- special chars to retain, e.g. apostrophe


-- creates a list of bigram frequencies given some input data.
-- only the 150 most frequent bigrams are retained to limit
-- space usage; increasing that number should minimize fallout,
-- and might need readjustment of the value for dDistanceThreshold.
createFrequencies :: T.Text -> [BigramFreq]
createFrequencies t =
    map (second (\c -> fromIntegral c / fromIntegral len))
    . take 150                                                          -- 150 bigrams ~4kB (on-disk) language model 
    . sortBy (comparing (Down . snd))
    . map (head &&& length)
    . group
    . sort
    $ bgs

  where
    bigrams a   = T.zip a (T.tail a)
    len         = length bgs
    bgs         =
        filter (\(x, y) -> x /= wordBoundary && y /= wordBoundary)
        . bigrams
        . prepare
        $ t


-- vector distance of bigram frequencies with respect to a language model
-- is used as a similarity measure of some input data with that model
vectorDistance :: LanguageModel -> [BigramFreq] -> Double
vectorDistance (LM _ freqs) =
    sqrt . sum . map ((^ 2) . distance)
  where
    distance (bg, freq) = maybe 
        (absentBigramPenaltyFactor * freq)                              -- penalty for bigrams absent from the model
        (subtract freq)
        (lookup bg freqs)


-- evaluate bigram frequency similarity relative to some language models
-- to utter a more or less precise guess
makeGuess :: Bool -> [LanguageModel] -> [BigramFreq] -> InputT IO ()
makeGuess showDeltas models freqs
    | length models <= 1 =
        outputStrLn "I cant' really tell, need at least 2 language models"
    | otherwise =
        let
            dFreqs@(a:b:_) = sortBy (comparing fst) 
                [(vectorDistance m freqs, name) | m@(LM name _) <- models]
            bestGuess = getExtendedModelName (snd a)
        in do
            when showDeltas $
                mapM_ (\(dist, name) -> 
                    outputStrLn ("Δ" ++ name ++ " ≈ " ++ showFFloat (Just 6) dist ""))
                    dFreqs
        
            outputStrLn $ if fst b - fst a >= dDistanceThreshold
                then "I'm quite sure this is: " ++ bestGuess
                else "I'd have to guess... " ++ bestGuess




--
-- File handling
--


-- loads all .lmod files in current directory
-- exceptions are caught and wrapped up in the Either type
loadModels :: InputT IO [Either String LanguageModel]
loadModels = liftIO $
    filesByExtension ".lmod" >>= mapM tryLoad
  where
    tryLoad fName = (Right `fmap` decodeFile fName)
        `catch` (\e -> return $
            Left (fName ++ " -- " ++ show (e :: SomeException)))
        

-- loads a text file and creates bigram frequency vector from it.
-- can throw an exception
loadAndCreateFrequencies :: FilePath -> IO [BigramFreq]
loadAndCreateFrequencies = 
    B.readFile >=> return . createFrequencies . decodeUtf8


-- returns a list of files with a given extension in the current directory
filesByExtension :: String -> IO [FilePath]
filesByExtension ext =
    getCurrentDirectory
    >>= (getDirectoryContents >=> return . filter (ext `isSuffixOf`))


-- write a model file for a given language code analyzing some file input
writeModelFile :: FilePath -> String -> InputT IO ()
writeModelFile fp lcode = handle
    (\e -> outputStrLn $ fp ++ " -- " ++ show (e :: SomeException)) $ liftIO $ do
        freqs <- loadAndCreateFrequencies fp
        encodeFile fName (LM lcode freqs)
        putStrLn ("model file created: " ++ fName)    
  where
    fName = lcode ++ ".lmod"
    




--
-- the REPL
--


eval :: [LanguageModel] -> String -> InputT IO ()

eval _ "h" =
    outputStrLn 
        "e <file>  - load a file, evaluate differences to loaded models and make a guess\n\
        \i <text>  - for some input, evaluate differences to loaded models and make a guess\n\
        \t <file>  - load a file, train a model from its data\n\
        \r         - rebuild models from <xy.txt> files, xy being a 2-letter language code\n\
        \s         - show models loaded in memory\n\
        \q         - quit"

eval models "s"
    | null models   = outputStrLn "no models loaded"
    | otherwise     = mapM_ (outputStrLn . show) models

eval _  ('t':' ':fp) = do
    ln <- getInputLine "2-letter language code? "
    let
        lcode = case ln of
            Just c@(a:b:[]) -> c
            _               -> "__"
    writeModelFile fp lcode

eval _ "r" = do
    fps <- filter ((== 6) . length) `fmap` liftIO (filesByExtension ".txt")
    sequence_ [writeModelFile fp (take 2 fp) | fp <- fps]    

eval models ('e':' ':fp) = handle
    (\e -> outputStrLn $ show (e :: SomeException)) $
    liftIO (loadAndCreateFrequencies fp) >>= makeGuess True models

eval models ('i':' ':someInput) =
    makeGuess True models
    . createFrequencies
    . T.pack 
    $ someInput

eval _ _ = outputStrLn "unknown command"



main =
    putStrLn
        "Guess the language !!1!\n\
        \    (c) by M. G. Meier 2013"
    >> runInputT defaultSettings main' >> putStrLn ""
   
main' = do
    (bads, goods) <- partitionEithers `fmap` loadModels
    mapM_ outputStrLn bads
    outputStrLn $ show (length goods) ++ " model(s) loaded"
    
    repl $ sortBy (comparing modelName) goods

  where
    repl models =
        getInputLine "(h for help) > " 
        >>= maybe (outputStrLn "")
            (\case
                "q"         -> return ()
                xs@(x:_)
                    | x `elem` "rt" -> eval models xs >> main'          -- to reload models
                    | otherwise     -> eval models xs >> repl models
                _           -> repl models)




--
-- some more static data; extend from here:
-- http://meta.wikimedia.org/wiki/Template:List_of_language_names_ordered_by_code
--

getExtendedModelName name =
    name ++ maybe "" (" -- " ++) (lookup name iso6391Codes)

iso6391Codes = [
    ("br", "Brezhoneg")
    , ("cy", "Cymraeg")
    , ("de", "Deutsch")
    , ("en", "English")
    , ("es", "Español")
    , ("et", "Eesti keel")
    , ("eu", "Euskara")
    , ("fi", "Suomi")
    , ("fr", "Français")
    , ("ka", "Kartuli ena")
    , ("pt", "Português")
    , ("sv", "Svenska")
    ]
