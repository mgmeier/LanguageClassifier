

{-
    Language Classifier
    
    Language Classification based on Bigram Model Similarity
    (see: http://en.wikipedia.org/wiki/Bigram)

    This program lets you train different language models from
    text data and save them as binary '.lmod' files. You can
    then evaluate the similarity of further text input (via file)
    to these models, and let the program guess the input language.

    A simple read-eval-print loop (REPL) is included
    to toy around with.
    
    This program just shows how it's done in Haskell and
    is published for its (hopefully) educational value.

    (c) by M. G. Meier 2013
    
    depends:
        - GHC >= 7.2
        - binary >= 0.6.3
        - text
        - bytestring
        - readline
-}


{-# LANGUAGE    DeriveGeneric #-}


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

import  Control.Monad           ((>=>), when)
import  Control.Arrow           ((&&&), second)
import  Control.Exception

import  System.Directory
import  System.Console.Readline



type BigramFreq = ((Char, Char), Double)


-- the langugage model consists of a name (ISO 639-1 code)
-- and a vector of bigram frequencies
data LanguageModel = LM !String ![BigramFreq]
    deriving Generic

instance Binary LanguageModel

-- custom Show instance for display in the REPL
instance Show LanguageModel where
    show (LM name freqs) = unlines $
        getExtendedModelName name
        : "  16 most common bigrams are:"
        : map (("    " ++). show) (take 16 freqs)

modelName (LM name _) = name




-- vector distance of bigram frequencies with respect to a language model
-- is used as a similarity measure of some input data with that model
vectorDistance :: LanguageModel -> [BigramFreq] -> Double
vectorDistance (LM _ freqs) =
    sqrt . sum . map ((^ 2) . distance)
    where
        distance (bg, freq) =
            maybe freq (subtract freq) (lookup bg freqs)


-- similarity is significant when the input data is
-- by at least dDistanceThreshold more similar to the best
-- fitting model than to the one next-to-best
dDistanceThreshold = 0.01 :: Double


wordBoundary = '#' :: Char


-- leaves only letters and '#' (word boundary) for bigram creation.
-- bigrams containing a word boundary will be disregarded
prepare :: T.Text -> T.Text
prepare =
    T.intercalate (T.singleton wordBoundary)
    . T.words
    . T.filter (\c -> isSpace c || isLetter c)
    . T.map punctuationToSpace
    where 
        punctuationToSpace c = if isPunctuation c then ' ' else c


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


-- loads all .lmod files in current directory
-- exceptions are caught and wrapped up in the Either type
loadModels :: IO [Either String LanguageModel]
loadModels =
    filesByExtension ".lmod"
    >>= mapM tryLoad
    where
        tryLoad fName = (Right `fmap` decodeFile fName)
            `catch` (\e -> return $
                Left (fName ++ " (error) " ++ show (e :: SomeException)))
        

-- loads a text file and creates bigram frequency vector from it
-- can throw an exception
loadAndCreateFrequencies :: FilePath -> IO [BigramFreq]
loadAndCreateFrequencies = 
    B.readFile >=> return . createFrequencies . decodeUtf8


filesByExtension ext =
    getCurrentDirectory
    >>= (getDirectoryContents >=> return . filter (ext `isSuffixOf`))


-- the eval and print part of the REPL
eval :: [LanguageModel] -> String -> IO ()

eval _ "h" =
    putStrLn 
        "e <file>  - load a file, evaluate difference to loaded models and make a guess\n\
        \t <file>  - load a file, train a model from its data\n\
        \s         - show models loaded in memory\n\
        \q         - quit"

eval models "s"
    | null models   = putStrLn "no models loaded"
    | otherwise     = mapM_ print models

eval _  ('t':' ':fp) = handle (\e ->
    print (e :: SomeException)) $ do
    lcode <- maybe "XX" (take 2)
        `fmap` readline "2-letter language code? "
    let fName = lcode ++ ".lmod"

    freqs <- loadAndCreateFrequencies fp 
    encodeFile fName (LM lcode freqs)
    putStrLn ("model file '" ++ fName ++ "' created")

eval models ('e':' ':fp)
    | null models = putStrLn "no models loaded"
    | otherwise = handle (\e -> 
        print (e :: SomeException)) $ do
        freqs <- loadAndCreateFrequencies fp
        let
            dFreqs = sortBy (comparing fst) 
                [(vectorDistance model freqs, name)
                    | model@(LM name _) <- models]

        mapM_ (\(dist, name) -> 
            putStrLn ("Δ" ++ name ++ " ≈ " ++ showFFloat (Just 6) dist ""))
            dFreqs
        
        putStrLn $ case dFreqs of
            a:b:_
                | fst b - fst a >= dDistanceThreshold ->
                    "best guess: " ++ getExtendedModelName (snd a)
                | otherwise -> "no significant similarity to any model"
            a:_ -> "cant' tell if similarity to model " ++ snd a ++ " is significant"

eval _ _ = putStrLn "unknown command"



main = do
    putStrLn
        "Guess the language !!1!\n\
        \    (c) by M. G. Meier 2013"
    lmods <- filesByExtension ".lmod"
    when (null lmods) $ do
        fps <- filesByExtension ".txt"
        sequence_ [rebuildLMod fp | fp <- fps, length fp == 6]
        putStrLn ".lmod files rebuilt"
    main'
    where
        rebuildLMod fp = do
            let lcode = take 2 fp
            freqs <- loadAndCreateFrequencies fp
            encodeFile (lcode ++ ".lmod") (LM lcode freqs)
    
main' = do
    (errs, models) <- partitionEithers `fmap` loadModels
    mapM_ putStrLn errs
    putStrLn (show (length models) ++ " model(s) loaded")
    repl $ sortBy (comparing modelName) models
    where
        repl models =
            readline "(h for help) > " 
            >>= maybe (putStrLn "")
                (\inp -> addHistory inp >> case inp of
                    "q"         -> return ()
                    a@('t':_)   -> eval models a >> main'               -- to reload models
                    a           -> eval models a >> repl models)



-- some static data; extend from here:
-- http://meta.wikimedia.org/wiki/Template:List_of_language_names_ordered_by_code

getExtendedModelName name =
    name ++ maybe "" (" -- " ++) (lookup name iso6391Codes)

iso6391Codes = [
	("aa", "Afar")
	, ("br", "Brezhoneg")
	, ("cy", "Cymraeg")
	, ("de", "Deutsch")
	, ("es", "Español")
	, ("eu", "Euskara")
	, ("fi", "Suomi")
	, ("fr", "Français")
	, ("pt", "Português")
    , ("sv", "Svenska")
    ]
