{-# LANGUAGE OverloadedRecordDot, OverloadedStrings #-}

import Data.Char (isAlpha)
import Data.List (foldl', sort)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)

import FemtoParsec

data NewDir
  = Root
  | Parent
  | Child T.Text 
  deriving Show

data Cmd
  = Cd NewDir
  | Ls [LsResult]
  deriving Show

data LsResult
  = FileEntry Int T.Text
  | DirectoryEntry T.Text
  deriving Show

data Directory = Directory { directSize :: Int
                           , children :: M.Map T.Text Directory
                           } deriving (Show, Eq, Ord)

emptyDirectory :: Directory
emptyDirectory = Directory 0 M.empty

insert :: T.Text -> Directory -> Directory -> Directory
insert name new into = into { children = M.insert name new into.children }

lookupChild :: T.Text -> Directory -> Maybe Directory
lookupChild name = M.lookup name . children

delete :: T.Text -> Directory -> Directory
delete name from = from { children = M.delete name from.children }

data PathZ
  = RootZ
  | ChildZ T.Text DirectoryZ
  deriving Show

data DirectoryZ = DirectoryZ PathZ Directory
  deriving Show

emptyDirectoryZ :: DirectoryZ
emptyDirectoryZ = DirectoryZ RootZ emptyDirectory

toDirectory :: DirectoryZ -> Directory
toDirectory (DirectoryZ _ d) = d

root :: DirectoryZ -> DirectoryZ
root z@(DirectoryZ RootZ _) = z
root c = root $ cdParent c

cd :: T.Text -> DirectoryZ -> DirectoryZ
cd name (DirectoryZ path d) = case lookupChild name d of
  Just d' -> DirectoryZ (ChildZ name (DirectoryZ path (delete name d))) d'
  _ -> DirectoryZ (ChildZ name (DirectoryZ path d)) emptyDirectory

cdParent :: DirectoryZ -> DirectoryZ
cdParent z@(DirectoryZ RootZ _) = z
cdParent (DirectoryZ (ChildZ name (DirectoryZ parentPath parent)) d) =
  DirectoryZ parentPath (insert name d parent)

apply :: DirectoryZ -> Cmd -> DirectoryZ
apply z (Cd Root) = root z
apply z (Cd Parent) = cdParent z
apply z (Cd (Child name)) = cd name z
apply z (Ls results) = foldl' f z results where
  f (DirectoryZ p d) (FileEntry sz _) =
    DirectoryZ p (d { directSize = d.directSize + sz })
  f z' (DirectoryEntry _) = z'

directoriesWithTotalSizes :: Directory -> [(Int, Directory)]
directoriesWithTotalSizes d@(Directory sz cs) =
  let kids = directoriesWithTotalSizes <$> M.elems cs
   in (sz + sum (fst . head <$> kids), d):concat kids

cmd :: Parser Cmd
cmd =  Cd <$> (lexeme "$" *> lexeme "cd" *> newDir)
   <|> Ls <$> (lexeme "$" *> lexeme "ls" *> some (lexeme lsResult))
 where
    newDir =  Root <$ "/"
          <|> Parent <$ ".."
          <|> Child <$> chars1 isAlpha
    lsResult =  FileEntry <$> lexeme unsignedIntNum
                          <*> chars1 (\c -> isAlpha c || c == '.')
            <|> DirectoryEntry <$> (lexeme "dir" *> chars1 isAlpha)

dumpTree :: Directory -> IO ()
dumpTree = go 0 "/" where
  go indent name d = do
    putStrLn $ replicate indent ' ' <> T.unpack name <> ": " <> show d.directSize
    mapM_ (\(n, c) -> go (indent + 2) n c) $ M.toList d.children

main :: IO ()
main = do
  transcript <- parseStdin (some $ lexeme' cmd)
  case transcript of
    Nothing -> do
      hPutStrLn stderr "bad input"
    Just transcript' -> do
      let slash = toDirectory $ root $ foldl' apply emptyDirectoryZ transcript'
          sizes = directoriesWithTotalSizes slash
          (totalUsed, _) = head sizes
          gap = 30000000 - (70000000 - totalUsed)
          sizes' = sort sizes
      -- dumpTree slash
      print $ sum $ takeWhile (<= 100000) $ fst <$> sizes'
      print $ head $ dropWhile (< gap) $ fst <$> sizes'
