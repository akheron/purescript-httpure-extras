module HTTPure.Contrib.Static (staticDir, staticFile) where

import Prelude

import Data.Array as Array
import Data.MediaType (MediaType(MediaType))
import Data.MediaType.Common as MediaType
import Data.String as String
import Effect.Aff (Aff, catchError)
import HTTPure as HTTPure
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path

staticDir :: FilePath -> HTTPure.Request -> Aff HTTPure.Response
staticDir dirPath { path, method } =
  if method /= HTTPure.Get
  then HTTPure.methodNotAllowed
  else if Array.any (_ == "..") path
  then HTTPure.badRequest "Filesystem traversal not allowed"
  else serveFile (Path.concat $ [dirPath] <> path)

staticFile :: FilePath -> HTTPure.Request -> Aff HTTPure.Response
staticFile filePath { path, method } =
  handle path method
  where
    handle [] HTTPure.Get = serveFile filePath
    handle _ HTTPure.Get = HTTPure.notFound
    handle _ _ = HTTPure.methodNotAllowed

serveFile :: FilePath -> Aff HTTPure.Response
serveFile path =
  catchError read (const HTTPure.notFound)
  where
    bufSize = 4 * 1024 * 1024
    read = do
      contents <- FS.readFile path
      HTTPure.ok' (contentType path) contents

contentType :: FilePath -> HTTPure.Headers
contentType path =
  HTTPure.header "Content-Type" mediaType
  where
    (MediaType mediaType) = extMediaType (Path.extname path)

extMediaType :: FilePath -> MediaType
extMediaType ext =
  case String.toLower ext of
    ".json" -> MediaType.applicationJSON
    ".js" -> MediaType.applicationJavascript
    ".gif" -> MediaType.imageGIF
    ".jpeg" -> MediaType.imageJPEG
    ".jpg" -> MediaType.imageJPEG
    ".png" -> MediaType.imagePNG
    ".csv" -> MediaType.textCSV
    ".html" -> MediaType.textHTML
    ".htm" -> MediaType.textHTML
    ".txt" -> MediaType.textPlain
    ".xml" -> MediaType.textXML
    _ -> MediaType.applicationOctetStream
