{-# LANGUAGE CPP                #-}

module Main where

import qualified Process

import qualified Tiff
import qualified Png
import qualified Jpeg
import qualified Bmp
--import qualified Gif

#ifdef COMPLETE
import qualified Zip
import qualified Tga
import qualified Ogg
import qualified Tar
import qualified Xml
import qualified Html
import qualified Pnm
import qualified Gzip
import qualified Bzip
import qualified Js
--import qualified Sh
--import qualified SimpleSvg
import qualified Svg
import qualified ID3
import qualified Pandoc

--import qualified MBox
import qualified Css
import qualified Dot
import qualified ByteString
import qualified Unicode
import qualified TTF
import qualified Wav
import qualified CPIO
import qualified MarkUp
import qualified Regex

--import qualified MarkUpSvg

import qualified URI

--import qualified Http
--import qualified Tftp
--import qualified Dns

#endif

import System.Console.ArgParser
import System.Random
import Args
import Data.Maybe
import System.Directory 
import System.Exit
import Control.Monad
import Data.List.Split

fillArgs :: MainArgs -> IO (String -> MainArgs)
fillArgs args =
    case findFileName args of
        [] -> do
            sG <- getStdGen
            let fname = take 10 ((randomRs ('a','z') sG) :: String )
            return $ formatArgs (formatFileName args fname)
        _ -> return $ formatArgs args

dispatch :: MainArgs -> IO ()
dispatch arg = do
        args <- fillArgs arg
        let b = findPar arg
        safetyChecks arg 
        case findFileType arg of

            "Bmp"  -> Process.main Bmp.mencode args b
            --"Gif"  -> Process.main Gif.mencode args b
            "Jpeg" -> Process.main Jpeg.mencode args b
            "Png"  -> Process.main Png.mencode args b
            "Tiff" -> Process.main Tiff.mencode args b

#ifdef COMPLETE
            "Dot"  -> Process.main Dot.mencode args b
            "Ogg"  -> Process.main Ogg.mencode args b
            "Zip"  -> Process.main Zip.mencode args b
            "Bzip" -> Process.main Bzip.mencode args b
            "Gzip" -> Process.main Gzip.mencode args b
            "Tar"  -> Process.main Tar.mencode args b
            "Tga"  -> Process.main Tga.mencode args b
            "Xml"  -> Process.main Xml.mencode args b
            "Html" -> Process.main Html.mencode args b

            --"Html" -> Process.main MarkUp.mencodeHtml args b
            --"XHtml" -> Process.main MarkUp.mencodeXml args b
            "Js"   -> Process.main Js.mencode args b
            "ID3"   -> Process.main ID3.mencode args b
            --"Sh"   -> Sh.main args b

            "Pnm"  -> Process.main Pnm.mencode args b
            "Rtf"  -> Process.main Pandoc.mencode_rtf args b
            "Docx"  -> Process.main Pandoc.mencode_docx args b
            "Odt"  -> Process.main Pandoc.mencode_odt args b

            "Svg"  -> Process.main Svg.mencode args b
            "TTF"  -> Process.main TTF.mencode args b
            "CSS"  -> Process.main Css.mencode args b
            "Wav"  -> Process.main Wav.mencode args b
            "CPIO" -> Process.main CPIO.mencode args b
            "Regex" -> Process.main Regex.mencode args b

            --"MarkUp" -> Process.main MarkUp.mencodeHtml args b
            --"MarkUpSvg" -> Process.main MarkUpSvg.mencode args b

            --"Http" -> Http.main args b
            --"Tftp" -> Tftp.main args b
            --"Dns" -> Dns.main args b
            "URI"   -> Process.main URI.mencode args b

            --"MBox"   -> MBox.main args b
            "Unicode" -> Process.main Unicode.mencode args b
            "BS"   -> Process.main ByteString.bencode args b

#endif
            _      -> print "Unsupported Type"

-- | Just checks that the command and the action are executables in the current
-- system
safetyChecks :: MainArgs -> IO ()
safetyChecks args = do
    return ()
    --cmdex <- findExecutable cmd
    --unless (isJust cmdex) (die $ "The command \"" ++ cmd ++ "\" is not present.")
    --let act = findAct args
    --actx <- findExecutable act
    --unless (isJust actx) (die $ "The action \"" ++ act ++ "\" cannot be done.")
        
main = do
    interface <- cli
    runApp interface dispatch
