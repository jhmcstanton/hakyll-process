{-|
Module      : Hakyll.Process
Description : Common compilers and helpers for external executables.
Stability   : experimental
-}
module Hakyll.Process
    (
      newExtension
    , newExtOutFilePath
    , execName
    , execCompiler
    , execCompilerWith
    , unsafeExecCompiler
    , CompilerOut(..)
    , ExecutableArg(..)
    , ExecutableArgs
    , ExecutableName
    , OutFilePath(..)
    ) where

import qualified Data.ByteString.Lazy.Char8 as B
import           GHC.Conc                        (atomically)
import           Hakyll.Core.Item
import           Hakyll.Core.Compiler
import           System.Process.Typed

-- | Expected output from the external compiler.
data CompilerOut   =
  -- | Compiler uses stdout as the output type.
    CStdOut
  -- | Compiler outputs to a specific target file on the filesystem.
  | COutFile OutFilePath

-- | Arguments to provide to the process
data ExecutableArg =
  -- | Abstract representation of the path to the Hakyll item.
    HakFilePath
  -- | Literal argument to provide to the other process.
  | ProcArg String deriving (Read, Show)

-- | Specifies the output file path of a process.
data OutFilePath   =
  -- | A specific, known filepath.
    SpecificPath FilePath
  -- | Indicates that the output path is related to the input path.
  | RelativePath (FilePath -> FilePath)

-- | Name of the executable if in the PATH or path to it if not
newtype ExecutableName = ExecutableName  String    deriving (Read, Show)
-- | Arguments to pass to the executable. Empty if none.
type ExecutableArgs    = [ExecutableArg]

-- | Helper function to indicate that the output file name is the same as the input file name with a new extension
-- Note: like hakyll, assumes that no "." is present in the extension
newExtension ::
     String   -- ^ New file extension, excluding the leading "."
  -> FilePath -- ^ Original FilePath
  -> FilePath
newExtension ext f = (reverse . dropWhile (/= '.') . reverse $ f) <> ext

-- | Helper function to indicate that the output file name is the same as the input file name with a new extension
-- Note: like hakyll, assumes that no "." is present in the extension
newExtOutFilePath :: String -> CompilerOut
newExtOutFilePath ext = COutFile $ RelativePath (newExtension ext)

execName :: String -> ExecutableName
execName = ExecutableName

-- | Calls the external compiler with no arguments. Returns the output contents as a 'B.ByteString'.
--   If an error occurs this raises an exception.
--   May be useful if you already have build scripts for artifacts in your repository.
execCompiler     :: ExecutableName                   -> CompilerOut -> Compiler (Item B.ByteString)
execCompiler name out          = execCompilerWith name [] out

-- | Calls the external compiler with the provided arguments. Returns the output contents as a 'B.ByteString'.
--   If an error occurs this raises an exception.
execCompilerWith :: ExecutableName -> ExecutableArgs -> CompilerOut -> Compiler (Item B.ByteString)
execCompilerWith name exArgs out = do
  input   <- getResourceFilePath
  let args = fmap (hargToArg input) exArgs
  let outputReader = cOutToFileContents input out
  unsafeExecCompiler name args outputReader

-- | Primarily for internal use, occasionally useful when already building a compiler imperatively.
-- Allows the caller to opt out of the declarative components of 'execCompiler' and 'execCompilerWith'.
unsafeExecCompiler ::
       ExecutableName                    -- ^ Name or filepath of the executable
    -> [String]                          -- ^ Arguments to pass to the executable
    -> (B.ByteString -> IO B.ByteString) -- ^ Action to read the output of the compiler. Input is the stdout of the process.
    -> Compiler (Item B.ByteString)
unsafeExecCompiler (ExecutableName exName) args outputReader =
  do
    results <- unsafeCompiler $ procResults
    -- just using this to get at the item
    oldBody <- getResourceString
    pure $ itemSetBody results oldBody
  where
  procResults = withProcessWait procConf waitOutput
  procConf = setStdout byteStringOutput . proc exName $ args
  waitOutput process = do
    let stmProc = getStdout process
    out <- atomically stmProc
    checkExitCode process
    outputReader out

--                 input fpath                   stdout contents
cOutToFileContents :: FilePath -> CompilerOut -> B.ByteString -> IO B.ByteString
cOutToFileContents _      CStdOut out                  = pure out
cOutToFileContents _     (COutFile (SpecificPath f)) _ = B.readFile  f
cOutToFileContents input (COutFile (RelativePath f)) _ = B.readFile (f input)

hargToArg :: FilePath -> ExecutableArg -> String
hargToArg _ (ProcArg s) = s
hargToArg f HakFilePath = f
