module Hakyll.Process
    (
      newExtension
    , newExtOutFilePath
    , execName
    , execCompiler
    , execCompilerWith
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
  -- | Abstract representation the path to the Hakyll item.
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
-- Note: like hakyll, assumes that no '.' is present in the extension
newExtension :: String -> FilePath -> FilePath
newExtension ext f = (reverse . dropWhile (/= '.') . reverse $ f) <> ext

-- | Helper function to indicate that the output file name is the same as the input file name with a new extension
-- Note: like hakyll, assumes that no '.' is present in the extension
newExtOutFilePath :: String -> CompilerOut
newExtOutFilePath ext = COutFile $ RelativePath (newExtension ext)

execName ::  String  -> ExecutableName
execName = ExecutableName

-- | Calls the external compiler with no arguments. Returns the output contents as a `ByteString`.
--   If an error occurs this raises an exception.
execCompiler     :: ExecutableName                   -> CompilerOut -> Compiler (Item B.ByteString)
execCompiler name out          = execCompilerWith name [] out

-- | Calls the external compiler with the provided arguments. Returns the output contents as a `ByteString`.
--   If an error occurs this raises an exception.
execCompilerWith :: ExecutableName -> ExecutableArgs -> CompilerOut -> Compiler (Item B.ByteString)
execCompilerWith name exArgs out = do
  input   <- getResourceFilePath
  let args = fmap (hargToArg input) exArgs
  results <- unsafeCompiler $ runExecutable name args out input
  -- just using this to get at the item
  oldBody <- getResourceString
  pure $ itemSetBody results oldBody

runExecutable :: ExecutableName -> [String] -> CompilerOut -> FilePath -> IO B.ByteString
runExecutable (ExecutableName exName) args compilerOut inputFile = withProcessWait procConf waitOutput where
  procConf = setStdout byteStringOutput . proc exName $ args
  waitOutput process = do
    let stmProc = getStdout process
    out <- atomically stmProc
    checkExitCode process
    case compilerOut of
      CStdOut    -> pure out
      COutFile (SpecificPath f) -> B.readFile f
      COutFile (RelativePath f) -> B.readFile (f inputFile)

hargToArg :: FilePath -> ExecutableArg -> String
hargToArg _ (ProcArg s) = s
hargToArg f HakFilePath = f
