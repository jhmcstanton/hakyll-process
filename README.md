# hakyll-process

Hakyll compilers for passing Hakyll items to external processes. This is useful for tools that do not have
Haskell bindings or may not make sense to have Haskell bindings, like Typescript or LaTex compilers.

## Common Usage

There are a few main entry points for this library:

- `execCompilerWith` is the most common. This allows the caller to declaratively run an external process
  and includes support for passing arguments.
- `execCompiler` is an aliased version of `execCompilerWith` that provides no arguments to the external executable/process.
- `unsafeExecCompiler` is the less type safe and more manual method to calling external executables. This provides
  no helper functionality but may be useful if you are already manually building out a compiler in your site generator.

### Example

This example shows how this library can be used to include latex files in your
site source and include the output pdf in your target site.

```haskell
import qualified Data.ByteString.Lazy.Char8 as B
import           Hakyll.Process

main = do
  hakyll $ do
    match "resume/*.tex" $ do
      route   $ setExtension "pdf"
      compile $ execCompilerWith (execName "xelatex") [ProcArg "-output-directory=resume/", HakFilePath] (newExtOutFilePath "pdf")

    -- alternative, manual method, using unsafeExecCompiler
    match "resume/*.tex" $ do
      route   $ setExtension "pdf"
      compile $ do
        input <- getResourceFilePath
        let outputReader _ = B.readFile (newExtension "pdf" input)
        unsafeExecCompiler (execName "xelatex") ["-output-directory=resume/", input] outputReader
```
