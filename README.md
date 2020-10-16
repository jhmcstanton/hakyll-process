# hakyll-process

Hakyll compilers for passing Hakyll items to external processes. This is useful for tools that do not have
Haskell bindings or may not make sense to have Haskell bindings, like Typescript or LaTex compilers.

## Example Usage:

This example shows how this library can be used to include latex files in your
site source and include the output pdf in your target site.

```haskell
import           Hakyll.Process

main = do
  hakyll $ do
    match "resume/*.tex" $ do
            route   $ setExtension "pdf"
            compile $ execCompilerWith (execName "xelatex") [ProcArg "-output-directory", HakFilePath] (COutFile (newExtOutFilePath ("pdf")))
```
