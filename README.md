# automerge-pandoc

CLI tool that converts [Automerge rich text](https://automerge.org/docs/reference/documents/rich-text/) to/from other formats, leveraging [Pandoc](https://pandoc.org/). Built using [Haskell](https://www.haskell.org/).

## How it Works

[Automerge Spans](https://automerge.org/automerge/api-docs/js/types/next.Span.html) (their JSON format) are used as the representation of Automerge rich text. When converting from Automerge spans to other formats, a conversion to the Haskell [Pandoc](https://hackage.haskell.org/package/pandoc-types-1.23.1/docs/Text-Pandoc-Definition.html) type is done first. This type corresponds to Pandoc's model for rich text. Then, Pandoc can handle producing an output to any of its supported formats. The inverse operation (other formats to Automerge spans) are done by first converting to the `Pandoc` model and then mapping it to the Automerge spans. Reading from and writing to various formats using Pandoc is based on its [simple example](https://pandoc.org/using-the-pandoc-api.html#a-simple-example).

## Prerequisites

Install GHC and [cabal](https://www.haskell.org/cabal/), e.g. with [GHCup](https://www.haskell.org/ghcup/), so that you can run the CLI. Use the GHC version that `with-compiler` names in `cabal.project`, and a cabal version that supports it.

### MacOS

Pandoc needs [pkg-config](https://formulae.brew.sh/formula/pkgconf) to be built in MacOS. To install it, first install [Homebrew](https://brew.sh/) and then run `brew install pkg-config`.

## How to Run

Navigate to project root and run `cabal build` to compile the project.

### Convert from Automerge Spans

#### To Pandoc AST

```
cabal run automerge-pandoc -- fromAutomerge --to pandoc AUTOMERGE_SPANS_JSON
```

#### To Markdown

```
cabal run automerge-pandoc -- fromAutomerge --to markdown AUTOMERGE_SPANS_JSON
```

#### To HTML

```
cabal run automerge-pandoc -- fromAutomerge --to html AUTOMERGE_SPANS_JSON
```

#### To JSON

```
cabal run automerge-pandoc -- fromAutomerge --to json AUTOMERGE_SPANS_JSON
```

### Convert to Automerge Spans

#### From Pandoc AST

```
cabal run automerge-pandoc -- toAutomerge --from pandoc PANDOC_AST
```

#### From Markdown

```
cabal run automerge-pandoc -- toAutomerge --from markdown MARKDOWN_STRING
```

#### From HTML

```
cabal run automerge-pandoc -- toAutomerge --from html HTML_STRING
```

#### From JSON

```
cabal run automerge-pandoc -- toAutomerge --from json JSON_STRING
```

## Testing

Navigate to the project root folder and run the following command to build and run the tests:

```
cabal test
```

## Dependency Versions

The dependencies are solved from the state of the package index in `index-state` in `cabal.project`, and `cabal.project.freeze` pins their versions. Commit both with any change to the dependencies.

After adding or removing a dependency, run `cabal freeze`.

To upgrade the dependencies:

1. Run `cabal update`, which prints the state it updated the index to.
2. Set `index-state` in `cabal.project` to that time.
3. Delete `cabal.project.freeze` and run `cabal freeze`.
4. Build, run the tests, and commit `cabal.project` together with `cabal.project.freeze`.
