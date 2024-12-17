# automerge-pandoc

CLI tool that converts Automerge spans to/from other formats, leveraging [Pandoc](https://pandoc.org/). Built using [Haskell](https://www.haskell.org/).

## How it Works

[Automerge Spans](https://automerge.org/automerge/api-docs/js/types/next.Span.html) (their JSON format) are used as the representation of Automerge rich text. When converting from Automerge spans to other formats, a conversion to the Haskell [Pandoc](https://hackage.haskell.org/package/pandoc-types-1.23.1/docs/Text-Pandoc-Definition.html) type is done first. This type corresponds to Pandoc's model for rich text. Then, Pandoc can handle producing an output to any of its supported formats. The inverse operation (other formats to Automerge spans) are done by first converting to the `Pandoc` model and then mapping it to the Automerge spans. Reading from and writing to various formats using Pandoc is based on its [simple example](https://pandoc.org/using-the-pandoc-api.html#a-simple-example).

## Prerequisites

Install [Haskell Stack](https://docs.haskellstack.org/en/stable/#__tabbed_1_1) so that you can run the CLI.

### MacOS

Pandoc needs [pkg-config](https://formulae.brew.sh/formula/pkgconf) to be built in MacOS. To install it, first install [Homebrew](https://brew.sh/) and then run `brew install pkg-config`.

## How to Run

Navigate to project root and run `stack build` to compile the project.

### Convert from Automerge Spans

#### To Pandoc AST

```
stack exec automerge-pandoc fromAutomerge -- --to pandoc AUTOMERGE_SPANS_JSON
```

#### To Markdown

```
stack exec automerge-pandoc fromAutomerge -- --to markdown AUTOMERGE_SPANS_JSON
```

#### To HTML

```
stack exec automerge-pandoc fromAutomerge -- --to html AUTOMERGE_SPANS_JSON
```

### Convert to Automerge Spans

#### To Pandoc AST

```
stack exec automerge-pandoc toAutomerge -- --from pandoc PANDOC_AST
```

#### To Markdown

```
stack exec automerge-pandoc toAutomerge -- --from markdown MARKDOWN_STRING
```

#### To HTML

```
stack exec automerge-pandoc toAutomerge -- --from html HTML_STRING
```

## Notes on Dependency Versions

It wasn't possible to make both [`aeson`](https://hackage.haskell.org/package/aeson) and [VSCode Haskell Extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell#supported-ghc-versions) work with a version newer than version `lts-22.43`, [which corresponds to GHC 9.6.6](https://www.stackage.org/).

This limitation results in using some older versions of `aeson` and `pandoc` (not the latest ones at the time of writing), which is something that needs to be reviewed and ideally solved soon.
