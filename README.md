# automerge-pandoc

CLI tool that converts [Automerge rich text](https://automerge.org/docs/under-the-hood/rich_text_schema/) to/from other formats, leveraging [Pandoc](https://pandoc.org/). Built using [Haskell](https://www.haskell.org/).

## A Note on Maturity

Not all Automerge rich text features are supported yet and most probably some corner cases are not handled properly yet. Specifically, blockquotes and images are not implemented yet. Same with the code block language attribute. Incrementally, this repo will be extended to cover the [Automerge Rich Text schema](https://automerge.org/docs/under-the-hood/rich_text_schema/) and then to reach feature parity with some important rich text representations such as Markdown.

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

#### To JSON

```
stack exec automerge-pandoc fromAutomerge -- --to json AUTOMERGE_SPANS_JSON
```

### Convert to Automerge Spans

#### From Pandoc AST

```
stack exec automerge-pandoc toAutomerge -- --from pandoc PANDOC_AST
```

#### From Markdown

```
stack exec automerge-pandoc toAutomerge -- --from markdown MARKDOWN_STRING
```

#### From HTML

```
stack exec automerge-pandoc toAutomerge -- --from html HTML_STRING
```

#### From JSON

```
stack exec automerge-pandoc toAutomerge -- --from json JSON_STRING
```

## Build for WebAssembly

There are instructions for building the project for [WebAssembly](https://webassembly.org/) [here](https://github.com/arisgk/automerge-pandoc/blob/main/WASM_BUILD.md).

## Testing

Navigate to the project root folder and run the following command to build and run the tests:

```
stack build --test
```
