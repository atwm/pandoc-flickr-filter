# pandoc-flickr-filter

A [Pandoc](https://pandoc.org) filter that rewrites Flickr photo-page links
as proper figures with captions, using the
[Flickr oEmbed API](https://www.flickr.com/services/oembed/) to fetch the
static image URL.

## What it does

A bare Flickr photo-page URL in your document — either as a plain link or as
a Markdown image (`![My caption](https://www.flickr.com/photos/…)`) — is
replaced with a `Figure` block:

- **HTML output:** `<figure><a href="…"><img …/></a><figcaption>…</figcaption></figure>`
- **LaTeX output:** `\begin{figure}…\caption{…}\end{figure}`

The image is sized to 80% of the text width and the alt text becomes the
caption.

## Usage

### As a standalone filter

```bash
pandoc --filter pandoc-flickr-filter input.md -o output.html
```

### As a Haskell library

```haskell
import FlickrFilter (transformBlocks)
import Text.Pandoc

-- inside your Hakyll or Pandoc pipeline:
pandocCompilerWithTransformM readerOpts writerOpts $ \(Pandoc meta blocks) -> do
    blocks' <- unsafeCompiler (transformBlocks blocks)
    return (Pandoc meta blocks')
```

## Installation

```bash
cabal install pandoc-flickr-filter
```

Requires the system OpenSSL library (`libssl`).

## Input syntax

Any of these forms are recognised:

```markdown
![A mountain](https://www.flickr.com/photos/alice/12345678/)

[See the photo](https://www.flickr.com/photos/alice/12345678/)

https://www.flickr.com/photos/alice/12345678/
```

## License

MIT — see [LICENSE](LICENSE).
