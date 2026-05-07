# pandoc-flickr-filter

[Flickr](https://www.flickr.com) requires a backlink to the original photo page on Flickr for any embedded content. So simply placing the Flickr URL into the markdown will not suffice. What has to be done is to find the correct static page and then wrap this picture in a picture link to the original photo page. This is where the [Pandoc](https://pandoc.org) filter comes in. It rewrites Flickr photo-page links to show both the image and the backlink to the original photo page. The filter transforms Flickr photo-page links in your markdown document into embedded images with captions, ensuring proper attribution to the original source. It does this by fetching the static image URL using the [Flickr oEmbed API](https://www.flickr.com/services/oembed/), allowing you to display Flickr photos seamlessly in your documents while adhering to Flickr's embedding requirements.


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
