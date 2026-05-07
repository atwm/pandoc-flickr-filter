# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.1] - 2026-05-07

### Changed
- Replaced `HsOpenSSL`/`http-client-openssl` with `http-client-tls` for TLS
  support.  `newTlsManager` loads the system certificate store automatically,
  ensuring enterprise proxy certificates are handled correctly.
- Removed dependency on `HsOpenSSL-x509-system`.

## [0.1.0.0] - 2026-05-07

### Added
- Initial release.
- Detects bare Flickr photo-page links and `![alt](flickr-url)` images in
  Pandoc documents.
- Fetches the static image URL via the Flickr oEmbed API.
- Rewrites matching nodes as `Figure` blocks with a `<figure>`/`<figcaption>`
  in HTML output and a `\begin{figure}` environment in LaTeX output.
- Uses `http-client-openssl` for TLS, delegating certificate validation to the
  system OpenSSL library.
