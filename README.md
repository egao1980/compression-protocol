# compression-protocol

Codecs + zip archives for [cl-stack](https://github.com/egao1980/cl-stack). HTTP still owns Content-Encoding names / `:identity`; this protocol owns bytes.

| System | Role |
|--------|------|
| `compression-protocol` | `compress` / `decompress`, stream GFs, `open-archive` (`:zip` `:tar` `:tar.gz` `:tar.bz2`) |
| `compression-backend-chipz` | **Default** — chipz + salza2 for `:gzip` `:zlib` `:deflate`; chipz inflate for `:bzip2` |

`:br` / `:zstd` / `:snappy` come from `cl-stack-brotli` / `cl-stack-zstd` / `cl-stack-snappy`
(eql methods on `compress-using-algorithm`). Nick `stack-compression`.

```lisp
(asdf:load-system "compression-backend-chipz")
(compression-protocol:decompress
 (compression-protocol:compress "hi" :algorithm :gzip)
 :algorithm :gzip)

(let ((z (compression-protocol:open-archive #p"data.zip" :format :zip)))
  (compression-protocol:read-entry z "readme.txt"))

(let ((tarball (compression-protocol:write-archive-bytes
                '(("a.txt" "hi")) :format :tar.gz)))
  (compression-protocol:read-entry
   (compression-protocol:open-archive tarball :format :tgz) "a.txt"))
```

`decompress … :max-output N` reads through the backend's decompressing stream and signals `compression-limit-exceeded` past N bytes. N is also passed to `make-decompressing-stream-using-algorithm` as `:max-output`, so a backend that allocates up front (Snappy's declared length) can refuse before allocating. Incremental backends (chipz) ignore it.

ZIP method 0 (stored) and 8 (deflate via the codec GFs). ustar read/write; `:tar.gz` / `:tgz` wrap gzip. `:bzip2` decompress only (no Lisp compressor). `:xz` stays overlay/unsupported.

## Damaged data

Callers handle one type, `compression-error`, as Python code catches `zlib.error` / `BadZipFile`:

- Data the chipz backend cannot decode signals `compression-error`; `compression-error-cause` is chipz's own condition. This holds for `decompress` (with or without `:max-output`) and for decompressing streams.
- `open-archive` / `read-entry` on a zip signal `archive-error` (a `compression-error`) for a record that runs past the end of the data, a bad signature, a ZIP64 marker (not supported) or an unsupported method.
- `read-entry` checks each zip entry's length and CRC-32 against the central directory, as `zipfile` does (`BadZipFile: Bad CRC-32`). A mismatch signals `archive-error` with a `continue` restart that returns the bytes anyway.
- `read-entry … :max-size N` signals `compression-limit-exceeded` for an entry larger than N bytes, and inflates at most N + 1 bytes even when the header understates the size (zip bombs). Without it, as in `zipfile`, there is no bound.
- A zip entry name is UTF-8 when it decodes (or when bit 11 says so); otherwise ISO-8859-1, where `zipfile` uses CP437 (encoding-protocol has no CP437 yet: egao1980/encoding-protocol#4).

Known gap: chipz's bzip2 decoder signals plain `simple-error` / `type-error` for most damaged bytes, which the backend does not translate (#8). Raw `:deflate` has no checksum, so chipz may return bytes for damaged data; in a zip the CRC-32 check catches them.

## License

MIT — see [LICENSE](LICENSE).
