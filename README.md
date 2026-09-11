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

ZIP method 0 (stored) and 8 (deflate via the codec GFs). ustar read/write; `:tar.gz` / `:tgz` wrap gzip. `:bzip2` decompress only (no Lisp compressor). `:xz` stays overlay/unsupported.

## License

MIT — see [LICENSE](LICENSE).
