# compression-protocol

Codecs + zip archives for [cl-stack](https://github.com/egao1980/cl-stack). HTTP still owns Content-Encoding names / `:identity`; this protocol owns bytes.

| System | Role |
|--------|------|
| `compression-protocol` | `compress` / `decompress`, stream GFs, `open-archive` (`:zip`) |
| `compression-backend-chipz` | **Default** — chipz + salza2 for `:gzip` `:zlib` `:deflate` |

`:br` / `:zstd` / `:snappy` wait for overlay backends. Nick `stack-compression`.

```lisp
(asdf:load-system "compression-backend-chipz")
(compression-protocol:decompress
 (compression-protocol:compress "hi" :algorithm :gzip)
 :algorithm :gzip)

(let ((z (compression-protocol:open-archive #p"data.zip" :format :zip)))
  (compression-protocol:read-entry z "readme.txt"))
```

ZIP method 0 (stored) and 8 (deflate via the codec GFs).

## License

MIT — see [LICENSE](LICENSE).
