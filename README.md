# hhuff

Huffman tree compression written in Haskell.

Build with [Stack](https://docs.haskellstack.org/en/stable/README/).

```
$ stack build
```

Encode a text file with the `-t` flag.

```
$ stack run -- -t input.txt compressed.bin
```

Decode a compressed text file with the `-T` flag.

```
$ stack run -- -T compressed.bin output.txt
```

Encode any other file with the `-b` ('binary') flag.

```
$ stack run -- -b example.png compressed.bin
```

Decode a compressed binary file with the `-B` flag.

```
$ stack run -- -B compressed.bin output.png
```

## Compression ratios

| File               | Mode   | Original Size (Bytes) | Compressed Size (Bytes) | Ratio (Compressed/Original) |
| `dom_casmurro.txt` | Text   | 389670                | 219091                  | 0.5622475428                |
| `fonte.txt`        | Text   | 1000000               | 368845                  | 0.368845                    |
| `fonte0.txt`       | Text   | 1000                  | 292                     | 0.292                       |
| `fonte1.txt`       | Text   | 1000000               | 416105                  | 0.416105                    |
| `TEncEntropy.txt`  | Text   | 19415                 | 14356                   | 0.7394282771                |
| `TEncSearch.txt`   | Text   | 253012                | 165855                  | 0.6555222677                |
| `lena.bmp`         | Binary | 263290                | 252557                  | 0.959235064                 |
