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

This program.

| File             | Original Size (Bytes) | Compressed Size (Bytes) | Ratio (Compressed/Original) |
| dom_casmurro.txt | 389670                | 219091                  | 0.562248                    |
| fonte0.txt       | 1000                  | 292                     | 0.292000                    |
| fonte1.txt       | 1000000               | 416105                  | 0.416105                    |
| fonte.txt        | 1000000               | 368845                  | 0.368845                    |
| lena.bmp         | 263290                | 252557                  | 0.959235                    |
| TEncEntropy.txt  | 19415                 | 14356                   | 0.739428                    |
| TEncSearch.txt   | 253012                | 165855                  | 0.655522                    |

Ubuntu's `zip` utility.

| File             | Original Size (Bytes) | Compressed Size (Bytes) | Ratio (Compressed/Original) |
| dom_casmurro.txt | 389670                | 153834                  | 0.394780                    |
| fonte0.txt       | 1000                  | 501                     | 0.501000                    |
| fonte1.txt       | 1000000               | 489832                  | 0.489832                    |
| fonte.txt        | 1000000               | 428496                  | 0.428496                    |
| lena.bmp         | 263290                | 227772                  | 0.865099                    |
| TEncEntropy.txt  | 19415                 | 4409                    | 0.227092                    |
| TEncSearch.txt   | 253012                | 38759                   | 0.153190                    |
