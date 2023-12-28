# Hava (Haskell AVAnza parser)

Hava is a Haskell-based tool designed for personal use, primarily to analyze
stock transactions exported from Avanza. It allows users, especially individual
investors, to easily overview how different companies in their portfolio are
performing. By parsing Avanza's exported CSV files, Hava provides insights
into investment.

## Getting Started

**Prerequisites:**
- Haskell
- Stack

**Installation:**
1. Clone the repository from GitHub.
2. Navigate to the project directory.
3. Run `stack setup` to set up the GHC (Glasgow Haskell Compiler).
4. Build the project using `stack build`.

## Usage
Run Hava using the command `stack run hava-exe -- [options]`. Replace
`[options]` with the specific commands or file paths as needed. 


Running `stack run hava-exe -- --help` will provide the different options and
the arguments needed for those options. One of the more useful commands is
`--group-by-company` which can be ran with `stack run hava-exe --
--group-by-company <path-to-csv-file>`

## Future Enhancements
New features and enhancements will be added as needed. Stay tuned for updates!

## License
MIT License

Copyright (c) [2023] [Daniel Tabacskó]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
