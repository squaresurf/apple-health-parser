# apple-health-parser

A simple parser that will parse out weight and fat percentage from an Apple Health App XML export.

## Usage

\* You must have [stack](https://docs.haskellstack.org/en/stable/README/) installed.

Run: `stack exec apple-health-parser-exe path/to/your/apple/export.xml`

When finished, it will write out weight.csv and fat_percent.csv files.
