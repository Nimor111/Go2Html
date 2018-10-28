# Go2Html Highlighter (WIP)

Library that takes in Go code, passes it through a lexer and outputs it highlighted in an html file

# How to build

Install [stack](https://docs.haskellstack.org/en/stable/README/) build tool

Run following commands from root of project:

```
stack build ( might take a long time in the beginning )
stack exec golangToHtml <file_with_code>
```

Result will be in `source.html`
