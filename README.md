# ff-parser

## Goal
Parse custom formula with a mix of simple arithmetic operator and function defined externally


```text
f("world","2022","KR")/g("world","2022","KR)
(f("world","2022","KR")+g("world","2022","KR))*t()
f("world","2022","KR")+g("world","2022","KR)
x()+y()+z()

```
## Build
> wasm-pack build --target web

## DEMO
> python3 -m http.server