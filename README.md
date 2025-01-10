# AoC2024

As usual I'm attempting to get at least some way through the yearly advent of code.

This time I'm challenging myself to complete it with F# which is outside of my usual C-family comfort space. I'm going to see how far I can get by using it to its strengths. As a result there's a lot of immutability and recursion at play, often at the sacrifice of performance.

## Running

Using Visual Studio on Windows you can just load the project, build it and then run the code.

On other platforms you'll need the dotnet compiler/SDK and build it with `dotnet build`.

This year to avoid _some_ of the boilerplate I've got a single entrypoint for all of the days/parts which can run any given solution by command line arguments. Using my individual input file versus the example input is also selectable. E.g.:

```shell
bin/Debug/net8.0/AoC2024 -d DAY -p PART [-t]
```

## Notes

- Day 9 Part 2 - didn't get the correct answer. I suspected maybe there are some adjacent free blocks that need to be merged, but couldn't find evidence of this. In any case the checksum I arrived at is too high.