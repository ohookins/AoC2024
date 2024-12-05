open System
open System.Reflection
open CommandLine

type options = {
    [<Option('d', "day", Required = true, HelpText = "Challenge day.")>] day : int;
    [<Option('p', "part", Required = true, HelpText = "Part 1 or 2 of the day challenge.")>] part : int;
    [<Option('t', "test", Default = false, Required = false, HelpText = "Use test input instead of real input.")>] test : bool;
}

let findAndRunModule(day: int, part: int, test: bool) =
    let assembly = Assembly.GetExecutingAssembly()
    let moduleType = assembly.GetType($"Day{day}.Part{part}")
    let method = moduleType.GetMethod("solve")
    method.Invoke(null, [|test|])

let run (opts: options) =
    printfn "day: %d" opts.day
    printfn "part: %d" opts.part
    printfn "use test input: %b" opts.test
    ignore(findAndRunModule(opts.day, opts.part, opts.test))

let fail (errors: seq<Error>) =
    printfn "Failed to parse arguments: %A" errors

[<EntryPoint>]
let main argv =
    let result = Parser.Default.ParseArguments<options>(argv)
    match result with
    | :? Parsed<options> as parsed -> run parsed.Value
    | :? NotParsed<options> as notParsed -> fail notParsed.Errors
    | _ -> printfn "something went wrong"

    0