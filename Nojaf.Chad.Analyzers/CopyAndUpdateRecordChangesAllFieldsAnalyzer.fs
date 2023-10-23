module Nojaf.Chad.Analyzers.CopyAndUpdateRecordChangesAllFieldsAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

type UpdateRecord = SynExprRecordField list * range

// This attribute is required and needs to match the correct context type!
[<CliAnalyzer("CopyAndUpdateRecordChangesAllFieldsAnalyzer",
              "Detect if all fields of an update record are updated.",
              "https://github.com/fsharp/fslang-suggestions/issues/603")>]
let copyAndUpdateRecordChangesAllFieldsAnalyzer : Analyzer<CliContext> =
    fun (context : CliContext) ->
        async {
            let untypedRecordUpdates =
                let xs = ResizeArray<UpdateRecord> ()

                let collector =
                    { new SyntaxCollectorBase() with
                        override x.WalkExpr (e : SynExpr) =
                            match e with
                            | SynExpr.Record (copyInfo = Some _ ; recordFields = fields) -> xs.Add (fields, e.Range)
                            | _ -> ()
                    }

                walkAst collector context.ParseFileResults.ParseTree
                Seq.toList xs

            let messages = ResizeArray<Message> untypedRecordUpdates.Length

            let handler m (t : FSharpType) =
                let matchingUnTypedNode =
                    untypedRecordUpdates |> List.tryFind (fun (_, mExpr) -> Range.equals mExpr m)

                match matchingUnTypedNode with
                | None -> ()
                | Some (fields, mExpr) ->

                if not t.TypeDefinition.IsFSharpRecord then
                    ()
                else if t.TypeDefinition.FSharpFields.Count = fields.Length then
                    messages.Add
                        {
                            Type = "CopyAndUpdateRecordChangesAllFieldsAnalyzer analyzer"
                            Message =
                                "All record fields of record are being updated. Consider creating a new instance instead."
                            Code = "NOJAF-001"
                            Severity = Warning
                            Range = mExpr
                            Fixes = []
                        }

            match context.TypedTree with
            | None -> ()
            | Some typedTree -> typedTree.Declarations |> List.iter (TASTCollecting.visitDeclaration handler)

            return Seq.toList messages
        }
