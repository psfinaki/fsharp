// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio
open Microsoft.VisualStudio.LanguageServices
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.CodeAnalysis.ExternalAccess.FSharp.Editor.Shared.Utilities

[<Export(typeof<IViewTaggerProvider>)>]
[<TagType(typeof<CodeLensGeneralTag>)>]
[<ContentType(FSharpConstants.FSharpContentTypeName)>]
[<TextViewRole(PredefinedTextViewRoles.Document)>]
type internal CodeLensProvider  
    [<ImportingConstructor>]
    (
        [<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider,
        textDocumentFactory: ITextDocumentFactoryService,
        metadataAsSource: FSharpMetadataAsSourceService,
        typeMap : FSharpClassificationTypeMap Lazy,
        settings: EditorOptions
    ) =

    let taggers = ResizeArray()
    let componentModel = Package.GetGlobalService(typeof<ComponentModelHost.SComponentModel>) :?> ComponentModelHost.IComponentModel
    let workspace = componentModel.GetService<VisualStudioWorkspace>()

    /// Returns n provider for the textView if already one has been created. Else create one.
    let addCodeLensProviderOnce wpfView buffer =
        let res = taggers |> Seq.tryFind(fun (view, _) -> view = wpfView)
        match res with
        | Some (_, (tagger, _)) -> tagger
        | None ->
            let documentId = 
                lazy (
                    match textDocumentFactory.TryGetTextDocument(buffer) with
                    | true, textDocument ->
                         Seq.tryHead (workspace.CurrentSolution.GetDocumentIdsWithFilePath(textDocument.FilePath))
                    | _ -> None
                    |> Option.get
                )

            let tagger = CodeLensGeneralTagger(wpfView, buffer)
            let service = FSharpCodeLensService(serviceProvider, workspace, documentId, buffer, metadataAsSource, componentModel.GetService(), typeMap, tagger, settings)
            let provider = (wpfView, (tagger, service))
            wpfView.Closed.Add (fun _ -> taggers.Remove provider |> ignore)
            taggers.Add((wpfView, (tagger, service)))
            tagger

    [<Export(typeof<AdornmentLayerDefinition>); Name("CodeLens");
      Order(Before = PredefinedAdornmentLayers.Text);
      TextViewRole(PredefinedTextViewRoles.Document)>]
    member val CodeLensAdornmentLayerDefinition : AdornmentLayerDefinition = null with get, set

    interface IViewTaggerProvider with
        override _.CreateTagger(view, buffer) =
            if settings.Advanced.CodeLensOptions.Enabled then
                let wpfView =
                    match view with
                    | :? IWpfTextView as view -> view
                    | _ -> invalidOp "No IWPFTextView was given when attempting to create a tagger in F# CodeLens"
            
                box(addCodeLensProviderOnce wpfView buffer) :?> _
            else
                null
