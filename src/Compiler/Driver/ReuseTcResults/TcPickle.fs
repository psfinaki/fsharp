module internal FSharp.Compiler.ReuseTcResults.TcPickle

open FSharp.Compiler.CheckDeclarations
open FSharp.Compiler.ParseAndCheckInputs

open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreePickle


// pickling 

let pickleTcState (tcState: TcState) (st: WriterState) =
    p_ccuref_new tcState.tcsCcu st
    p_bool tcState.tcsCreatesGeneratedProvidedTypes st
    (p_list p_tcs_root_sig) (tcState.tcsRootSigs.ToList()) st
    p_list p_qualified_name_of_file (tcState.tcsRootImpls.ToList()) st
    p_modul_typ_new tcState.tcsCcuSig st
    p_list p_open_decl tcState.tcsImplicitOpenDeclarations st
    
let pickleTopAttribs (tcInfo: TopAttribs) (st: WriterState) =
    p_tup3
        p_attribs
        p_attribs
        p_attribs
        (tcInfo.mainMethodAttrs, tcInfo.netModuleAttrs, tcInfo.assemblyAttrs)
        st

let pickleCheckedImplFile (checkedImplFile: CheckedImplFile) (st: WriterState) =
    p_checked_impl_file checkedImplFile st


// unpickling


let unpickleTcState (st: ReaderState) : TcState =
    let tcsCcu = u_ccuref_new st
    let tcsCreatesGeneratedProvidedTypes = u_bool st
    let tcsRootSigs = u_list u_tcs_root_sig st
    let tcsRootImpls = u_list u_qualified_name_of_file st
    let tcsCcuSig = u_modul_typ_new st
    let tcsImplicitOpenDeclarations = u_list u_open_decl st

    { 
        tcsCcu = tcsCcu
        tcsCreatesGeneratedProvidedTypes = tcsCreatesGeneratedProvidedTypes
        tcsTcSigEnv = Unchecked.defaultof<_>
        tcsTcImplEnv = Unchecked.defaultof<_>
        tcsRootSigs = RootSigs.FromList(qnameOrder, tcsRootSigs)
        tcsRootImpls = RootImpls.Create(qnameOrder, tcsRootImpls)
        tcsCcuSig = tcsCcuSig
        tcsImplicitOpenDeclarations = tcsImplicitOpenDeclarations 
    }

let unpickleTopAttribs st : TopAttribs =
    let mainMethodAttrs, netModuleAttrs, assemblyAttrs =
        u_tup3
            u_attribs
            u_attribs
            u_attribs
            st

    {
        mainMethodAttrs = mainMethodAttrs
        netModuleAttrs = netModuleAttrs
        assemblyAttrs = assemblyAttrs
    }

let unpickleCheckedImplFile st : CheckedImplFile =
    u_checked_impl_file st