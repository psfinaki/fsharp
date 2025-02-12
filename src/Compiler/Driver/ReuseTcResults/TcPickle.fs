module internal FSharp.Compiler.ReuseTcResults.TcPickle

open FSharp.Compiler.CheckDeclarations

open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreePickle


// pickling 

let pickleTcState (tcState: PickledTcState) (st: WriterState) =
    p_ccuref_new tcState.TcsCcu st
    p_bool tcState.TcsCreatesGeneratedProvidedTypes st
    (p_list p_tcs_root_sig) tcState.TcsRootSigs st
    p_list p_qualified_name_of_file tcState.TcsRootImpls st
    p_modul_typ_new tcState.TcsCcuSig st
    p_list p_open_decl tcState.TcsImplicitOpenDeclarations st
    
let pickleTcInfo (tcInfo: TopAttribs) (st: WriterState) =
    p_tup3
        p_attribs
        p_attribs
        p_attribs
        (tcInfo.mainMethodAttrs, tcInfo.netModuleAttrs, tcInfo.assemblyAttrs)
        st

let pickleCheckedImplFile (checkedImplFile: CheckedImplFile) (st: WriterState) =
    p_checked_impl_file checkedImplFile st


// unpickling


let unpickleTcState (st: ReaderState) : PickledTcState =
    let tcsCcu = u_ccuref_new st
    let tcsCreatesGeneratedProvidedTypes = u_bool st
    let tcsRootSigs = u_list u_tcs_root_sig st
    let tcsRootImpls = u_list u_qualified_name_of_file st
    let tcsCcuSig = u_modul_typ_new st
    let tcsImplicitOpenDeclarations = u_list u_open_decl st

    { 
        TcsCcu = tcsCcu
        TcsCreatesGeneratedProvidedTypes = tcsCreatesGeneratedProvidedTypes
        TcsRootSigs = tcsRootSigs
        TcsRootImpls = tcsRootImpls
        TcsCcuSig = tcsCcuSig
        TcsImplicitOpenDeclarations = tcsImplicitOpenDeclarations 
    }

let unpickleTcInfo st : TopAttribs =
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