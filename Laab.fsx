#I @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.6.2"
#r @"node_modules\fable-core\Fable.Core.dll"
#r @"node_modules\fable-powerpack\Fable.PowerPack.dll"

open Fable.Core
open Fable.Import.Browser
open Fable.PowerPack.Fetch
open Fable.Core.JsInterop

let ready fn =
    if (document.readyState <> "loading") 
    then 
        fn()
    else
        document.addEventListener("DOMContentLoaded",
                                  U2.Case1 (EventListener(fun _ -> fn())))

let JustLazy = importDefault<obj> "./node_modules/justlazy/src/justlazy.js"
let fetchMy url (loadme:Element) post =
    async {
        let! response = fetch url [] |> Async.AwaitPromise
        let! body = response.text() |> Async.AwaitPromise
        loadme.innerHTML <- body
        post()
        JustLazy?registerLazyLoadByClass("justlazy-spinner") |> ignore
        return ()
    }

let rec toload target origin =
    let loadme = document.getElementById(target)
    let links = document.querySelectorAll(origin)
    let l = links.length
    let reparseFun =
        match target with
        | "content" -> (fun _ -> toload "LoadMe" "a.pageFetcher")
        | _ -> ignore
    for i in 0.0 .. (l-1.0) do
        let el = links.item(i) :?> HTMLElement
        let url = el.getAttribute("href")
        el.onclick <- (fun _ -> fetchMy url loadme reparseFun
                                |> Async.StartImmediate
                                box false)

let init() =
    toload "content" "nav a"
    document.getElementById("first").click()
    document.getElementById("first").focus()

ready init
