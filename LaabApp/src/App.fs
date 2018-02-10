module LaabApp

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.PowerPack

importAll "core-js/shim"
importAll "whatwg-fetch"

let JustLazy = importDefault<obj> "../node_modules/justlazy/src/justlazy.js"

let ready fn =
    if (document.readyState <> "loading") 
    then 
        fn()
    else
        document.addEventListener("DOMContentLoaded",
                                  U2.Case1 (unbox (fun _ -> fn())))

let fetchMy url (loadme:Element) post post2 hidden =
    promise {
        let! response = Fetch.fetch url [] 
        let! body = response.text()
        let mydiv = document.createElement_div()
        mydiv.id <- url
        mydiv.hidden <- hidden
        mydiv.innerHTML <- body
        loadme.appendChild(mydiv) |> ignore
        post()
        post2()
        return ()
    }

let findVisible (l : NodeListOf<Element>) =
    seq { for i in 0.0 .. (l.length - 1.0) do
            let el = l.item(i) :?> HTMLElement
            if (not el.hidden) then yield el }


let makeVisible (l: NodeListOf<Element>) =
    for i in 0.0 .. (l.length - 1.0) do
        JustLazy?registerLazyLoad(l.item(i)) |> ignore

let rec fluff (el: HTMLElement) url target origin =
    let curactive = document.querySelector(origin + ".active")
    try
        match curactive with
        | null -> ()
        | _ -> curactive.classList.remove("active")
        let old = document.querySelectorAll("#" + target + "> div") |> findVisible
        match Seq.isEmpty old with
        | true -> ()
        | false -> (old |> Seq.head).hidden <- true
        let newVis = document.getElementById(url)
        match newVis with
        | null -> printfn "Got null with document.getElementById(%s)" url
        | _ -> newVis.hidden <- false        
        if System.String.Equals(el.id,"first") then
            fluff (document.getElementById("firstContentClick")) "firstContent" "LoadMe" "a.pageFetcher" |> ignore
        else
            makeVisible(newVis.querySelectorAll(".justlazy-spinner"))
        with
            | ex -> printfn "%s" ex.Message
    el.classList.add("active")
    box false

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
        let hidden = not (System.String.Equals(url,"Content.html"))
        let postFirstContent = match url with
                                | "Content.html" ->  (fun _ -> makeVisible(document.getElementById("firstContent").querySelectorAll(".justlazy-spinner")))
                                | _ -> ignore
        fetchMy url loadme reparseFun postFirstContent hidden |> Async.AwaitPromise |> Async.StartImmediate
        el.onclick <- (fun _ -> fluff el url target origin)

let init() =
    toload "content" "nav a"

ready init
