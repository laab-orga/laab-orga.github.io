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

let fetchMy (url:string) (loadme:Element) post hidden =
    promise {
        let! response = Fetch.fetch (url.Substring(2)) [] 
        let! body = response.text()
        let mydiv = document.createElement_div()
        mydiv.id <- url.Split([|'#'|]).[1]
        mydiv.hidden <- hidden
        mydiv.innerHTML <- body
        loadme.appendChild(mydiv) |> ignore
        post()
        return ()
    }

let findVisible (l : NodeListOf<Element>) =
    seq { for i in 0.0 .. (l.length - 1.0) do
            let el = l.item(i) :?> HTMLElement
            if (not el.hidden) then yield el }


let makeVisible (l: NodeListOf<Element>) =
    for i in 0.0 .. (l.length - 1.0) do
        JustLazy?registerLazyLoad(l.item(i)) |> ignore


let rec fluff (url: string) target origin =
    printfn "fluff : %A %A %A" url target origin
    try
        let idurl = url.Split([|'#'|]).[1]
        let el = document.getElementById(idurl)
        match el with
        | null -> printfn "Got null with document.getElementById(%s)" idurl
        | _ -> el.hidden <- false        
               el.classList.add("active")

        let curactive = document.querySelector(origin + ".active")
        match curactive with
        | null -> ()
        | _ -> curactive.classList.remove("active")

        let old = document.querySelectorAll("#" + target + "> div") |> findVisible
        match Seq.isEmpty old with
        | true -> ()
        | false -> (old |> Seq.head).hidden <- true
        if System.String.Equals(el.id,"first") then
                    fluff "/firstContent" "LoadMe" "a.pageFetcher" |> ignore
        else
            makeVisible(el.querySelectorAll(".justlazy-spinner"))

        with
            | ex -> printfn "%s" ex.Message


let rec toload target origin =
    let loadme = document.getElementById(target)
    let links = document.querySelectorAll(origin)
    let l = links.length
    for i in 0.0 .. (l-1.0) do
        let el = links.item(i) :?> HTMLElement
        let id = el.getAttribute("href")
        let url = "#/" + id
        el.setAttribute("href",url)
        let hidden = not (System.String.Equals(id,"Content.html"))
        let reparseFun =
            match target with
            | "content" when id.Equals("Content.html") -> (fun _ -> toload "LoadMe" "a.pageFetcher")
            | _ -> ignore
        fetchMy url loadme reparseFun hidden |> Async.AwaitPromise |> Async.StartImmediate

open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser

type Route = Nav of string | Content of string

let contentParse (state: string) = 
    if state.StartsWith "content-" then
        Ok state
    else
        Error "Does not start with content-"

let route : Parser<Route->_,_> =
    oneOf
        [ map (Content "firstContent") (top </> s "Content.html")
          map (Content "firstContent") (top)
          map Content (top </> custom "content" contentParse)
          map Nav (top </> str)]

open Elmish


let mapRoute (result:Option<Route>) =
    let mapRes = 
        match result with
        | Some s -> s
        | None -> Content "firstContent"
    let mapFun = match mapRes with
                 | Content s -> (fun _ -> promise { fluff "#/Content.html" "content" "nav a" |> ignore
                                                    fluff ("#/" + s) "LoadMe" "a.pageFetcher" |> ignore })
                 | Nav s -> (fun _ -> promise { fluff ("#/" + s) "content" "nav a" |> ignore })         
    mapRes, (Cmd.ofPromise mapFun () id ignore)
    
let urlUpdate (result:Option<Route>) _ =
    printfn "urlUpdate : %A" result
    mapRoute result
    
let init (result:Option<Route>) =
    ready (fun _ -> toload "content" "nav a")
    printfn "init : %A" result
    mapRoute result

let update _ m = 
    printfn "update : %A" m
    m, Cmd.none

Program.mkProgram init update (fun m _ -> printfn "view : %A" m)
|> Program.toNavigable (parseHash route) urlUpdate
|> Program.run
