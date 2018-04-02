module LaabApp

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.PowerPack

importAll "core-js/shim"
importAll "whatwg-fetch"

let JustLazy = importDefault<obj> "../node_modules/justlazy/src/justlazy.js"
let mutable refcount = 64

let LoadPageEvent = document.createEvent_CustomEvent()
LoadPageEvent.initEvent("loadFragment",true,true)

let fetchMy (url:string) (loadme:Element) post hidden =
    promise {
        let! response = Fetch.fetch (url.Substring(2)) [] 
        let! body = response.text()
        let mydiv = document.createElement_div()
        mydiv.id <- url.Split([|'#'|]).[1].Substring(1)
        mydiv.hidden <- hidden
        mydiv.innerHTML <- body
        loadme.appendChild(mydiv) |> ignore
        post()
        printfn "fini de charger : %s" url
        refcount <- refcount - 1
        if (refcount = 0) 
        then 
            document.dispatchEvent(LoadPageEvent) |> ignore
        else 
            ()
        return ()
    }

let findVisible (l : NodeListOf<Element>) =
    seq { for i in 0.0 .. (l.length - 1.0) do
            let el = l.item(i) :?> HTMLElement
            if (not el.hidden) then yield el }


let makeVisible (l: NodeListOf<Element>) =
    for i in 0.0 .. (l.length - 1.0) do
        JustLazy?registerLazyLoad(l.item(i)) |> ignore


let rec fluff (id: string) target origin =
    printfn "fluff : %A %A %A" id target origin
    try
        let curactive = document.querySelector(origin + ".active")
        match curactive with
        | null -> ()
        | _ when not (id = "Content.html" && curactive.id = "first") -> printfn "removing active class for : %s" curactive.id; curactive.classList.remove("active")
        | _ -> ()

        let el = document.getElementById(id)
        match el with
        | null -> printfn "Got null with document.getElementById(%s)" id; failwith "null element"
        | _ -> el.hidden <- false        
               el.classList.add("active")

        let old = document.querySelectorAll("#" + target + "> div") |> findVisible
        match Seq.isEmpty old with
        | true -> ()
        | false -> (old |> Seq.head).hidden <- true
        if System.String.Equals(el.id,"first") then
                    fluff "firstContent" "LoadMe" "a.pageFetcher" |> ignore
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
                 | Content s -> (fun _ -> fluff "Content.html" "content" "nav a" |> ignore
                                          fluff s "LoadMe" "a.pageFetcher" |> ignore; Some mapRes)
                 | Nav s -> (fun _ -> fluff s "content" "nav a" |> ignore; Some mapRes)         
    Some mapRes, mapFun
    
let readyR s =
    let sub dispatch =
        document.addEventListener("DOMContentLoaded",
                                  U2.Case1 (unbox (fun _ -> toload "content" "nav a"; printfn "content %A dispatch %A" s dispatch; dispatch s)))
    Cmd.ofSub sub

let readyF r =
    let sub dispatch =
        document.addEventListener("loadFragment",
                                   U2.Case1 (unbox (fun _ -> printfn "dispatch %A dispatch %A" r dispatch; (snd (mapRoute r))() |> ignore; dispatch r)))
    Cmd.ofSub sub

let urlUpdate r _ =
    let mapRes,mapFun = mapRoute r
    mapRes, Cmd.ofFunc mapFun () id (fun _ -> mapRes)

Program.mkSimple (fun a -> printfn "init %A" a; a) (fun a  b -> printfn "update a: %A b: %A" a b; b) (fun a b -> printfn "view a: %A b: %A" a b; a)
|> Program.withSubscription readyR
|> Program.withSubscription readyF
|> Program.toNavigable (parseHash route) urlUpdate
|> Program.run
