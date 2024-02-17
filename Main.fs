module Hyve.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bridge

type Model =
    {
        host : int
        cls1 : int
        cls2 : int
        cls3 : int
        cls4 : int
        cls5 : int
        cls6 : int
        lblh : string
        lbl1 : string
        lbl2 : string
        lbl3 : string
        lbl4 : string
        lbl5 : string
        lbl6 : string
    }

let initModel =
    {
        host = 6
        cls1 = 4
        cls2 = 4
        cls3 = 6
        cls4 = 5
        cls5 = 6
        cls6 = 5
        lblh = ""
        lbl1 = ""
        lbl2 = ""
        lbl3 = ""
        lbl4 = ""
        lbl5 = ""
        lbl6 = ""
    }

type Message =
    | SetHost of int
    | SetCls1 of int
    | SetCls2 of int
    | SetCls3 of int
    | SetCls4 of int
    | SetCls5 of int
    | SetCls6 of int
    | LblHost of string
    | LblCls1 of string
    | LblCls2 of string
    | LblCls3 of string
    | LblCls4 of string
    | LblCls5 of string
    | LblCls6 of string

let update message model =
    match message with
    | SetHost value -> { model with host = value }
    | SetCls1 value -> { model with cls1 = value }
    | SetCls2 value -> { model with cls2 = value }
    | SetCls3 value -> { model with cls3 = value }
    | SetCls4 value -> { model with cls4 = value }
    | SetCls5 value -> { model with cls5 = value }
    | SetCls6 value -> { model with cls6 = value }
    | LblHost value -> { model with lblh = value }
    | LblCls1 value -> { model with lbl1 = value }
    | LblCls2 value -> { model with lbl2 = value }
    | LblCls3 value -> { model with lbl3 = value }
    | LblCls4 value -> { model with lbl4 = value }
    | LblCls5 value -> { model with lbl5 = value }
    | LblCls6 value -> { model with lbl6 = value }

let view model dispatch =      
    concat {
        div{
            attr.``class`` "flex-container-1"
            div{
                attr.``class`` "flex-container-2"
                attr.id "header"
                a{
                    attr.href "https://github.com/vykrum/Hywe"
                    attr.target "blank"
                    img{
                        attr.width "30"
                        attr.height "30"
                        attr.src "https://vykrum.github.io/Hywe/favicon-32x32.png"
                    }
                }
                div{
                    attr.id "title"
                    " H Y W E"
                    }
                div{
                    img{
                    attr.width "200"
                    attr.height "45"
                    attr.src "https://vykrum.github.io/Hywe/hyweLogoAcronym.png"
                    }
                }
                }
            div{
                attr.id "description"
                p{
                    "Hywe is a space layout planning concept currently undergoing its formative stages of development being developed as an early stage design interface."
                    br
                    "Manipulating the color coded sliders controls the scale of corresponding clusters."
                }
            }
        }
        div{
            div{
                attr.``class`` "flex-container-2"
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "host"
                    attr.min "6"
                    attr.max "25"
                    bind.input.int model.host (fun a -> dispatch (SetHost a))
                }
                label{
                    attr.``class`` "label"
                    attr.id "host"
                    attr.``type`` "text"
                    $" {model.host * 4}%%"
                }
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "host"
                    bind.input.string model.lblh (fun a -> dispatch (LblHost a))
                }
            }
            div{
                attr.``class`` "flex-container-2"
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls1"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls1 (fun b -> dispatch (SetCls1 b))
                }
                label{
                    attr.``class`` "label"
                    attr.id "cls1"
                    attr.``type`` "text"
                    $" {model.cls1 * 4}%%"
                }
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls1"
                    bind.input.string model.lbl1 (fun a -> dispatch (LblCls1 a))
                }
            }
            div{
                attr.``class`` "flex-container-2"
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls2"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls2 (fun c -> dispatch (SetCls2 c))
                }
                label{
                    attr.``class`` "label"
                    attr.id "cls2"
                    attr.``type`` "text"
                    $" {model.cls2 * 4}%%"
                }
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls2"
                    bind.input.string model.lbl2 (fun a -> dispatch (LblCls2 a))
                }
            }
            div{
                attr.``class`` "flex-container-2"
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls3"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls3 (fun d -> dispatch (SetCls3 d))
                }
                label{
                    attr.``class`` "label"
                    attr.id "cls3"
                    attr.``type`` "text"
                    $" {model.cls3 * 4}%%"
                }
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls3"
                    bind.input.string model.lbl3 (fun a -> dispatch (LblCls3 a))
                }
            }
            div{
                attr.``class`` "flex-container-2"
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls4"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls4 (fun e -> dispatch (SetCls4 e))
                }
                label{
                    attr.``class`` "label"
                    attr.id "cls4"
                    attr.``type`` "text"
                    $" {model.cls4 * 4}%%"
                }
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls4"
                    bind.input.string model.lbl4 (fun a -> dispatch (LblCls4 a))
                }
            }
            div{
                attr.``class`` "flex-container-2"
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls5"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls5 (fun e -> dispatch (SetCls5 e))
                }
                label{
                    attr.``class`` "label"
                    attr.id "cls5"
                    attr.``type`` "text"
                    $" {model.cls5 * 4}%%"
                }
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls5"
                    bind.input.string model.lbl5 (fun a -> dispatch (LblCls5 a))
                }
            }
            div{
                attr.``class`` "flex-container-2"
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls6"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls6 (fun g -> dispatch (SetCls6 g))
                }
                label{
                    attr.``class`` "label"
                    attr.id "cls6"
                    attr.``type`` "text"
                    $"{model.cls6 * 4}%%"
                }
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls6"
                    bind.input.string model.lbl6 (fun a -> dispatch (LblCls6 a))
                }
            }   
            }
        let (loc,wdt,hgt) = cls ([|model.host;model.cls1;model.cls2;model.cls3;model.cls4;model.cls5;model.cls6|])
        let lbls = [|model.lblh;model.lbl1;model.lbl2;model.lbl3;model.lbl4;model.lbl5;model.lbl6|]
        let clrs = [|"#ececce";"#bccfd3";"#867869";"#687b7f";"#c6bdb4";"#3496a3";"#eabdb5"|]
        div{
            attr.``class`` "center"
            cluster (Array.zip3 loc lbls clrs) wdt hgt
            }
        footer{
            a{
                attr.id "footer"
                attr.href "https://twitter.com/_hywe_"
                attr.target "blank"
                img{
                    attr.width "30"
                    attr.height "30"
                    attr.align "right"
                    attr.src "https://vykrum.github.io/Hywe/X.svg"
                    
                    }
            }
            
        }
    }

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view