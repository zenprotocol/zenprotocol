#light "off"
module FStar.Pervasives

open Prims

module Native =
    begin
    type 'Aa option =
    | None
    | Some of 'Aa

    let uu___is_None = function None -> true | _ -> false
    let uu___is_Some = function Some _ -> true | _ -> false
    let __proj__Some__item__v = function Some x -> x | _ -> assert false

    type ('a,'b) tuple2 = 'a * 'b

    let fst = Microsoft.FSharp.Core.Operators.fst
    let snd = Microsoft.FSharp.Core.Operators.snd

    let __proj__Mktuple2__1 = fst
    let __proj__Mktuple2__2 = snd

    type ('a,'b,'c) tuple3 =
     'a* 'b* 'c

    let uu___is_Mktuple3 projectee = true
    let __proj__Mktuple3__item___1 projectee =
      match projectee with | (_1,_2,_3) -> _1
    let __proj__Mktuple3__item___2 projectee =
      match projectee with | (_1,_2,_3) -> _2
    let __proj__Mktuple3__item___3 projectee =
      match projectee with | (_1,_2,_3) -> _3
    type ('a,'b,'c,'d) tuple4 =
     'a* 'b* 'c* 'd
    let uu___is_Mktuple4 projectee = true
    let __proj__Mktuple4__item___1 projectee =
      match projectee with | (_1,_2,_3,_4) -> _1
    let __proj__Mktuple4__item___2 projectee =
      match projectee with | (_1,_2,_3,_4) -> _2
    let __proj__Mktuple4__item___3 projectee =
      match projectee with | (_1,_2,_3,_4) -> _3
    let __proj__Mktuple4__item___4 projectee =
      match projectee with | (_1,_2,_3,_4) -> _4
    type ('a,'b,'c,'d,'e) tuple5 =
     'a* 'b* 'c* 'd* 'e
    let uu___is_Mktuple5 projectee = true
    let __proj__Mktuple5__item___1 projectee =
      match projectee with | (_1,_2,_3,_4,_5) -> _1
    let __proj__Mktuple5__item___2 projectee =
      match projectee with | (_1,_2,_3,_4,_5) -> _2
    let __proj__Mktuple5__item___3 projectee =
      match projectee with | (_1,_2,_3,_4,_5) -> _3
    let __proj__Mktuple5__item___4 projectee =
      match projectee with | (_1,_2,_3,_4,_5) -> _4
    let __proj__Mktuple5__item___5 projectee =
      match projectee with | (_1,_2,_3,_4,_5) -> _5
    type ('a,'b,'c,'d,'e,'f) tuple6 =
     'a* 'b* 'c* 'd* 'e* 'f
    let uu___is_Mktuple6 projectee = true
    let __proj__Mktuple6__item___1 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6) -> _1
    let __proj__Mktuple6__item___2 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6) -> _2
    let __proj__Mktuple6__item___3 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6) -> _3
    let __proj__Mktuple6__item___4 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6) -> _4
    let __proj__Mktuple6__item___5 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6) -> _5
    let __proj__Mktuple6__item___6 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6) -> _6
    type ('a,'b,'c,'d,'e,'f,'g) tuple7 =
     'a* 'b* 'c* 'd* 'e* 'f* 'g
    let uu___is_Mktuple7 projectee = true
    let __proj__Mktuple7__item___1 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7) -> _1
    let __proj__Mktuple7__item___2 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7) -> _2
    let __proj__Mktuple7__item___3 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7) -> _3
    let __proj__Mktuple7__item___4 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7) -> _4
    let __proj__Mktuple7__item___5 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7) -> _5
    let __proj__Mktuple7__item___6 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7) -> _6
    let __proj__Mktuple7__item___7 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7) -> _7
    type ('a,'b,'c,'d,'e,'f,'g,'h) tuple8 =
     'a* 'b* 'c* 'd* 'e* 'f* 'g* 'h
    let uu___is_Mktuple8 projectee = true
    let __proj__Mktuple8__item___1 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7,_8) -> _1
    let __proj__Mktuple8__item___2 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7,_8) -> _2
    let __proj__Mktuple8__item___3 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7,_8) -> _3
    let __proj__Mktuple8__item___4 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7,_8) -> _4
    let __proj__Mktuple8__item___5 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7,_8) -> _5
    let __proj__Mktuple8__item___6 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7,_8) -> _6
    let __proj__Mktuple8__item___7 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7,_8) -> _7
    let __proj__Mktuple8__item___8 projectee =
      match projectee with | (_1,_2,_3,_4,_5,_6,_7,_8) -> _8
    end


type 'Aa result =
| V of 'Aa
| E of Prims.exn
| Err of Prims.string



let uu___is_V = (fun ( projectee  :  'Aa result ) -> (match (projectee) with
| V (v) -> begin
true
end
| uu____552 -> begin
false
end))


let __proj__V__item__v = (fun ( projectee  :  'Aa result ) -> (match (projectee) with
| V (v) -> begin
v
end))


let uu___is_E = (fun ( projectee  :  'Aa result ) -> (match (projectee) with
| E (e) -> begin
true
end
| uu____594 -> begin
false
end))


let __proj__E__item__e = (fun ( projectee  :  'Aa result ) -> (match (projectee) with
| E (e) -> begin
e
end))


let uu___is_Err = (fun ( projectee  :  'Aa result ) -> (match (projectee) with
| Err (msg) -> begin
true
end
| uu____636 -> begin
false
end))


let __proj__Err__item__msg = (fun ( projectee  :  'Aa result ) -> (match (projectee) with
| Err (msg) -> begin
msg
end))

type 'Aa inversion = Inversion of Prims.unit

let allow_inversion = ()

let invertOption = (fun ( uu____1569  :  Prims.unit ) -> ())

type ('a, 'b) either =
| Inl of 'a
| Inr of 'b


let uu___is_Inl = (fun ( projectee  :  ('a, 'b) either ) -> (match (projectee) with
| Inl (v) -> begin
true
end
| uu____1620 -> begin
false
end))


let __proj__Inl__item__v = (fun ( projectee  :  ('a, 'b) either ) -> (match (projectee) with
| Inl (v) -> begin
v
end))


let uu___is_Inr = (fun ( projectee  :  ('a, 'b) either ) -> (match (projectee) with
| Inr (v) -> begin
true
end
| uu____1680 -> begin
false
end))


let __proj__Inr__item__v = (fun ( projectee  :  ('a, 'b) either ) -> (match (projectee) with
| Inr (v) -> begin
v
end))


let dfst = (fun ( t  :  ('Aa, 'Ab) Prims.dtuple2 ) -> (Prims.__proj__Mkdtuple2__item___1 t))


let dsnd = (fun ( t  :  ('Aa, 'Ab) Prims.dtuple2 ) -> (Prims.__proj__Mkdtuple2__item___2 t))

type ('Aa, 'Ab, 'Ac) dtuple3 =
| Mkdtuple3 of 'Aa * 'Ab * 'Ac


let uu___is_Mkdtuple3 = (fun ( projectee  :  ('Aa, 'Ab, 'Ac) dtuple3 ) -> true)


let __proj__Mkdtuple3__item___1 = (fun ( projectee  :  ('Aa, 'Ab, 'Ac) dtuple3 ) -> (match (projectee) with
| Mkdtuple3 (_1, _2, _3) -> begin
_1
end))


let __proj__Mkdtuple3__item___2 = (fun ( projectee  :  ('Aa, 'Ab, 'Ac) dtuple3 ) -> (match (projectee) with
| Mkdtuple3 (_1, _2, _3) -> begin
_2
end))


let __proj__Mkdtuple3__item___3 = (fun ( projectee  :  ('Aa, 'Ab, 'Ac) dtuple3 ) -> (match (projectee) with
| Mkdtuple3 (_1, _2, _3) -> begin
_3
end))

type ('Aa, 'Ab, 'Ac, 'Ad) dtuple4 =
| Mkdtuple4 of 'Aa * 'Ab * 'Ac * 'Ad


let uu___is_Mkdtuple4 = (fun ( projectee  :  ('Aa, 'Ab, 'Ac, 'Ad) dtuple4 ) -> true)


let __proj__Mkdtuple4__item___1 = (fun ( projectee  :  ('Aa, 'Ab, 'Ac, 'Ad) dtuple4 ) -> (match (projectee) with
| Mkdtuple4 (_1, _2, _3, _4) -> begin
_1
end))


let __proj__Mkdtuple4__item___2 = (fun ( projectee  :  ('Aa, 'Ab, 'Ac, 'Ad) dtuple4 ) -> (match (projectee) with
| Mkdtuple4 (_1, _2, _3, _4) -> begin
_2
end))


let __proj__Mkdtuple4__item___3 = (fun ( projectee  :  ('Aa, 'Ab, 'Ac, 'Ad) dtuple4 ) -> (match (projectee) with
| Mkdtuple4 (_1, _2, _3, _4) -> begin
_3
end))


let __proj__Mkdtuple4__item___4 = (fun ( projectee  :  ('Aa, 'Ab, 'Ac, 'Ad) dtuple4 ) -> (match (projectee) with
| Mkdtuple4 (_1, _2, _3, _4) -> begin
_4
end))


let ignore = (fun ( x  :  'Aa ) -> ())


let rec false_elim = (fun ( u  :  Prims.unit ) -> (false_elim ()))

type __internal_ocaml_attributes =
| PpxDerivingShow
| PpxDerivingShowConstant of Prims.string


let uu___is_PpxDerivingShow : __internal_ocaml_attributes  ->  Prims.bool = (fun ( projectee  :  __internal_ocaml_attributes ) -> (match (projectee) with
| PpxDerivingShow -> begin
true
end
| uu____2494 -> begin
false
end))


let uu___is_PpxDerivingShowConstant : __internal_ocaml_attributes  ->  Prims.bool = (fun ( projectee  :  __internal_ocaml_attributes ) -> (match (projectee) with
| PpxDerivingShowConstant (_0) -> begin
true
end
| uu____2503 -> begin
false
end))


let __proj__PpxDerivingShowConstant__item___0 : __internal_ocaml_attributes  ->  Prims.string = (fun ( projectee  :  __internal_ocaml_attributes ) -> (match (projectee) with
| PpxDerivingShowConstant (_0) -> begin
_0
end))
