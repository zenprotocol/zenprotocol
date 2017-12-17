#light "off"
module FStar.Pervasives
open FSharp.Compatibility.OCaml.Pervasives
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

    let fst = fst
    let snd = snd

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
| OK of 'Aa
| EX of Prims.exn
| ERR of Prims.string



let uu___is_OK = (fun ( projectee  :  'Aa result ) -> (match (projectee) with
| OK (v) -> begin
true
end
| uu____552 -> begin
false
end))


let __proj__OK__item__v = (fun ( projectee  :  'Aa result ) -> (match (projectee) with
| OK (v) -> begin
v
end))


let uu___is_EX = (fun ( projectee  :  'Aa result ) -> (match (projectee) with
| EX (e) -> begin
true
end
| uu____594 -> begin
false
end))


let __proj__EX__item__e = (fun ( projectee  :  'Aa result ) -> (match (projectee) with
| EX (e) -> begin
e
end))


let uu___is_ERR = (fun ( projectee  :  'Aa result ) -> (match (projectee) with
| ERR (msg) -> begin
true
end
| uu____636 -> begin
false
end))


let __proj__ERR__item__msg = (fun ( projectee  :  'Aa result ) -> (match (projectee) with
| ERR (msg) -> begin
msg
end))

type ex_pre =
obj


type 'Aa ex_post =
'Aa result  ->  obj


type 'Aa ex_wp =
Prims.unit  ->  ex_pre


type ('Aa, 'Ax, 'Ap) ex_return =
'Ap


type ('Ar1, 'Aa, 'Ab, 'Awp1, 'Awp2, 'Ap) ex_bind_wp =
Prims.unit


type ('Aa, 'Awp, 'Apost) ex_ite_wp =
Prims.unit


type ('Aa, 'Ap, 'Awp_then, 'Awp_else, 'Apost) ex_if_then_else =
Prims.unit


type ('Aa, 'Awp1, 'Awp2) ex_stronger =
Prims.unit


type ('Aa, 'Ab, 'Awp, 'Ap) ex_close_wp =
Prims.unit


type ('Aa, 'Aq, 'Awp, 'Ap) ex_assert_p =
Prims.unit


type ('Aa, 'Aq, 'Awp, 'Ap) ex_assume_p =
Prims.unit


type ('Aa, 'Ap) ex_null_wp =
Prims.unit


type ('Aa, 'Awp) ex_trivial =
'Awp

type 'Aa inversion = Prims.unit

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
| CInline
| Substitute
| Gc
| Comment of Prims.string


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
