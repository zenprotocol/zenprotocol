#light "off"
module Zen.Cost
open Prims
open FStar.Pervasives
type ('Aa, 'An) cost =
| C of Lazy<'Aa>


let uu___is_C = (fun ( n  :  Prims.nat ) ( projectee  :  ('Aa, Prims.unit) cost ) -> true)


let __proj__C__item__inj = (fun ( n  :  Prims.nat ) ( projectee  :  ('Aa, Prims.unit) cost ) -> (match (projectee) with
| C (inj) -> begin
inj.Force()
end))


let ret = (fun ( x  :  'Aa ) -> C (lazy (x)))


let bind = (fun ( n  :  Prims.nat ) ( m  :  Prims.nat ) ( uu____117  :  ('Aa, Prims.unit) cost ) ( f  :  'Aa  ->  ('Ab, Prims.unit) cost ) -> (match (uu____117) with
| C (x) -> begin
C (lazy (__proj__C__item__inj m (f (x.Force()))))
end))


let inc = (fun ( n  :  Prims.nat ) ( uu____164  :  ('Aa, Prims.unit) cost ) ( k  :  Prims.nat ) -> (match (uu____164) with
| C (x) -> begin
C (x)
end))


let bind2 = (fun ( uu___153_383  :  Prims.nat ) ( uu___154_384  :  Prims.nat ) ( uu___155_385  :  Prims.nat ) ( mx  :  ('Auu___150_320, Prims.unit) cost ) ( my  :  ('Auu___151_321, Prims.unit) cost ) ( f  :  'Auu___150_320  ->  'Auu___151_321  ->  ('Auu___152_322, Prims.unit) cost ) -> (bind (uu___154_384 + uu___155_385) uu___153_383 mx (fun ( x  :  'Auu___150_320 ) -> (bind uu___155_385 uu___154_384 my (fun ( y  :  'Auu___151_321 ) -> (f x y))))))


let bind3 = (fun ( uu___160_576  :  Prims.nat ) ( uu___161_577  :  Prims.nat ) ( uu___162_578  :  Prims.nat ) ( uu___163_579  :  Prims.nat ) ( mx  :  ('Auu___156_493, Prims.unit) cost ) ( my  :  ('Auu___157_494, Prims.unit) cost ) ( mz  :  ('Auu___158_495, Prims.unit) cost ) ( f  :  'Auu___156_493  ->  'Auu___157_494  ->  'Auu___158_495  ->  ('Auu___159_496, Prims.unit) cost ) -> (bind (uu___161_577 + (uu___162_578 + uu___163_579)) uu___160_576 mx (fun ( x  :  'Auu___156_493 ) -> (bind (uu___162_578 + uu___163_579) uu___161_577 my (fun ( y  :  'Auu___157_494 ) -> (bind uu___163_579 uu___162_578 mz (fun ( z  :  'Auu___158_495 ) -> (f x y z))))))))


let join = (fun ( uu___165_729  :  Prims.nat ) ( uu___166_730  :  Prims.nat ) ( x  :  (('Auu___164_693, Prims.unit) cost, Prims.unit) cost ) -> (bind uu___166_730 uu___165_729 x (fun ( z  :  ('Auu___164_693, Prims.unit) cost ) -> z)))


let liftM = (fun ( uu___169_829  :  Prims.nat ) ( f  :  'Auu___167_797  ->  'Auu___168_798 ) ( mx  :  ('Auu___167_797, Prims.unit) cost ) -> (bind (Prims.parse_int "0") uu___169_829 mx (fun ( x  :  'Auu___167_797 ) -> (ret (f x)))))


let ap = (fun ( uu___172_917  :  Prims.nat ) ( uu___173_918  :  Prims.nat ) ( mf  :  ('Auu___170_874  ->  'Auu___171_875, Prims.unit) cost ) ( mx  :  ('Auu___170_874, Prims.unit) cost ) -> (bind uu___172_917 uu___173_918 mf (fun ( f  :  ('Auu___170_874  ->  'Auu___171_875) ) -> (liftM uu___172_917 f mx))))


let liftM2 = (fun ( uu___177_1041  :  Prims.nat ) ( uu___178_1042  :  Prims.nat ) ( f  :  'Auu___174_992  ->  'Auu___175_993  ->  'Auu___176_994 ) ( x  :  ('Auu___174_992, Prims.unit) cost ) -> (ap uu___178_1042 uu___177_1041 (liftM uu___177_1041 f x)))


let liftM3 = (fun ( uu___183_1186  :  Prims.nat ) ( uu___184_1187  :  Prims.nat ) ( uu___185_1188  :  Prims.nat ) ( f  :  'Auu___179_1116  ->  'Auu___180_1117  ->  'Auu___181_1118  ->  'Auu___182_1119 ) ( mx  :  ('Auu___179_1116, Prims.unit) cost ) ( my  :  ('Auu___180_1117, Prims.unit) cost ) ( mz  :  ('Auu___181_1118, Prims.unit) cost ) -> (ap uu___185_1188 (uu___183_1186 + uu___184_1187) (ap uu___184_1187 uu___183_1186 (liftM uu___183_1186 f mx) my) mz))


let op_Greater_Equals_Greater = (fun ( uu___189_1338  :  Prims.nat ) ( uu___190_1339  :  Prims.nat ) ( f  :  'Auu___186_1288  ->  ('Auu___187_1289, Prims.unit) cost ) ( g  :  'Auu___187_1289  ->  ('Auu___188_1290, Prims.unit) cost ) ( x  :  'Auu___186_1288 ) -> (bind uu___189_1338 uu___190_1339 (f x) g))


let op_Less_Equals_Less = (fun ( uu___194_1451  :  Prims.nat ) ( uu___195_1452  :  Prims.nat ) ( f  :  'Auu___192_1403  ->  ('Auu___193_1404, Prims.unit) cost ) ( g  :  'Auu___191_1402  ->  ('Auu___192_1403, Prims.unit) cost ) -> (op_Greater_Equals_Greater uu___195_1452 uu___194_1451 g f))


let eq = (fun ( uu___197_1545  :  Prims.nat ) ( uu___198_1546  :  Prims.nat ) -> (liftM2 uu___197_1545 uu___198_1546 Prims.op_Equality))


let incRet = (fun ( n  :  Prims.nat ) ( x  :  'Auu___199_1572 ) -> (inc (Prims.parse_int "0") (ret x) n))


let autoInc = (fun ( m  :  Prims.nat ) ( n  :  Prims.nat ) ( mx  :  ('Auu___200_1612, Prims.unit) cost ) -> (inc m mx (n - m)))


let autoRet = (fun ( uu___202_1703  :  Prims.nat ) ( x  :  'Auu___201_1686 ) -> (autoInc (Prims.parse_int "0") uu___202_1703 (ret x)))


let retf = (fun ( f  :  'Auu___203_1724  ->  'Auu___204_1725 ) ( x  :  'Auu___203_1724 ) -> (ret (f x)))


let op_Tilde_Bang = (fun ( uu____1760  :  Prims.unit ) -> ret)


let op_Plus_Bang : Prims.nat  ->  Prims.unit  ->  Prims.nat  ->  (obj, Prims.unit) cost  ->  (obj, Prims.unit) cost = (fun ( uu____1799  :  Prims.nat ) ( uu____1800  :  Prims.unit ) ( k  :  Prims.nat ) ( x  :  (obj, Prims.unit) cost ) -> (inc uu____1799 x k))


let op_Plus_Tilde_Bang = (fun ( uu____1849  :  Prims.unit ) -> incRet)


let op_Tilde_Plus_Bang = (fun ( uu____1876  :  Prims.unit ) -> incRet)


let op_Greater_Greater_Equals : Prims.nat  ->  Prims.unit  ->  Prims.nat  ->  Prims.unit  ->  (obj, Prims.unit) cost  ->  (obj  ->  (obj, Prims.unit) cost)  ->  (obj, Prims.unit) cost = (fun ( uu____1933  :  Prims.nat ) ( uu____1934  :  Prims.unit ) ( uu____1935  :  Prims.nat ) ( uu____1936  :  Prims.unit ) -> (bind uu____1933 uu____1935))


let op_Equals_Less_Less : Prims.nat  ->  Prims.nat  ->  Prims.unit  ->  Prims.unit  ->  (obj  ->  (obj, Prims.unit) cost)  ->  (obj, Prims.unit) cost  ->  (obj, Prims.unit) cost = (fun ( uu____2008  :  Prims.nat ) ( uu____2009  :  Prims.nat ) ( uu____2010  :  Prims.unit ) ( uu____2011  :  Prims.unit ) ( f  :  obj  ->  (obj, Prims.unit) cost ) ( x  :  (obj, Prims.unit) cost ) -> (bind uu____2009 uu____2008 x f))


let op_Less_Dollar_Greater : Prims.nat  ->  Prims.unit  ->  Prims.unit  ->  (obj  ->  obj)  ->  (obj, Prims.unit) cost  ->  (obj, Prims.unit) cost = (fun ( uu____2088  :  Prims.nat ) ( uu____2089  :  Prims.unit ) ( uu____2090  :  Prims.unit ) -> (liftM uu____2088))


let op_Less_Dollar_Dollar_Greater : Prims.nat  ->  Prims.nat  ->  Prims.unit  ->  Prims.unit  ->  Prims.unit  ->  (obj  ->  obj  ->  obj)  ->  (obj, Prims.unit) cost  ->  (obj, Prims.unit) cost  ->  (obj, Prims.unit) cost = (fun ( uu____2165  :  Prims.nat ) ( uu____2166  :  Prims.nat ) ( uu____2167  :  Prims.unit ) ( uu____2168  :  Prims.unit ) ( uu____2169  :  Prims.unit ) -> (liftM2 uu____2166 uu____2165))


let op_Less_Dollar_Dollar_Dollar_Greater : Prims.nat  ->  Prims.nat  ->  Prims.nat  ->  Prims.unit  ->  Prims.unit  ->  Prims.unit  ->  Prims.unit  ->  (obj  ->  obj  ->  obj  ->  obj)  ->  (obj, Prims.unit) cost  ->  (obj, Prims.unit) cost  ->  (obj, Prims.unit) cost  ->  (obj, Prims.unit) cost = (fun ( uu____2276  :  Prims.nat ) ( uu____2277  :  Prims.nat ) ( uu____2278  :  Prims.nat ) ( uu____2279  :  Prims.unit ) ( uu____2280  :  Prims.unit ) ( uu____2281  :  Prims.unit ) ( uu____2282  :  Prims.unit ) -> (liftM3 uu____2278 uu____2277 uu____2276))


let op_Less_Star_Greater : Prims.nat  ->  Prims.nat  ->  Prims.unit  ->  Prims.unit  ->  (obj  ->  obj, Prims.unit) cost  ->  (obj, Prims.unit) cost  ->  (obj, Prims.unit) cost = (fun ( uu____2357  :  Prims.nat ) ( uu____2358  :  Prims.nat ) ( uu____2359  :  Prims.unit ) ( uu____2360  :  Prims.unit ) -> (ap uu____2357 uu____2358))


let op_Star_Greater : Prims.nat  ->  Prims.unit  ->  Prims.nat  ->  Prims.unit  ->  (obj, Prims.unit) cost  ->  (obj  ->  obj, Prims.unit) cost  ->  (obj, Prims.unit) cost = (fun ( uu____2432  :  Prims.nat ) ( uu____2433  :  Prims.unit ) ( uu____2434  :  Prims.nat ) ( uu____2435  :  Prims.unit ) ( x  :  (obj, Prims.unit) cost ) ( f  :  (obj  ->  obj, Prims.unit) cost ) -> (ap uu____2434 uu____2432 f x))


let op_Less_Equals_Greater : Prims.nat  ->  Prims.nat  ->  Prims.unit  ->  (obj, Prims.unit) cost  ->  (obj, Prims.unit) cost  ->  (Prims.bool, Prims.unit) cost = (fun ( uu____2519  :  Prims.nat ) ( uu____2520  :  Prims.nat ) ( uu____2521  :  Prims.unit ) -> (eq uu____2520 uu____2519))


let op_Dollar_Greater = (fun ( uu____2591  :  Prims.nat ) ( uu____2592  :  Prims.unit ) ( x  :  (obj, Prims.unit) cost ) ( f  :  obj  ->  'Auu____2557 ) -> (liftM uu____2591 f x))


let op_Dollar_Dollar_Greater = (fun ( uu___208_2689  :  Prims.nat ) ( uu___209_2690  :  Prims.nat ) ( uu____2691  :  (('Auu___205_2638, Prims.unit) cost * ('Auu___206_2639, Prims.unit) cost) ) ( f  :  'Auu___205_2638  ->  'Auu___206_2639  ->  'Auu___207_2640 ) -> (match (uu____2691) with
| (mx, my) -> begin
(liftM2 uu___208_2689 uu___209_2690 f mx my)
end))


let op_Dollar_Dollar_Dollar_Greater = (fun ( uu___214_2868  :  Prims.nat ) ( uu___215_2869  :  Prims.nat ) ( uu___216_2870  :  Prims.nat ) ( uu____2871  :  (('Auu___210_2798, Prims.unit) cost * ('Auu___211_2799, Prims.unit) cost * ('Auu___212_2800, Prims.unit) cost) ) ( f  :  'Auu___210_2798  ->  'Auu___211_2799  ->  'Auu___212_2800  ->  'Auu___213_2801 ) -> (match (uu____2871) with
| (mx, my, mz) -> begin
(liftM3 uu___214_2868 uu___215_2869 uu___216_2870 f mx my mz)
end))


let op_Less_Tilde_Greater = (fun ( uu___219_3029  :  Prims.nat ) ( mf  :  ('Auu___217_2997  ->  'Auu___218_2998, Prims.unit) cost ) ( x  :  'Auu___217_2997 ) -> (ap (Prims.parse_int "0") uu___219_3029 mf (ret x)))


let left_id = (fun ( uu___222_3090  :  Prims.nat ) ( uu____3091  :  'Auu___220_3066 ) ( uu____3092  :  'Auu___220_3066  ->  ('Auu___221_3067, Prims.unit) cost ) -> ())


let right_id = (fun ( uu___224_3133  :  Prims.nat ) ( uu____3134  :  ('Auu___223_3116, Prims.unit) cost ) -> ())


let assoc = (fun ( uu___228_3234  :  Prims.nat ) ( uu___229_3235  :  Prims.nat ) ( uu___230_3236  :  Prims.nat ) ( uu____3237  :  ('Auu___225_3176, Prims.unit) cost ) ( uu____3238  :  'Auu___225_3176  ->  ('Auu___226_3177, Prims.unit) cost ) ( uu____3239  :  'Auu___226_3177  ->  ('Auu___227_3178, Prims.unit) cost ) -> ())


let force_ret = (fun ( x  :  'Auu___234_3340 ) -> ())


let force_inc = (fun ( uu___236_3381  :  Prims.nat ) ( uu____3382  :  Prims.nat ) ( uu____3383  :  ('Auu___235_3359, Prims.unit) cost ) -> ())


let force_retInc = (fun ( uu____3416  :  Prims.nat ) ( uu____3417  :  'Auu___237_3407 ) -> ())


let force_bind = (fun ( uu___240_3478  :  Prims.nat ) ( uu___241_3479  :  Prims.nat ) ( uu____3480  :  ('Auu___238_3441, Prims.unit) cost ) ( uu____3481  :  'Auu___238_3441  ->  ('Auu___239_3442, Prims.unit) cost ) -> ())


let force_liftM = (fun ( uu___244_3546  :  Prims.nat ) ( uu____3547  :  'Auu___242_3522  ->  'Auu___243_3523 ) ( uu____3548  :  ('Auu___242_3522, Prims.unit) cost ) -> ())


let force_ap = (fun ( uu___247_3620  :  Prims.nat ) ( uu___248_3621  :  Prims.nat ) ( uu____3622  :  ('Auu___245_3582  ->  'Auu___246_3583, Prims.unit) cost ) ( uu____3623  :  ('Auu___245_3582, Prims.unit) cost ) -> ())
