module GeneratedParser (
LexerInfo: sig val prim_token_names : (string, int) Hashtbl.t end
) = struct
let (* A *)token_id_of_65 : int = Hashtbl.find LexerInfo.prim_token_names "A"
let (* b *)token_id_of_98 : int = Hashtbl.find LexerInfo.prim_token_names "b"
let rec parse_B (prim__state, prim__tokens) =
begin
  let _tmp_0_flag : (bool) ref =
      ref false
  in 
  let _off_0 : int =
      prim__tokens .offset
  in 
  let _tmp_0_result : ast =
      if prim__peekable (prim__tokens 0)
      then begin
           match prim__peek (prim__tokens 0) .idint with
           | (* b *)token_id_of_98 -> begin
               let _off_1 : int =
                   prim__tokens .offset
               in 
               let _slot_0 : ast =
                   prim__match__tk (prim__tokens ((* b *)token_id_of_98))
               in 
               if prim__is__null (_slot_0)
               then prim__null
               else begin
                 let _off_2 : int =
                     prim__tokens .offset
                 in 
                 let _slot_1 : ast =
                     prim__match__tk (prim__tokens ((* A *)token_id_of_65))
                 in 
                 if prim__is__null (_slot_1)
                 then prim__null
                 else begin
                   let _slot_local__1 : (ast * ast) =
                       (_slot_0, _slot_1)
                   in 
                   let _slot_local__2 : ast =
                       prim__mk__ast (B (_slot_local__1))
                   in 
                   _slot_local__2 ;
                   end ;
                 end ;
               end
           | (* A *)token_id_of_65 -> begin
               let _off_1 : int =
                   prim__tokens .offset
               in 
               let _slot_0 : ast =
                   prim__match__tk (prim__tokens ((* A *)token_id_of_65))
               in 
               if prim__is__null (_slot_0)
               then prim__null
               else begin
                 let _slot_local__1 : ast =
                     prim__mk__ast (B (_slot_0))
                 in 
                 _slot_local__1 ;
                 end ;
               end
           | _ ->  begin
             _tmp_0_flag  :=  true ;
             prim__null ;
             end
           end
      else prim__null
  in 
  if prim__is__null (_tmp_0_result) || !_tmp_0_flag
  then _tmp_0_result
  else begin
    prim__reset (prim__tokens _off_0) ;
    prim__null ;
    end ;
  end
end