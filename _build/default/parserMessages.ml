
(* This file was auto-generated based on "parserMessages.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "At the beginning, an expression is expected.\n"
    | 2 ->
        "After an opening parenthesis, an expression is expected.\n"
    | 4 ->
        "I have read an opening parenthesis,\nfollowed with the expression '$0'.\nI am now expecting either an arithmetic operator or a closing parenthesis.\n"
    | 16 ->
        "I have read the expression '$0'.\nI am now expecting either an arithmetic operator or the end of the input.\n"
    | 10 | 12 | 8 | 5 | 1 ->
        "After an arithmetic operator, an expression is expected.\n"
    | _ ->
        raise Not_found
