%%%
%%% File    : bfile.erl
%%% Author  : Claes Wikstrom <klacke@kaja.klacke.net>
%%% Purpose : Interface to stdio buffered FILE io 
%%% Created : 22 Nov 1999 by Claes Wikstrom <klacke@kaja.klacke.net>
%%%----------------------------------------------------------------------

-module(bfile).
-vsn("$Revision: 1.1 $ ").
-author('klacke@kaja.klacke.net').

-export([
	 load_driver/0,
	 fopen/2,
	 fclose/1,
	 fread/2,
	 fwrite/2,
	 feof/1,
	 ferror/1,
	 set_linebuf_size/2,
	 fseek/3,
	 ftell/1,
	 ftruncate/1,
	 fflush/1,
	 frewind/1,
	 fgetc/1,
	 fungetc/2,
	 fgets/1,
	 gets/1,
	 pwrite/3,
	 pread/3
	]).


%% Opcodes
-define(OPEN,             $o).
-define(CLOSE,            $c).
-define(READ,             $r).
-define(WRITE,            $w).
-define(SEEK,             $s).
-define(TELL,             $t).
-define(TRUNCATE,         $T).
-define(FLUSH,            $f).
-define(OEOF,             $e).
-define(ERROR,            $E).
-define(GETC,             $g).
-define(GETS,             $G).
-define(GETS2,            $2).
-define(SET_LINEBUF_SIZE, $S).
-define(UNGETC,           $u).


%% ret codes
-define(VALUE, $v).
-define(FLINE,  $L).
-define(OK,    $o).
-define(I32,   $O).
-define(NOLINE,$N).
-define(FERROR, $E).
-define(REOF,   $x).

-define(int32(X), 
        [((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
         ((X) bsr 8) band 16#ff, (X) band 16#ff]).
%% Bytes to unsigned
-define(u32(X3,X2,X1,X0), 
        (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).
%% Bytes to signed
-define(i32(X3,X2,X1,X0),
        (?u32(X3,X2,X1,X0) - 
         (if (X3) > 127 -> 16#100000000; true -> 0 end))).

load_driver() ->
    Dir = filename:join([filename:dirname(code:which(bfile)),"..", "priv"]),
    erl_ddll:load_driver(Dir, "FILE_drv").


%% Flags = "r" | "w" | ... etc, see fopen(3)
%% Ret: {ok, Fd} | {error, Reason}

fopen(Fname, Flags) ->  
    P = open_port({spawn, 'FILE_drv'}, [binary]),
    Res = erlang_port_control(P, ?OPEN, [Fname, 0, Flags, 0]),
    case decode(Res) of
	ok ->
	    {ok, {bfile, P}};
	Err ->
	    unlink(P),
	    exit(P, die),
	    Err
    end.

%% void()
fclose({bfile, Fd}) ->
    unlink(Fd),
    catch erlang:port_close(Fd).

%% {ok, #Bin} | {error, Reason} | eof
fread({bfile, Fd}, Sz) ->
    Res = erlang_port_control(Fd, ?READ, ?int32(Sz)),
    decode(Res).

%% ok | {error, Reason}
fwrite({bfile, Fd}, IoList) ->
    Res = erlang_port_control(Fd, ?WRITE, IoList),
    decode(Res).

%% ok | {error, Reason}
pwrite(BFd, Pos, IoList) ->
    case fseek(BFd, Pos, seek_set) of
	ok ->
	    fwrite(BFd, IoList);
	Error ->
	    Error
    end.

%% {ok, #Bin} | {error, Reason} | eof
pread(BFd, Pos, Sz) ->
    case fseek(BFd, Pos, seek_set) of
	ok ->
	    fread(BFd, Sz);
	Error ->
	    Error
    end.

%% bool
feof({bfile, Fd}) ->
    Res = erlang_port_control(Fd, ?OEOF, []),
    bool(decode(Res)).

%% bool
ferror({bfile, Fd}) ->
    Res = erlang_port_control(Fd, ?ERROR, []),
    bool(decode(Res)).


%% void()
set_linebuf_size({bfile, Fd}, Sz) ->
    Res = erlang_port_control(Fd, ?SET_LINEBUF_SIZE, ?int32(Sz)),
    decode(Res).

%% Whence  == seek_set | seek_cur || seek_end
%% ok | {error, Reason}
fseek({bfile, Fd}, Offs, Whence) ->
    Res = erlang_port_control(Fd, ?SEEK, [?int32(Offs), whence_enc(Whence)]),
    decode(Res).

%% {ok, Int} | {error, Reason}
ftell({bfile, Fd}) ->
    Res = erlang_port_control(Fd, ?TELL, []),
    decode(Res).

%% ok | {error, Reason}
ftruncate({bfile, Fd}) ->
    Res = erlang_port_control(Fd, ?TRUNCATE, []),
    decode(Res).

%% ok | {error, Reason}
fflush({bfile, Fd}) ->
    Res = erlang_port_control(Fd, ?FLUSH, []),
    decode(Res).

%% ok | {error, Reason}
frewind(BFd) ->
    fseek(BFd, 0, seek_set).

%% {ok, Char} | {error, Reason} | eof
fgetc({bfile, Fd}) ->
    Res = erlang_port_control(Fd, ?GETC, []),
    decode(Res).

%% ok | {error, Reason}
fungetc({bfile, Fd}, Char) ->
    Res = erlang_port_control(Fd, ?UNGETC, [Char]),
    decode(Res).

%% {line, #Bin} | {noline, #Bin} | {error, Reason} | eof
%% including newline
fgets({bfile, Fd}) ->
    Res = erlang_port_control(Fd, ?GETS, []),
    decode(Res).

%% {line, #Bin} | {noline, #Bin} | {error, Reason} | eof
%% not including newline
gets({bfile, Fd}) ->
    Res = erlang_port_control(Fd, ?GETS2, []),
    decode(Res).


whence_enc(seek_set) ->
    1;
whence_enc(seek_cur) ->
    2;
whence_enc(seek_end) ->
    3.


bool({ok, 1}) ->
    true;
bool({ok, 0}) ->
    false.


decode(Res)  ->
    case Res of
	<<?VALUE, Bin/binary>> ->
	    {ok, Bin};
	<<?FLINE, Bin/binary>> ->
	    {line, Bin};
	<<?OK>> ->
	    ok;
	<<?I32, X1, X2, X3, X4>> ->
	    {ok, ?i32(X1, X2, X3, X4)};
	<<?NOLINE, Bin/binary>> ->
	    {noline, Bin};
	<<?FERROR, Err/binary>> ->
	    {error, list_to_atom(binary_to_list(Err))};
	<<?REOF>> ->
	    eof
    end.

erlang_port_control(P, C, Data) ->
    erlang:port_control(P, C, Data).

