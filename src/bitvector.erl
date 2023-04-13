%% bitvector
%%
%% MIT No Attribution  
%% Copyright 2023 David J Goehrig <dave@dloh.org>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy 
%% of this software and associated documentation files (the "Software"), to 
%% deal in the Software without restriction, including without limitation the 
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
%% sell copies of the Software, and to permit persons to whom the Software is 
%% furnished to do so.  
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
%% IN THE SOFTWARE.


-module(bitvector).
-author({ "David J Goehrig", "dave@dloh.org"}).
-copyright(<<"© 2023 David J. Goehrig"/utf8>>).
-export([ wrap/2, unwrap/2, same/2, merge/2, mask/2 ]).

set_bit(BitSet, Index) ->
	BitSize = byte_size(BitSet) * 8,
	io:format("Size ~p index ~p~n", [ BitSize, Index ]),
	case Index =< BitSize of
	true ->
		<<A:((Index-1)*1), _B:1, C:((BitSize-Index)*1)>> = BitSet,
		<<A:((Index-1)*1), 1:1, C:((BitSize-Index)*1)>>;
	false ->
		BitSet
	end.

set_sorted(Bits, [H1|T1], [H2|T2], SetSize) ->
	case H1 of
	H2 ->
		Index = SetSize - length(T2),
		NewBits = set_bit(Bits,Index),
		set_sorted(NewBits,T1,T2,SetSize);
	_ ->
		case H1 < H2 of
			true -> set_sorted(Bits, T1,[H2|T2],SetSize);
			false -> set_sorted(Bits,[H1|T1],T2,SetSize)
		end
	end;
set_sorted(Bits,_,_,_) ->
	Bits.


%% Takes a list of targets, and a super set list both sorted and returns a bitvector
wrap(Targets, Set) ->
	SetSize = length(Set),
	Count = 64 * ((SetSize div 64) + 1),	%% make sure we're 64 bit aligned
	Bits = <<0:Count>>,
	set_sorted(Bits, Targets, Set, SetSize).


% Helper function to check if all bits in a binary are 0.
is_zero(<<0:64/little-unsigned-integer, Rest/binary>>) ->
	is_zero(Rest);
is_zero(<<>>) ->
	true;
is_zero(_) ->
	false.

bitwise_xor_int64(<<A:64/little-unsigned-integer, Rest1/binary>>, <<B:64/little-unsigned-integer, Rest2/binary>>, IntCount, Acc) when IntCount > 0 ->
	XOR = A bxor B, 
	bitwise_xor_int64(Rest1, Rest2, IntCount - 1, <<Acc/binary, XOR:64/little-unsigned-integer>>);
bitwise_xor_int64(_, _, 0, Acc) ->
	Acc.

bitwise_and_int64(<<A:64/little-unsigned-integer, Rest1/binary>>, <<B:64/little-unsigned-integer, Rest2/binary>>, IntCount, Acc) when IntCount > 0 ->
	AND = A band B,
	bitwise_and_int64(Rest1,Rest2,IntCount-1,<<Acc/binary, AND:64/little-unsigned-integer>>);
bitwise_and_int64(_,_,0,Acc) ->
	Acc.

bitwise_or_int64(<<A:64/little-unsigned-integer, Rest1/binary>>, <<B:64/little-unsigned-integer, Rest2/binary>>, IntCount, Acc) when IntCount > 0 ->
	AND = A bor B,
	bitwise_or_int64(Rest1,Rest2,IntCount-1,<<Acc/binary, AND:64/little-unsigned-integer>>);
bitwise_or_int64(_,_,0,Acc) ->
	Acc.

%% Tests to see if two bitvectors are the same, and if not returns the differences
same(Bin1, Bin2) ->
	case size(Bin1) =:= size(Bin2) of
		true ->
			IntCount = size(Bin1) div 8,
			XOR = bitwise_xor_int64(Bin1, Bin2, IntCount, <<>>),
			case is_zero(XOR) of
				true -> true;
				false -> XOR
			end;
		false ->
			{error, different_sizes}
	end.

merge(Bin1,Bin2) ->
	   case size(Bin1) =:= size(Bin2) of
		true ->
			IntCount = size(Bin1) div 8,
			bitwise_or_int64(Bin1, Bin2, IntCount, <<>>);
		false ->
			{error, different_sizes}
	end.

mask(Bin1,Bin2) ->
	   case size(Bin1) =:= size(Bin2) of
		true ->
			IntCount = size(Bin1) div 8,
			bitwise_and_int64(Bin1, Bin2, IntCount, <<>>);
		false ->
			{error, different_sizes}
	end.

%% takes a bitvector and it's map and returns the elements that were encoded
unwrap(<<1:1, Rest/bitstring>>, [H|T], Index, Acc) ->
	io:format("Found ~p~n", [ H ] ),
	unwrap(Rest, T, Index + 1, [H|Acc]);
unwrap(<<0:1, Rest/bitstring>>, [_|T], Index, Acc) ->
 	unwrap(Rest, T, Index + 1, Acc);
unwrap(_, [], _, Acc) ->
	lists:reverse(Acc);
unwrap(<<>>, _, _, Acc) ->
	lists:reverse(Acc).

unwrap(Bin,List) ->
	unwrap(Bin, List, 1, []).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

wrap_test() ->
	List = lists:sort([
		aa,ab,ac,ad,ae,af,ag,ah,ai,aj,
		ba,bb,bc,bd,be,bf,bg,bh,bi,bj,
		ca,cb,cc,cd,ce,cf,cg,ch,ci,cj,
		da,db,dc,dd,de,df,dg,dh,di,dj,
		ea,eb,ec,ed,ee,ef,eg,eh,ei,ej,
		fa,fb,fc,fd,fe,ff,fg,fh,fi,fj,
		ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,
		ha,hb,hc,hd,he,hf,hg,hh,hi,hj
	]),
	?assertEqual(<<0:1,1:1,0:1,0:1,1:1,0:59>>, wrap([borf,snarf],[bar,borf,foo,narf,snarf])),
	?assertEqual(<<0:64,0:64>>, wrap([borf,snarf], List)),
	?assertEqual(<<1:1,0:1,0:1,1:1,0:60,1:1,0:1,1:1,0:61>>,wrap([aa,ad,ge,gg],List)).

unwrap_test() ->
	List = lists:sort([
		aa,ab,ac,ad,ae,af,ag,ah,ai,aj,
		ba,bb,bc,bd,be,bf,bg,bh,bi,bj,
		ca,cb,cc,cd,ce,cf,cg,ch,ci,cj,
		da,db,dc,dd,de,df,dg,dh,di,dj,
		ea,eb,ec,ed,ee,ef,eg,eh,ei,ej,
		fa,fb,fc,fd,fe,ff,fg,fh,fi,fj,
		ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,
		ha,hb,hc,hd,he,hf,hg,hh,hi,hj
	]),
	?assertEqual([aa,ad,ge,gg], unwrap(<<1:1,0:1,0:1,1:1,0:60,1:1,0:1,1:1,0:61>>,List)).

same_test() ->
	?assertEqual(true, same(wrap([borf,snarf],[bar,borf,foo,narf,snarf]),wrap([borf,snarf],[bar,borf,foo,narf,snarf]))),
	?assertEqual(<<0:1,1:1,0:62>>, same(wrap([borf,snarf],[bar,borf,foo,narf,snarf]),wrap([snarf],[bar,borf,foo,narf,snarf]))),
	?assertEqual(<<0:1,1:1,0:1,0:1,1:1,0:59>>, same(wrap([borf,snarf],[bar,borf,foo,narf,snarf]),wrap([],[bar,borf,foo,narf,snarf]))).

merge_test() ->
	List = [bar,borf,foo,narf,snarf],
	?assertEqual([borf,foo,snarf], unwrap(merge(wrap([borf,snarf],List),wrap([foo,snarf],List)),List)).

mask_test() ->
	List = [bar,borf,foo,narf,snarf],
	?assertEqual([snarf], unwrap(mask(wrap([borf,snarf],List),wrap([foo,snarf],List)),List)).
-endif.
