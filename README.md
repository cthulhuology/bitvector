bitvector - bit mapping terms for Erlang
========================================

This module is intended for use in quick dependency and claims checking
and any other purpose for which you may have a long list of booleans
you might want to compare.  Say for example, you have a messaging app
and you want to know what channels a user can join to:

	Grant = [ annoucements, general, rules ]
	
But the server has a list of channels like:

	Channels = [ admins, announcements, bots, general, lobby, nsfw, rules ]

We can encode the user's grants into a bitvector:

	GrantBits = bitvector:wrap( Grant, Channels ).

And we can decode the bitvector using:

	Grant = bitvector:unwrap( GrantBits, Channels ).

Now if we want to grant a new channel we can merge two bitvectors:

	NewGrantBits = bitvector:merge(bitvector:wrap([lobby],Channels),GrantBits).

We can also test channel memberships:

	GeneralMask = bitvector:wrap([general],Channels),
	[ general ] = bitvector:unwrap(bitvector:mask(GeneralMask,NewGrantBits), Channels).

We can also discover the differences between two vectors

	[lobby] = bitvector:unwrap(bitvector:same(GrantBits,NewGrantBits), Channels).

If two bitvectors are the same the same/2 function will return true.

NB: ALL LISTS SHOULD BE SORTED PRIOR TO ALL CALLS!

MIT License

Copyright (c) 2023 David J Goehrig

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
	
