


**status: experimental.**



(first attempt at an intro)
---------------------------

VoteButton provides js functions that create the dom elements that
have all the event handlers already installed.  these functions take:

1. a callback `getCredentials` that may call the backend and
   asynchronously produce the voting details signed off by the backend
   (see below).
2. a voteObject structure that describes what is voted on (just a
   colour name, for now.  possibly this is completely opaque to
   VoteButton and can be managed by the app.
3. a user id.

all the rest of the action is handled by the service, for now.

the app only needs to make a bigger effort if it wants things to
happen like vote-based sorting and searching.


Installation
------------

you need to have nodejs, npm, purescript, and pulp installed.
the following should get you going (in three different terminals).

```
1. $ cd core && make init && make run-backend
2. $ cd favcolors && make init && make run-backend
3. direct your browser to http://localhost:7000
```

see Makefile for details.  if it doesn't work out of the box, please
open a ticket.


The secure multi-party protocol (first draft)
---------------------------------------------

this is a micro-service for voting.  if you are an app vendor, you can
use this by importing html, css, js for the vote button and popups,
and very little extra.

to make this secure, your app, the user, and the vote button service
need to agree on what is been voted on by whom.  for this, your app
backend sends a cryptographic token to every occurrance of a vote
button (there may be several vote buttons for several, say, comments
that can be up- or down-voted).  this token contains:

 1. an app id;
 2. a unique id of the object voted on; and
 3. a unique user id.
 4. a validity period.
 4. a nonce guaranteeing uniqueness and fighting replay attacks.

your app is responsible for uniqueness and integrity of 2, 3.  1 is
retrieved from the vote button service via a rest api call during or
right after deployment of your app, together with a symmetric
authentication key.

the token is a signed with this symmetric key.

every time a vote is cast, the vote button sends the following items
to the vote button service:

 1. the token described above
 2. all information from the token without any crypto
 3. the vote (the payload)

1 needs to be cryptographically signed off by the backend because the
backend is responsible for deciding which objects may be voted on by
which users.  2 is there for redundancy.  if they do not match, the
vote button service will respond with an error.  this will allow you
to write your frontend code in a way that makes it as easy as possible
to prove that tokens will never end up being assigned to the wrong
objects.

the backend also needs access to the data collected by the voting
service (e.g. in order to deliver paginated content ordered by
controversiality).  since this is performance-critical, the data
should be closely taylored to the respective needs of your app.  not
much work has been done yet regarding this.
