# secretary

The knowledge of a perfect robot secretary to be used as a prosthetic brain. An
exercise in learning Common Lisp. A lot of the code is straight out of Practical
Lisp, and the rest probably isn't very good.

The knowledge is organized into "events". Things that are events:

* Kid events, like bath times, television start/stop times
* Appointments
* Deadlines
* Grocery purchases (to be parsed by recipes project)
* Trades (to be referenced by traderunner)
* Todos

## Trades

Trades should have a standard format in the description field, so parsing them
will be easier later. Time is taken care of by timestamp.

## Facts

This one is tough. Some facts should have a timestamp, and some probably don't
need one, so just setting the timestamp to "fact" would not be effective, unless
it's an immutable fact.

## List

The whole thing is saved as a list, until that becomes onerous. Switching to a
database shouldn't be that much effort, and hardly necessary until we start
getting well into the thousands of events, which should take years.
