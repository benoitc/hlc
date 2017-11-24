

# Module hlc #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

hlc.

<a name="description"></a>

## Description ##

implements the Hybrid Logical Clock outlined in
"Logical Physical Clocks and Consistent Snapshots in Globally
Distributed Databases", available online at
http://www.cse.buffalo.edu/tech-reports/2014-04.pdf.

An hybrid logical clock is available as a linked process.   Objects of this
type model causality while maintaining a relation  to physical time.
Roughly speaking, timestamps  consist of the largest wall clock time among
all  events, and a logical clock that ticks whenever  an event happens in
the future of the local physical  clock.

<a name="types"></a>

## Data Types ##




### <a name="type-clock">clock()</a> ###


<pre><code>
clock() = pid()
</code></pre>




### <a name="type-clock_fun">clock_fun()</a> ###


<pre><code>
clock_fun() = function()
</code></pre>




### <a name="type-timestamp">timestamp()</a> ###


<pre><code>
timestamp() = #timestamp{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#equal-2">equal/2</a></td><td>compare if 2 timestamps are equal.</td></tr><tr><td valign="top"><a href="#get_maxoffset-1">get_maxoffset/1</a></td><td>returns the maximal offset allowed.</td></tr><tr><td valign="top"><a href="#less-2">less/2</a></td><td>compare if one timestamps happen before the other.</td></tr><tr><td valign="top"><a href="#manual_clock-0">manual_clock/0</a></td><td>create a manually controlled physicl clock.</td></tr><tr><td valign="top"><a href="#manual_clock-1">manual_clock/1</a></td><td>create a manually controlled physicl clock and initialise it
with a default ts.</td></tr><tr><td valign="top"><a href="#now-1">now/1</a></td><td> returns a timestamp associated with an event from the local
machine that may be sent to other members of the distributed network.</td></tr><tr><td valign="top"><a href="#physical_clock-0">physical_clock/0</a></td><td>timestamp in milliseconds.</td></tr><tr><td valign="top"><a href="#set_manual_clock-2">set_manual_clock/2</a></td><td>change the value of the manually controlled physicall clock.</td></tr><tr><td valign="top"><a href="#set_maxoffset-2">set_maxoffset/2</a></td><td>Sets the maximal offset from the physical clock that a call to
Update may cause.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>start a new hybrid logical clock with physical clock and maxoffset=0.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>start a new hybrid logical clock with a custom physical clock function.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>start a new hybrid logical clock with a name.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>stop a clock.</td></tr><tr><td valign="top"><a href="#timestamp-1">timestamp/1</a></td><td>return a copy of the clock timestamp without adjusting it.</td></tr><tr><td valign="top"><a href="#update-2">update/2</a></td><td>takes a hybrid timestamp, usually originating from an event
received from another member of a distributed system.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="equal-2"></a>

### equal/2 ###

<pre><code>
equal(TS::<a href="#type-timestamp">timestamp()</a>, X2::<a href="#type-timestamp">timestamp()</a>) -&gt; true | false
</code></pre>
<br />

compare if 2 timestamps are equal

<a name="get_maxoffset-1"></a>

### get_maxoffset/1 ###

<pre><code>
get_maxoffset(Clock::<a href="#type-clock">clock()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

returns the maximal offset allowed.
A value of 0 means offset checking is disabled.

<a name="less-2"></a>

### less/2 ###

<pre><code>
less(Timestamp::<a href="#type-timestamp">timestamp()</a>, X2::<a href="#type-timestamp">timestamp()</a>) -&gt; true | false
</code></pre>
<br />

compare if one timestamps happen before the other

<a name="manual_clock-0"></a>

### manual_clock/0 ###

<pre><code>
manual_clock() -&gt; {pid(), function()}
</code></pre>
<br />

create a manually controlled physicl clock

<a name="manual_clock-1"></a>

### manual_clock/1 ###

<pre><code>
manual_clock(TS0::integer()) -&gt; {pid(), function()}
</code></pre>
<br />

create a manually controlled physicl clock and initialise it
with a default ts.

<a name="now-1"></a>

### now/1 ###

<pre><code>
now(Clock::<a href="#type-clock">clock()</a>) -&gt; {<a href="#type-timestamp">timestamp()</a>, <a href="#type-clock">clock()</a>}
</code></pre>
<br />

returns a timestamp associated with an event from the local
machine that may be sent to other members of the distributed network.
This is the counterpart of Update, which is passed a timestamp
received from another member of the distributed network.

<a name="physical_clock-0"></a>

### physical_clock/0 ###

<pre><code>
physical_clock() -&gt; non_neg_integer()
</code></pre>
<br />

timestamp in milliseconds

<a name="set_manual_clock-2"></a>

### set_manual_clock/2 ###

<pre><code>
set_manual_clock(Pid::pid(), TS::integer()) -&gt; ok
</code></pre>
<br />

change the value of the manually controlled physicall clock.

<a name="set_maxoffset-2"></a>

### set_maxoffset/2 ###

<pre><code>
set_maxoffset(Offset::non_neg_integer(), Clock::<a href="#type-clock">clock()</a>) -&gt; ok
</code></pre>
<br />

Sets the maximal offset from the physical clock that a call to
Update may cause. A well-chosen value is large enough to ignore a
reasonable amount of clock skew but will prevent ill-configured nodes
from dramatically skewing the wall time of the clock into the future.

A value of zero disables this safety feature.  The default value for
a new instance is zero.

<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; {ok, <a href="#type-clock">clock()</a>}
</code></pre>
<br />

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(ClockFun::<a href="#type-clock_fun">clock_fun()</a>, MaxOffset::non_neg_integer()) -&gt; {ok, <a href="#type-clock">clock()</a>}
</code></pre>
<br />

<a name="start-3"></a>

### start/3 ###

<pre><code>
start(Name::atom(), ClockFun::<a href="#type-clock_fun">clock_fun()</a>, MaxOffset::non_neg_integer()) -&gt; {ok, <a href="#type-clock">clock()</a>}
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, <a href="#type-clock">clock()</a>}
</code></pre>
<br />

start a new hybrid logical clock with physical clock and maxoffset=0

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(ClockFun::<a href="#type-clock_fun">clock_fun()</a>, MaxOffset::non_neg_integer()) -&gt; {ok, <a href="#type-clock">clock()</a>}
</code></pre>
<br />

start a new hybrid logical clock with a custom physical clock function.

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(Name::atom(), ClockFun::<a href="#type-clock_fun">clock_fun()</a>, MaxOffset::non_neg_integer()) -&gt; {ok, <a href="#type-clock">clock()</a>}
</code></pre>
<br />

start a new hybrid logical clock with a name. Clocks are always local

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Clock::<a href="#type-clock">clock()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

stop a clock

<a name="timestamp-1"></a>

### timestamp/1 ###

<pre><code>
timestamp(Clock::<a href="#type-clock">clock()</a>) -&gt; <a href="#type-timestamp">timestamp()</a>
</code></pre>
<br />

return a copy of the clock timestamp without adjusting it

<a name="update-2"></a>

### update/2 ###

<pre><code>
update(Clock::<a href="#type-clock">clock()</a>, RT::<a href="#type-timestamp">timestamp()</a>) -&gt; {ok, <a href="#type-timestamp">timestamp()</a>, <a href="#type-clock">clock()</a>} | {timeahead, <a href="#type-timestamp">timestamp()</a>}
</code></pre>
<br />

takes a hybrid timestamp, usually originating from an event
received from another member of a distributed system. The clock is
updated and the hybrid timestamp  associated to the receipt of the
event returned.  An error may only occur if offset checking is active
and  the remote timestamp was rejected due to clock offset,  in which
case the state of the clock will not have been  altered. To timestamp
events of local origin, use Now instead.

