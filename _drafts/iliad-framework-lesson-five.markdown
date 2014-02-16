---
layout: post
title:  "Iliad Framework, how events are implemented"
date:   2014-02-12 21:28:24
categories: programming smalltalk
---

In the [previous post][previous-post] we implemented the counter
example. Now I would like to talk about how the event's black magic
actually works from the browser point of view.

If you are curious like me you have opened the browser source view
just to see how the generated HTML looks like and you have found
something like this:

{% highlight html %}
<html lang="en" xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
<script type="text/javascript" src="/javascripts/jquery-1.4.4.min.js"> </script>
<script type="text/javascript" src="/javascripts/no_conflict.js"> </script>
<script type="text/javascript" src="/javascripts/iliad.js"> </script>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
</head>
<body>
<p>Hi! Widget example</p>
<div class="45926">
<p>0</p>
<a href="javascript:{}" onclick="iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45931&amp;_state=71db1789&quot;);">++</a>
<a href="javascript:{}" onclick="iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45932&amp;_state=71db1789&quot;);">--</a>
</div>
</body>
</html>
{% endhighlight %}

Our widget has been wrapped in a `div` tag that have, in my example,
the class `45926`. We have created two actions and these actions have
been written as two Javascript calls with two parameters:

{% highlight html %}
<a href="javascript:{}" onclick="iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45931&amp;_state=71db1789&quot;);">++</a>
<a href="javascript:{}" onclick="iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45932&amp;_state=71db1789&quot;);">--</a>
{% endhighlight %}

The parameter "action" is different because, on the server, two
different actions have to be invoked, but the "state" variable has the
same value. In effect, when we have generated the page, the state was
the same between the two links.

Now I will click on the `++` link and capture the Ajax request and the
server response for you:

    GET http://localhost:7070/leonardoBlog/widgetExample?_action=45927&_state=0261dc5b
    Accept:application/json, text/javascript, */*; q=0.01
    Accept-Encoding:gzip,deflate,sdch
    Accept-Language:en-US,en;q=0.8,it;q=0.6
    Connection:keep-alive
    Cookie:_iliad685744=587b7bac-82f8-4e95-84bc-7a39b13aa458
    Host:localhost:7070
    Referer:http://localhost:7070/leonardoBlog/widgetExample
    User-Agent:Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36
    X-Requested-With:XMLHttpRequest
 
    {
	"head": [],
	"widgets":
	    {
		  "45926":
		  "<div class=\"45926\"><p>1</p><a href=\"javascript:{}\" onclick=\"iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45934&amp;_state=0261dc5b&quot;);\">++</a><a href=\"javascript:{}\" onclick=\"iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45935&amp;_state=0261dc5b&quot;);\">--</a></div>"
	    }
	}

Now the black magic is explained by itself: the page on the browser
has made an Ajax request to the server telling him that it has to
execute the action with the code `45927` on the state `71db1789` and
the server response tell to the client that he must replace the HTML
content of the widget `45926` with the new content. The server knows
that which widgets must be redrawn using the `markDirty` method.

The meaning of the `action` and of the `class` fields should now be
clear to you.

We are missing the role of the `state` parameter.

This time I will invoke the same request using a verbose curl
invocation:

{% highlight console %}
$ curl 'http://localhost:7070/leonardoBlog/widgetExample?_action=45927&_state=0261dc5b' -H 'Cookie: _iliad685744=587b7bac-82f8-4e95-84bc-7a39b13aa458' -H 'Accept-Encoding: gzip,deflate,sdch' -H 'Host: localhost:7070' -H 'Accept-Language: en-US,en;q=0.8,it;q=0.6' -H 'User-Agent: Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36' -H 'Accept: application/json, text/javascript, */*; q=0.01' -H 'Referer: http://localhost:7070/leonardoBlog/widgetExample' -H 'X-Requested-With: XMLHttpRequest' -H 'Connection: keep-alive' --compressed --verbose
> GET /leonardoBlog/widgetExample?_action=45927&_state=0261dc5b HTTP/1.1
> Cookie: _iliad685744=587b7bac-82f8-4e95-84bc-7a39b13aa458
> Accept-Encoding: gzip,deflate,sdch
> Host: localhost:7070
> Accept-Language: en-US,en;q=0.8,it;q=0.6
> User-Agent: Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36
> Accept: application/json, text/javascript, */*; q=0.01
> Referer: http://localhost:7070/leonardoBlog/widgetExample
> X-Requested-With: XMLHttpRequest
> Connection: keep-alive
>
< HTTP/1.1 200 OK
< Date: Sun, 16 Feb 2014 22:00:00 GMT
< expires: Sun, 16 Feb 2014 23:00:00 GMT
< Server: KomHttpServer/7.1.3 (unix)
< Cache-Control: no-store, no-cache, must-revalidate
< Connection: close
< Content-type: application/json
< Content-length: 355
<
* Closing connection #0
{"head": [], "widgets": {"45926": "<div class=\"45926\"><p>2</p><a href=\"javascript:{}\" onclick=\"iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45937&amp;_state=0261dc5b&quot;);\">++</a><a href=\"javascript:{}\" onclick=\"iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45938&amp;_state=0261dc5b&quot;);\">--</a></div>"}}
{% endhighlight %}

