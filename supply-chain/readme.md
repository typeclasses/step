The initial idea for this library stemmed from the [pipes] library. The interfaces of a pipes [Proxy] are given by four types: What the proxy sends upstream, receives from upstream, sends downstream, and receives from downstream. A value of type `Proxy a' a b' b m r` is depicted in the pipes documentation as follows:

    Upstream | Downstream
        +---------+
        |         |
    a' <==       <== b'
        |         |
    a  ==>       ==> b
        |    |    |
        +----|----+
             v
             r

A pipes server (focusing on downstream side) can accept exactly one of request (`b'`) and issue exactly one type of response (`b`). The response type cannot vary based on what type of request it is responding to.

When interfaces are described by a *type constructor* rather than a pair of types, then we can pair types of requests with appropriate responses. I first saw this approach in the [haxl] library. The [Facebook example] in haxl's documentation gives this example of what it calls a "data source API":

```haskell
data FacebookReq response =
     (response ~ Object)   => GetObject Id
   | (response ~ User)     => GetUser UserId
   | (response ~ [Friend]) => GetUserFriends UserId
```

The kind of `FacebookReq` is `Type -> Type`. The phantom type variable `response` here indicates what type of response a data source must give in response to a `FacebookReq response`. For example, the `GetUser` constructor produces a request of type `FacebookReq User`, and so the data source returns a value of type `User`.

This pattern can also be found in the [effectful] library, where a similar sort of type constructor is called a "dynamic effect". To take just one example of a dynamic effect that the library offers, its [dynamic State effect] (analogous to [StateT] of the [transformers] library) is defined as follows:

```haskell
data State s m response =
      (response ~ s)  => Get
    | (response ~ ()) => Put s
    | (response ~ a)  => State (s -> (a, s))
    | (response ~ a)  => StateM (s -> m (a, s))
```

The kind of `State s m` is, again, `Type -> Type`, and a constraint on each constructor specifies what type of corresponding response is expected.

Let us return our attention to the pipes `Proxy` type. When we attempt to alter `Proxy` to employ the type constructor interface style as discussed in the previous two examples, it becomes apparent that much of the generality and symmetry that the pipes library enjoys must be sacrificed.

todo, discuss push-based streams

todo, discuss why vendor and job are different types, unlike proxy

todo, discuss why vendors can't return

todo, discuss `>->`

todo, thank streaming-benchmark for the [Bind] constructor


A job's upstream interface and action context correspond to what the effectful library calls "dynamic effects" and "static effects" respectively. Our `vendorToJob` function corresponds roughly to what effectful describes as "reinterpeting" the job's upstream interface.

  [pipes]: https://hackage.haskell.org/package/pipes
  [Proxy]: https://github.com/Gabriella439/pipes/blob/e43acc24100dca20cdb901d91a7553143b2c1369/src/Pipes/Internal.hs#L72-L76
  [haxl]: https://hackage.haskell.org/package/haxl
  [Facebook example]: https://github.com/facebook/Haxl/blob/ef52a522fb851be8ed0a38bcd370d29310d5bba0/example/facebook/readme.md?plain=1#L35-L38
  [effectful]: https://hackage.haskell.org/package/effectful
  [dynamic State effect]: https://github.com/haskell-effectful/effectful/blob/b6cd978db35d66dbfa82ce74b0d011833411248a/effectful-core/src/Effectful/State/Dynamic.hs#L38-L42
  [transformers]: https://hackage.haskell.org/package/transformers
  [StateT]: https://hackage.haskell.org/package/transformers-0.6.0.4/docs/Control-Monad-Trans-State-Strict.html
  [Bind]: https://github.com/tomjaguarpaw/streaming-benchmark/blob/05d1adde6ddfc7b48726372649d7be3fabb37ce0/app/Streaming/Bind.hs#L9-L12
