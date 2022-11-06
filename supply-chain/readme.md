# supply-chain

A *supply chain* represents a flow of information from one `Vendor` to the next,
and so on, ultimately reaching a `Job` that returns a product.

```haskell
run (vendor1 >-> vendor2 >-> vendor3 >-> job)
```

A job or vendor can place an `order`, which is fulfilled by the vendor
*upstream* of it. In the above example:

* `vendor2` is downstream of `vendor1`; the orders made by `vendor2` are served
  by `vendor1`.
* The orders made by the `job` are served by `vendor3`.
* The orders made by `vendor3` are served by `vendor2`.
* `vendor1` does not make any requests (its upstream interface is
  `NoInterface`).

## Interfaces

An *interface* is a type constructor (kind `Type -> Type`) that describes the
requests and responses exchanged between a vendor and a job.

If a job's upstream interface is `i`, then when the job makes a request of type
`i x`, it receives a response of type `x`.

Values of a type of this kind represent requests. Each constructor will
typically have a constraint that specifies what type of response is expected in
return. Types of this kind are therefore often [GADTs]. Types of this kind are
also often not functors.

The lack of any interface at all can be expressed as `Nil`.

## Actions

An *action* is a monadic context such as `IO`.

The lack of any actions at all can be expressed as `Nil`.

## Jobs

                 ▲   │
           up x  │   │  x
                 │   ▼
    ┌─────────────────────────┐
    │  Job up action product  │
    └─────────────────────────┘

## Vendors


                 ▲   │
           up x  │   │  x
                 │   ▼
    ┌───────────────────────────┐
    │   Vendor up down action   │
    └───────────────────────────┘
                 ▲   │
         down y  │   │  y
                 │   ▼

The most common way to use a `Vendor` is to connect it to a `Job` using
`vendorToJob`.

## Connection

If `i` is the downstream interface of vendor `a` and the upstream interface of
job `b`, then we can form the composition `vendorToJob a b` (which may also
written as `a >-> b`). When the `Job` makes a request of type `i x`, the
`Vendor` replies with a response of type `x`.

    ┌────────────────────────┐
    │   Vendor up i action   │
    └────────────────────────┘
                 ▲   │
            i x  │   │  x
                 │   ▼
    ┌────────────────────────┐
    │  Job i action product  │
    └────────────────────────┘

## How to create a Vendor

We define vendors using the `Vendor` constructor. Please inspect its type
carefully:

```haskell
forall product. down product -> Job up action (Supply up down action product)
```

A vendor is a function that accepts a request. The request type is polymorphic
but constrained by the vendor's downstream interface.

```haskell
forall product. down product -> Job up action (Supply up down action product)
                ^^^^^^^^^^^^
```

A vendor has an upstream interface and can do everything a job can, therefore
the request handler operates in a `Job` context.

```haskell
forall product. down product -> Job up action (Supply up down action product)
                                ^^^^^^^^^^^^^
```

This allows the vendor to undertake a monadic sequence involving `order` and
`perform` while fulfilling the request.

The final step in fulfilling a request is to return a `Supply`.

```haskell
forall product. down product -> Job up action (Supply up down action product)
                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

A `Supply` is written using its `Supply` constructor, which has two parameters:

```haskell
Supply :: product -> Vendor up down action -> Supply up down action product
```

The first is the vendor's response to the client's request.

```haskell
Supply :: product -> Vendor up down action -> Supply up down action product
          ^^^^^^^
```

The second is a new `Vendor`.

```haskell
Supply :: product -> Vendor up down action -> Supply up down action product
                     ^^^^^^^^^^^^^^^^^^^^^
```

This latter component is what allows vendors to be stateful, and it is usually
defined recursively.

  [GADTs]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html
