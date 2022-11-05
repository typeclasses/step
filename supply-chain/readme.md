# supply-chain

A "supply chain" represents a flow of information from one `Vendor` to the next,
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

### How to create a Vendor

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
