# Token Bucket

Implementation of Token bucket algorithm for one bucket

### API

```

token_bucket:limit_is_reached(UserId :: term()) -> true | false.

token_bucket:limit_is_reached(UserId :: term(), MaxRPS :: non_neg_integer()) -> true | false.

```